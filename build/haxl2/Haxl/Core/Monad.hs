-- Copyright (c) 2014, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file. An additional grant of patent rights can
-- be found in the PATENTS file.

{- TODO

- write different scheduling policies
- implement dumpCacheAsHaskell

-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | The implementation of the 'Haxl' monad.
module Haxl.Core.Monad (
    -- * The monad
    GenHaxl (..), runHaxl,
    env,

    IVar(..), IVarContents(..), ResultVal(..),

    -- * Exceptions
    throw, catch, catchIf, try, tryToHaxlException,

    -- * Data fetching and caching
    dataFetch, uncachedRequest,
    cacheRequest, cacheResult, cachedComputation,
    {- dumpCacheAsHaskell, -}

    -- * Unsafe operations
    unsafeLiftIO,
  ) where

import Haxl.Core.Types hiding (trace)
import Haxl.Core.Fetch
import Haxl.Core.Env
import Haxl.Core.Exception
import Haxl.Core.RequestStore as RequestStore
import Haxl.Core.Util
import Haxl.Core.DataCache as DataCache

import qualified Data.Text as Text
import Control.Exception (Exception(..), SomeException, throwIO)
#if __GLASGOW_HASKELL__ >= 708
import Control.Exception (SomeAsyncException(..))
#endif
#if __GLASGOW_HASKELL__ >= 710
import Control.Exception (AllocationLimitExceeded(..))
#endif
import qualified Control.Exception
import Control.Applicative hiding (Const)
import GHC.Exts (IsString(..))
#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif
import Data.IORef
import Text.Printf
import Control.Arrow (left)
import Control.Concurrent.STM
import Control.Monad
import Debug.Trace

trace_ :: String -> a -> a
trace_ _ = id
--trace_ = trace

-- -----------------------------------------------------------------------------
-- | The Haxl monad, which does several things:
--
--  * It is a reader monad for 'Env' and 'IORef' 'RequestStore', The
--    latter is the current batch of unsubmitted data fetch requests.
--
--  * It is a concurrency, or resumption, monad. A computation may run
--    partially and return 'Blocked', in which case the framework should
--    perform the outstanding requests in the 'RequestStore', and then
--    resume the computation.
--
--  * The Applicative combinator '<*>' explores /both/ branches in the
--    event that the left branch is 'Blocked', so that we can collect
--    multiple requests and submit them as a batch.
--
--  * It contains IO, so that we can perform real data fetching.
--
newtype GenHaxl u a = GenHaxl
  { unHaxl :: Env u -> SchedState u -> IO (Result u a) }

data SchedState u = SchedState
  { reqStoreRef :: !(IORef (RequestStore u))
       -- ^ The set of requests that we have not submitted to data sources yet.
       -- Owned by the scheduler.
  , runQueue    :: !(IORef (JobList u))
       -- ^ Computations waiting to run.
       -- Owned by the scheduler.
  , numRequests :: !(IORef Int)
       -- ^ Number of requests that we have submitted to data sources but
       -- that have not yet completed.
       -- Owned by the scheduler.
  , completions :: !(TVar [CompleteReq u])
       -- ^ Requests that have completed.
       -- Modified by data sources (via putResult) and the scheduler.
  , pendingWaits :: [IO ()]
  }

-- | A list of 'GenHaxl' computations waiting for a value.  When we
-- block on a resource, a 'ContVar' is created to track the blocked
-- computations.  If another computation becomes blocked on the same
-- resource, it will be added to the list in this 'IORef'.
type ContVar u a = IORef (JobList u) -- Owned by the scheduler
  -- morally this list contains @a -> GenHaxl u ()@, but instead of
  -- using a function, each computation begins with `getIVar` to grab
  -- the value it is waiting for.  This is less type safe but a little
  -- faster (benchmarked with tests/MonadBench.hs).

-- list with O(1) cons/uncons, usually O(1) append
data JobList u
 = JobNil
 | forall a . JobCons (GenHaxl u a) !(IVar u a) (JobList u)
 | JobAppend (JobList u) (JobList u) -- invariant: left is JobCons

appendJobList :: JobList u -> JobList u -> JobList u
appendJobList JobNil c = c
appendJobList c JobNil = c
appendJobList (JobAppend a b) c = JobAppend a (appendJobList b c)
appendJobList a b = JobAppend a b

-- | A synchronisation point.  It either contains the value, or a list
-- of computations waiting for the value.
newtype IVar u a = IVar (IORef (IVarContents u a))

data IVarContents u a
  = IVarFull (ResultVal a)
  | IVarEmpty !(ContVar u a)

-- | A completed request from a data source, containing the result,
-- and the 'IVar' representing the blocked computations.  The job of a
-- data source is just to add these to a queue (completions) using
-- putResult; the scheduler collects them from the queue and unblocks
-- the relevant computations.
data CompleteReq u =
  forall a . CompleteReq (Either SomeException a)
                         !(IVar u a)  -- IVar because the result is cached

data ResultVal a
  = Ok a
  | ThrowHaxl SomeException
  | ThrowIO SomeException

done :: ResultVal a -> IO (Result u a)
done (Ok a) = return (Done a)
done (ThrowHaxl e) = return (Throw e)
done (ThrowIO e) = throwIO e

eitherToResultThrowIO :: Either SomeException a -> ResultVal a
eitherToResultThrowIO (Right a) = Ok a
eitherToResultThrowIO (Left e)
  | Just HaxlException{} <- fromException e = ThrowHaxl e
  | otherwise = ThrowIO e

eitherToResult :: Either SomeException a -> ResultVal a
eitherToResult (Right a) = Ok a
eitherToResult (Left e) = ThrowHaxl e

doneEither :: Either SomeException a -> IO (Result u a)
doneEither = done . eitherToResult

-- | The result of a computation is either 'Done' with a value, 'Throw'
-- with an exception, or 'Blocked' on the result of a data fetch with
-- a continuation.
data Result u a
  = Done a
  | Throw SomeException
  | forall b . Blocked
      !(ContVar u b)      -- ^ What we are blocked on
      (Cont u a)
         -- ^ The continuation.  This might be wrapped further if
         -- we're nested inside multiple '>>=', before finally being
         -- added to the 'ContVar'.  Morally @b -> GenHaxl u a@, but see
         -- 'ContVar',

data Cont u a
  = ContGetIVar (IVar u a)
  | forall b . ContFmap (b -> a) (Cont u b)
  | forall b . ContBind (Cont u b) (b -> GenHaxl u a)
  | forall e . Exception e => ContCatch (Cont u a) (e -> GenHaxl u a)

runCont :: Cont u a -> GenHaxl u a
runCont (ContGetIVar ivar) = getIVar ivar return
runCont (ContBind (ContBind c k1) k2) = runCont (c `ContBind` (k1 >=> k2))
runCont (ContBind (ContFmap f c) k) = runCont (c `ContBind` (\x -> k (f x)))
runCont (ContBind (ContGetIVar ivar) k) = getIVar ivar k
runCont (ContBind c k) = runCont c >>= k
runCont (ContCatch c h) = catch (runCont c) h
runCont (ContFmap f c) = fmap f (runCont c)

-- haxl exceptions:
--   - want to be able to discard on the right of <*>
-- IO exceptions / error / AllocationLimitExceeded
--   - must not be discarded on the right of <*>
-- If we use throwIO for Haxl exceptions, then
--   - have to catch when running the right of <*>,
--     and distinguish between Haxl exceptions and other exceptions
-- If we use Throw for Haxl exceptions,
--   - don't need to do try when running the right of <*>
--   - but we need to distinguish between Throw and throwIO in the
--     contents of an IVar
--   - putIVar needs to be built-in to ContVar
--
-- ToDo: check how all this interacts with memoization

instance (Show a) => Show (Result u a) where
  show (Done a) = printf "Done(%s)" $ show a
  show (Throw e) = printf "Throw(%s)" $ show e
  show Blocked{} = "Blocked"

instance Monad (GenHaxl u) where
  return a = GenHaxl $ \_env _ref -> return (Done a)
  GenHaxl m >>= k = GenHaxl $ \env ref -> do
    e <- m env ref
    case e of
      Done a         -> unHaxl (k a) env ref
      Throw e        -> return (Throw e)
      Blocked cvar f -> trace_ ">>= Blocked" $
        return (Blocked cvar (ContBind f k))

instance Functor (GenHaxl u) where
  fmap f (GenHaxl m) = GenHaxl $ \env ref -> do
    r <- m env ref
    case r of
     Done a -> return (Done (f a))
     Throw e -> return (Throw e)
     Blocked cvar cont -> trace_ "fmap Blocked" $
       return (Blocked cvar (ContFmap f cont))

instance Applicative (GenHaxl u) where
  pure = return
  GenHaxl ff <*> GenHaxl aa = GenHaxl $ \env ref -> do
    rf <- ff env ref
    case rf of
      Done f -> do
        ra <- aa env ref
        case ra of
          Done a -> trace_ "Done/Done" $ return (Done (f a))
          Throw e -> trace_ "Done/Throe" $ return (Throw e)
          Blocked cvar fcont -> trace_ "Done/Blocked" $
            return (Blocked cvar (ContFmap f fcont))
      Throw e -> trace_ "Throw" $ return (Throw e)
      Blocked cvar1 fcont -> do
         ra <- aa env ref
         case ra of
           Done a -> trace_ "Blocked/Done" $
             return (Blocked cvar1 (ContFmap ($ a) fcont))
           Throw e -> trace_ "Blocked/Throw" $
             return (Blocked cvar1 (ContBind fcont (\_ -> throw e)))
           Blocked cvar2 acont -> trace_ "Blocked/Blocked" $ do
             -- Note [Blocked/Blocked]
              i <- newIVar
              modifyIORef' cvar2 ((JobCons $! runCont acont) i)
              let cont =  fcont `ContBind` \f -> getIVar i (\a -> return (f a))
              return (Blocked cvar1 cont)




-- Note [Blocked/Blocked]
--
-- This is the tricky case: we're blocked on both sides of the <*>.
-- We need to divide the computation into two pieces that may continue
-- independently when the resources they are blocked on become
-- available.  Moreover, the computation as a whole depends on the two
-- pieces.  It works like this:
--
--   ff <*> aa
--
-- becomes
--
--   (ff >>= putIVar i) <*> (a <- aa; f <- getIVar i; return (f a)
--
-- where the IVar i is a new synchronisation point.  If the left side
-- gets to the `getIVar` first, it will block until the right side has
-- called 'putIVar'.
--
-- We can also do it the other way around:
--
--   (do ff <- f; getIVar i; return (ff a)) <*> (a >>= putIVar i)
--
-- The first was slightly faster according to tests/MonadBench.hs.

getIVar :: IVar u a -> (a -> GenHaxl u b) -> GenHaxl u b
getIVar (IVar !ref) cont = GenHaxl $ \env sref -> do
  e <- readIORef ref
  case e of
    IVarFull (Ok a) -> case cont a of GenHaxl fn -> fn env sref
    IVarFull (ThrowHaxl e) -> return (Throw e)
    IVarFull (ThrowIO e) -> throwIO e
    IVarEmpty cref -> return (Blocked cref (ContGetIVar (IVar ref) `ContBind` cont))

newIVar :: IO (IVar u a)
newIVar = do
  cref <- newIORef JobNil
  ref <- newIORef (IVarEmpty cref)
  return (IVar ref)


data SchedPolicy
  = SubmitImmediately
  | WaitAtLeast Int{-ms-}
  | WaitForAllPendingRequests

-- | Runs a 'Haxl' computation in an 'Env'.
runHaxl :: forall u a. Env u -> GenHaxl u a -> IO a
runHaxl env haxl = do
  q <- newIORef JobNil          -- run queue
  cv <- newContVar
  resultRef <- newIORef (IVarEmpty cv) -- where to put the final result
  rs <- newIORef noRequests          -- RequestStore
  nr <- newIORef 0                   -- numRequests
  comps <- newTVarIO []              -- completion queue
  let st = SchedState
             { reqStoreRef = rs
             , runQueue = q
             , numRequests = nr
             , completions = comps
             , pendingWaits = [] }
  schedule st haxl (IVar resultRef)
  r <- readIORef resultRef
  case r of
    IVarEmpty _ -> throwIO (CriticalError "runHaxl: missing result")
    IVarFull (Ok a)  -> return a
    IVarFull (ThrowIO e)  -> throwIO e
    IVarFull (ThrowHaxl e)  -> throwIO e
 where
  schedule :: SchedState u -> GenHaxl u b -> IVar u b -> IO ()
  schedule q (GenHaxl run) (IVar !ref) = do
    r <- Control.Exception.try $ run env q
    case r of
      Left ex -> do
        e <- readIORef ref
        case e of
         IVarFull _ -> error "multiple put"
         IVarEmpty cref -> do
           writeIORef ref (IVarFull (ThrowIO ex))
           haxls <- readIORef cref
           modifyIORef' (runQueue q) (appendJobList haxls)
           reschedule q
      Right (Done a) -> do
        e <- readIORef ref
        case e of
         IVarFull _ -> error "multiple put"
         IVarEmpty cref -> do
           writeIORef ref (IVarFull (Ok a))
           haxls <- readIORef cref
           modifyIORef' (runQueue q) (appendJobList haxls)
           reschedule q
      Right (Throw ex) -> do
        e <- readIORef ref
        case e of
         IVarFull _ -> error "multiple put"
         IVarEmpty cref -> do
           writeIORef ref (IVarFull (ThrowHaxl ex))
           haxls <- readIORef cref
           modifyIORef' (runQueue q) (appendJobList haxls)
           reschedule q
      Right (Blocked cref fn) -> do
        modifyIORef' cref ((JobCons $! runCont fn) (IVar ref))
        reschedule q

  reschedule :: SchedState u -> IO ()
  reschedule q@SchedState{..} = do
--    printf "reschedule\n"
    haxls <- readIORef runQueue
    case haxls of
      JobNil -> emptyRunQueue q
      JobAppend (JobCons a b c) d -> do
--       printf "run queue: %d\n" (length (h:hs))
       writeIORef runQueue (appendJobList c d)
       schedule q a b
      JobCons a b c -> do
--       printf "run queue: %d\n" (length (h:hs))
       writeIORef runQueue c
       schedule q a b

  -- Here we have a choice:
  --   - for latency: submit requests as soon as we have them
  --   - for batching: wait until all outstanding requests have finished
  --     before submitting the next batch.  We can still begin running
  --     as soon as we have results.
  --   - compromise: wait at least Nms for an outstanding result
  --     before giving up and submitting new requests.
  emptyRunQueue :: SchedState u -> IO ()
  emptyRunQueue q@SchedState{..} = do
--    printf "emptyRunQueue\n"
    any_done <- checkCompletions q
    if any_done
      then reschedule q
      else do
        case pendingWaits of
          [] -> checkRequestStore q
          wait:waits -> do
--            printf "invoking wait\n"
            wait
            emptyRunQueue q { pendingWaits = waits } -- check completions

  checkRequestStore :: SchedState u -> IO ()
  checkRequestStore q@SchedState{..} = do
    reqStore <- readIORef reqStoreRef
    if RequestStore.isEmpty reqStore
      then waitCompletions q
      else do
        writeIORef reqStoreRef noRequests
        waits <- performFetches env reqStore -- latency optimised
--        printf "performFetches: %d waits\n" (length waits)
        emptyRunQueue q{ pendingWaits = waits ++ pendingWaits }

  checkCompletions :: SchedState u -> IO Bool
  checkCompletions q@SchedState{..} = do
--    printf "checkCompletions\n"
    comps <- atomically $ do
      c <- readTVar completions
      writeTVar completions []
      return c
    case comps of
      [] -> return False
      _ -> do
--        printf "%d complete\n" (length comps)
        let getComplete (CompleteReq a (IVar cr)) = do
              r <- readIORef cr
              case r of
                IVarFull _ -> do
--                  printf "existing result\n"
                  return JobNil
                  -- this happens if a data source reports a result,
                  -- and then throws an exception.  We call putResult
                  -- a second time for the exception, which comes
                  -- ahead of the original request (because it is
                  -- pushed on the front of the completions list) and
                  -- therefore overrides it.
                IVarEmpty cv -> do
                  writeIORef cr (IVarFull (eitherToResult a))
                  modifyIORef' numRequests (subtract 1)
                  readIORef cv
        jobs <- mapM getComplete comps
        modifyIORef' runQueue (\c -> foldr appendJobList c jobs)
        return True

  waitCompletions :: SchedState u -> IO ()
  waitCompletions q@SchedState{..} = do
    n <- readIORef numRequests
    if n == 0
       then return ()
       else do
         atomically $ do
           c <- readTVar completions
           when (null c) retry
         emptyRunQueue q


-- | Extracts data from the 'Env'.
env :: (Env u -> a) -> GenHaxl u a
env f = GenHaxl $ \env _ref -> return (Done (f env))


-- -----------------------------------------------------------------------------
-- Cache management

-- | Possible responses when checking the cache.
data CacheResult u a
  -- | The request hadn't been seen until now.
  = Uncached
       (ResultVar a)
       !(ContVar u (ResultVal a))
       !(IVar u a)

  -- | The request has been seen before, but its result has not yet been
  -- fetched.
  | CachedNotFetched
      !(ContVar u (ResultVal a))
      !(IVar u a)

  -- | The request has been seen before, and its result has already been
  -- fetched.
  | Cached (ResultVal a)


-- | Checks the data cache for the result of a request.
cached :: (Request r a) => Env u -> SchedState u -> r a
       -> IO (CacheResult u a)
cached env st = checkCache (flags env) st (cacheRef env)

-- | Checks the memo cache for the result of a computation.
memoized :: (Request r a) => Env u -> SchedState u -> r a
         -> IO (CacheResult u a)
memoized env st = checkCache (flags env) st (memoRef env)

-- | Common guts of 'cached' and 'memoized'.
checkCache
  :: (Request r a)
  => Flags
  -> SchedState u
  -> IORef (DataCache u)
  -> r a
  -> IO (CacheResult u a)

checkCache flags SchedState{..} ref req = do
  cache <- readIORef ref
  let
    do_fetch = do
      cvar <- newContVar
      cr <- IVar <$> newIORef (IVarEmpty cvar)
      let done r = atomically $ do
            cs <- readTVar completions
            writeTVar completions (CompleteReq r cr : cs)
      rvar <- newEmptyResult done
      writeIORef ref $! DataCache.insert req cr cache
      modifyIORef' numRequests (+1)
      return (Uncached rvar cvar cr)
  case DataCache.lookup req cache of
    Nothing -> do_fetch
    Just (IVar cr) -> do
      e <- readIORef cr
      case e of
        IVarEmpty cvar -> return (CachedNotFetched cvar (IVar cr))
        IVarFull r -> do
          ifTrace flags 3 $ putStrLn $ case r of
            ThrowIO _ -> "Cached error: " ++ show req
            ThrowHaxl _ -> "Cached error: " ++ show req
            Ok _ -> "Cached request: " ++ show req
          return (Cached r)

newContVar :: IO (ContVar u a)
newContVar = newIORef JobNil

-- -----------------------------------------------------------------------------
-- Exceptions

-- | Throw an exception in the Haxl monad
throw :: (Exception e) => e -> GenHaxl u a
throw e = GenHaxl $ \_env _ref -> raise e

raise :: (Exception e) => e -> IO (Result u a)
raise = return . Throw . toException

-- | Catch an exception in the Haxl monad
catch :: Exception e => GenHaxl u a -> (e -> GenHaxl u a) -> GenHaxl u a
catch (GenHaxl m) h = GenHaxl $ \env ref -> do
   e <- m env ref
   case e of
     Done a -> return (Done a)
     Throw e | Just e' <- fromException e -> unHaxl (h e') env ref
             | otherwise -> return (Throw e)
     Blocked cvar k -> return (Blocked cvar (ContCatch k h))

-- | Catch exceptions that satisfy a predicate
catchIf
  :: Exception e => (e -> Bool) -> GenHaxl u a -> (e -> GenHaxl u a)
  -> GenHaxl u a
catchIf cond haxl handler =
  catch haxl $ \e -> if cond e then handler e else throw e

-- | Returns @'Left' e@ if the computation throws an exception @e@, or
-- @'Right' a@ if it returns a result @a@.
try :: Exception e => GenHaxl u a -> GenHaxl u (Either e a)
try haxl = (Right <$> haxl) `catch` (return . Left)

-- -----------------------------------------------------------------------------
-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Haxl
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenHaxl u a
unsafeLiftIO m = GenHaxl $ \_env _ref -> Done <$> m

-- | Like 'try', but lifts all exceptions into the 'HaxlException'
-- hierarchy.  Uses 'unsafeToHaxlException' internally.  Typically
-- this is used at the top level of a Haxl computation, to ensure that
-- all exceptions are caught.
tryToHaxlException :: GenHaxl u a -> GenHaxl u (Either HaxlException a)
tryToHaxlException h = left asHaxlException <$> try h

-- -----------------------------------------------------------------------------
-- Data fetching and caching

-- | Performs actual fetching of data for a 'Request' from a 'DataSource'.
dataFetch :: (DataSource u r, Request r a) => r a -> GenHaxl u a
dataFetch req = GenHaxl $ \env st@SchedState{..} -> do
  -- First, check the cache
  res <- cached env st req
  case res of
    -- Not seen before: add the request to the RequestStore, so it
    -- will be fetched in the next round.
    Uncached rvar cvar ivar -> do
      modifyIORef' reqStoreRef $ \bs -> addRequest (BlockedFetch req rvar) bs
      return $ Blocked cvar (ContGetIVar ivar)

    -- Seen before but not fetched yet.  We're blocked, but we don't have
    -- to add the request to the RequestStore.
    CachedNotFetched cvar ivar -> return $ Blocked cvar (ContGetIVar ivar)

    -- Cached: either a result, or an exception
    Cached r -> done r


-- | A data request that is not cached.  This is not what you want for
-- normal read requests, because then multiple identical requests may
-- return different results, and this invalidates some of the
-- properties that we expect Haxl computations to respect: that data
-- fetches can be aribtrarily reordered, and identical requests can be
-- commoned up, for example.
--
-- 'uncachedRequest' is useful for performing writes, provided those
-- are done in a safe way - that is, not mixed with reads that might
-- conflict in the same Haxl computation.
--
uncachedRequest :: (DataSource u r, Request r a) => r a -> GenHaxl u a
uncachedRequest req = GenHaxl $ \_env SchedState{..} -> do
  cvar <- newContVar
  cr <- IVar <$> newIORef (IVarEmpty cvar)
  let done r = atomically $ do
        cs <- readTVar completions
        writeTVar completions (CompleteReq r cr : cs)
  rvar <- newEmptyResult done
  modifyIORef' numRequests (+1)
  return $ Blocked cvar (ContGetIVar cr)

-- | Transparently provides caching. Useful for datasources that can
-- return immediately, but also caches values.
cacheResult :: (Request r a)  => r a -> IO a -> GenHaxl u a
cacheResult req val = GenHaxl $ \env st -> do
  cachedResult <- cached env st req
  case cachedResult of
    Uncached rvar cvar _ivar -> do
      result <- Control.Exception.try val
      putResult rvar result
      -- important to re-throw IO exceptions here
      done (eitherToResultThrowIO result)
    Cached result -> done result
    CachedNotFetched _ _ -> corruptCache
  where
    corruptCache = raise . DataSourceError $ Text.concat
      [ textShow req
      , " has a corrupted cache value: these requests are meant to"
      , " return immediately without an intermediate value. Either"
      , " the cache was updated incorrectly, or you're calling"
      , " cacheResult on a query that involves a blocking fetch."
      ]

-- We must be careful about turning IO monad exceptions into Haxl
-- exceptions.  An IO monad exception will normally propagate right
-- out of runHaxl and terminate the whole computation, whereas a Haxl
-- exception can get dropped on the floor, if it is on the right of
-- <*> and the left side also throws, for example.  So turning an IO
-- monad exception into a Haxl exception is a dangerous thing to do.
-- In particular, we never want to do it for an asynchronous exception
-- (AllocationLimitExceeded, ThreadKilled, etc.), because these are
-- supposed to unconditionally terminate the computation.
--
-- There are three places where we take an arbitrary IO monad exception and
-- turn it into a Haxl exception:
--
--  * wrapFetchInCatch.  Here we want to propagate a failure of the
--    data source to the callers of the data source, but if the
--    failure came from elsewhere (an asynchronous exception), then we
--    should just propagate it
--
--  * cacheResult (cache the results of IO operations): again,
--    failures of the IO operation should be visible to the caller as
--    a Haxl exception, but we exclude asynchronous exceptions from
--    this.
--
--  * unsafeToHaxlException: assume the caller knows what they're
--    doing, and just wrap all exceptions.
--

-- | Inserts a request/result pair into the cache. Throws an exception
-- if the request has already been issued, either via 'dataFetch' or
-- 'cacheRequest'.
--
-- This can be used to pre-populate the cache when running tests, to
-- avoid going to the actual data source and ensure that results are
-- deterministic.
--
cacheRequest
  :: (Request req a) => req a -> Either SomeException a -> GenHaxl u ()
cacheRequest request result = GenHaxl $ \env _st -> do
  cache <- readIORef (cacheRef env)
  case DataCache.lookup request cache of
    Nothing -> do
      cr <- IVar <$> newIORef (IVarFull (eitherToResult result))
      writeIORef (cacheRef env) $! DataCache.insert request cr cache
      return (Done ())

    -- It is an error if the request is already in the cache.  We can't test
    -- whether the cached result is the same without adding an Eq constraint,
    -- and we don't necessarily have Eq for all results.
    _other -> raise $
      DataSourceError "cacheRequest: request is already in the cache"

instance IsString a => IsString (GenHaxl u a) where
  fromString s = return (fromString s)

-- | 'cachedComputation' memoizes a Haxl computation.  The key is a
-- request.
--
-- /Note:/ These cached computations will /not/ be included in the output
-- of 'dumpCacheAsHaskell'.
--
cachedComputation
   :: forall req u a. (Request req a)
   => req a -> GenHaxl u a -> GenHaxl u a
cachedComputation req haxl = GenHaxl $ \env ref -> do
  res <- memoized env ref req
  case res of
    -- Uncached: we must compute the result and store it in the ResultVar.
    Uncached rvar cvar _ivar -> do
      let
          with_result :: Either SomeException a -> GenHaxl u a
          with_result r = GenHaxl $ \_ _ -> do putResult rvar r; done (eitherToResult r)

      unHaxl (try haxl >>= with_result) env ref

    -- CachedNotFetched: this request is already being computed, we just
    -- have to block until the result is available.  Note that we might
    -- have to block repeatedly, because the Haxl computation might block
    -- multiple times before it has a result.
    CachedNotFetched cvar ivar -> return $ Blocked cvar (ContGetIVar ivar)
    Cached r -> done r

{-
-- | Dump the contents of the cache as Haskell code that, when
-- compiled and run, will recreate the same cache contents.  For
-- example, the generated code looks something like this:
--
-- > loadCache :: GenHaxl u ()
-- > loadCache = do
-- >   cacheRequest (ListWombats 3) (Right ([1,2,3]))
-- >   cacheRequest (CountAardvarks "abcabc") (Right (2))
--
dumpCacheAsHaskell :: GenHaxl u String
dumpCacheAsHaskell = do
  ref <- env cacheRef  -- NB. cacheRef, not memoRef.  We ignore memoized
                       -- results when dumping the cache.
  entries <- unsafeLiftIO $ readIORef ref >>= showCache
  let
    mk_cr (req, res) =
      text "cacheRequest" <+> parens (text req) <+> parens (result res)
    result (Left e) = text "except" <+> parens (text (show e))
    result (Right s) = text "Right" <+> parens (text s)

  return $ show $
    text "loadCache :: GenHaxl u ()" $$
    text "loadCache = do" $$
      nest 2 (vcat (map mk_cr (concatMap snd entries))) $$
    text "" -- final newline


-}
