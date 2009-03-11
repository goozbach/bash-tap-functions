{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TAP (
    planTests, planNoPlan, planSkipAll,
    is, isnt, like, unlike, pass, fail, ok,
    skip, skipIf, toDo, 
    diag, bailOut, runTests
    ) where


import Prelude hiding (fail)
import System.IO
import System.Exit
import Control.Monad.State hiding (fail)
import Control.Exception
import Text.Regex.Posix


data TAPState = TAPState {
  planSet :: Bool,
  noPlan :: Bool,
  skipAll :: Bool,
  testDied :: Bool,
  expectedTests :: Int,
  executedTests :: Int,
  failedTests :: Int,
  toDoReason :: Maybe String,
  exitCode :: Int
} deriving (Show)

initState = TAPState {
  planSet = False,
  noPlan = False,
  skipAll = False,
  testDied = False,
  expectedTests = 0,
  executedTests = 0,
  failedTests = 0,
  toDoReason = Nothing,
  exitCode = 0 
}


newtype TAP a = TAP {
        runTAP :: StateT TAPState IO a
    } deriving (Monad, MonadIO, MonadState TAPState)


_assertNotPlanned :: TAP ()
_assertNotPlanned = do
    ts <- get
    when (planSet ts) $ _die "You tried to plan twice!"


_assertPlanned :: TAP ()
_assertPlanned = do
    ts <- get
    when (not $ planSet ts) $ _die $ "You tried to run a test without a plan!"
        ++ "  Gotta have a plan."


_printPlan :: Int -> Maybe String -> IO ()
_printPlan n plan = do
    putStrLn $ "1.." ++ show n ++
        case plan of
           Just plan -> " # " ++ plan
           otherwise -> ""


planTests :: Int -> TAP ()
planTests n = do 
    _assertNotPlanned
    when (n == 0) $ _die $ "You said to run 0 tests!"
        ++ " You've got to run something."
    liftIO $ _printPlan n Nothing
    modify (\x -> x {planSet = True, expectedTests = n})


planNoPlan :: TAP ()
planNoPlan = do 
    _assertNotPlanned
    modify (\x -> x {planSet = True, noPlan = True})


planSkipAll :: String -> TAP ()
planSkipAll plan = do 
    _assertNotPlanned
    liftIO . _printPlan 0 . Just $ "Skip " ++ plan
    modify (\x -> x {planSet = True, skipAll = True})
    _exit $ Just 0
    return ()



_matches :: String -> String -> Bool
_matches "" _ = False
_matches _ "" = False
_matches target pattern = target =~ pattern :: Bool


ok :: Bool -> Maybe String -> TAP Bool
ok result msg = do
    _assertPlanned
    modify (\x -> x {executedTests = executedTests x + 1})

    case msg of
        Just s -> when (_matches s "^[0-9]+$") $ do
            diag $ "    You named your test '" ++ s 
                ++ "'.  You shouldn't use numbers for your test names."
            diag $ "    Very confusing."
        otherwise -> return ()

    when (not result) $ do
        liftIO $ putStr "not "
        modify (\x -> x {failedTests = failedTests x + 1})

    ts <- get
    liftIO . putStr $ "ok " ++ (show $ executedTests ts)

    case msg of
        -- TODO: Escape s
        Just s -> liftIO . putStr $ " - " ++ s
        otherwise -> return ()

    case (toDoReason ts) of
        Just r -> liftIO . putStr $ " # TODO " ++ r
        otherwise -> return ()

    when (not result) $ do
        modify (\x -> x {failedTests = failedTests x - 1})
 
    liftIO $ putStrLn ""

    -- TODO: STACK TRACE?

    return result


is :: (Show a, Eq a) => a -> a -> Maybe String -> TAP Bool
is result expected msg = do
    rc <- ok (result == expected) msg
    when (not rc) $ do
        diag $ "           got: '" ++ (show result) ++ "'"
        diag $ "      expected: '" ++ (show expected) ++ "'"
    return rc


isnt :: (Show a, Eq a) => a -> a -> Maybe String -> TAP Bool
isnt result expected msg = do
    rc <- ok (result /= expected) msg
    when (not rc) $ do
        diag $ "           got: '" ++ (show result) ++ "'"
        diag $ " didn't expect: '" ++ (show expected) ++ "'"
    return rc


like :: String -> String -> Maybe String -> TAP Bool
like target pattern msg = do
    rc <- ok (_matches target pattern) msg
    when (not rc) $ diag $ "    '" ++ target ++ "' doesn't match '" 
        ++ pattern ++ "'"
    return rc


unlike :: String -> String -> Maybe String -> TAP Bool
unlike target pattern msg = do
    rc <- ok (not $ _matches target pattern) msg
    when (not rc) $ diag $ "    '" ++ target ++ "' matches '" 
        ++ pattern ++ "'"
    return rc


pass :: Maybe String -> TAP Bool
pass s = ok True s


fail :: Maybe String -> TAP Bool
fail s = ok False s



skip :: Int -> String -> TAP ()
skip n reason = do
    forM_ [1 .. n] (\n' -> do
        modify (\x -> x {executedTests = executedTests x + 1})
        ts <- get
        liftIO . putStrLn $ "ok " ++ (show $ executedTests ts) 
            ++ " # skip: " ++ reason)
    return ()


skipIf :: Bool -> Int -> String -> TAP a -> TAP ()
skipIf cond n reason tap = do
    if cond
        then skip n reason
        else do
            tap
            return ()


toDo :: String -> TAP a -> TAP ()
toDo reason tap = do
    modify (\x -> x {toDoReason = Just reason})
    a <- tap
    modify (\x -> x {toDoReason = Nothing})
    return ()


diag :: String -> TAP ()
diag s = do
    liftIO . putStrLn $ "# " ++ s


bailOut :: String -> TAP a
bailOut s = do
    liftIO $ hPutStrLn stderr s
    _exit $ Just 255


_die :: String -> TAP a
_die s = do 
    liftIO $ hPutStrLn stderr s
    modify (\x -> x {testDied = True})
    _exit $ Just 255


_wrapup :: TAP ()
_wrapup = do
    ts <- get
    let s n = if (n > 1) then "s" else ""
    let err | not $ planSet ts = diag "Looks like your test died before it could output anything." >> return True
            | testDied ts      = diag ("Looks like your test died just after " ++ (show $ executedTests ts)) >> return True
            | otherwise        = return False
    stop <- err
    if stop 
        then return () 
        else do
            when ((not $ noPlan ts)&&((expectedTests ts) < (executedTests ts))) $ do
                let extra = (executedTests ts) - (expectedTests ts)
                diag $ "Looks like you planned " ++ (show $ expectedTests ts) 
                    ++ " test" ++ (s $ expectedTests ts)
                    ++ " but ran " ++ (show extra) ++ " extra."
                modify (\x -> x {exitCode = -1})

            when ((not $ noPlan ts)&&((expectedTests ts) > (executedTests ts))) $ do
                diag $ "Looks like you planned " ++ (show $ expectedTests ts) 
                    ++ " test" ++ (s $ expectedTests ts) 
                    ++ " but only ran " ++ (show $ executedTests ts)

            when (failedTests ts > 0) $ do
                diag $ "Looks like you failed " ++ (show $ failedTests ts) 
                    ++ " test" ++ (s $ failedTests ts) 
                    ++ " of " ++ (show $ executedTests ts)


_exit :: Maybe Int -> TAP a
_exit mrc = do
    case mrc of
        Just rc -> modify (\x -> x {exitCode = rc})
        otherwise -> return ()
    ts <- get
    when (exitCode ts == 0) $ do
        rc <- if ((noPlan ts)||(not $ planSet ts)) 
                  then return $ failedTests ts
                  else if ((expectedTests ts) < (executedTests ts))
                      then return $ (executedTests ts) - (expectedTests ts)
                      else return $ ((failedTests ts) 
                          + ((expectedTests ts) - (executedTests ts)))
        modify (\x -> x {exitCode = rc})

    _wrapup
    ts <- get
    let rc = exitCode ts
    liftIO . exitWith $ if (rc == 0) then ExitSuccess else ExitFailure rc


runTests :: TAP a -> IO (a, TAPState)
-- TODO: Add exception handling here?
runTests s = runStateT (runTAP (s >> _exit Nothing)) initState
