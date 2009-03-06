module TAP (
    planTests, planNoPlan, planSkipAll,
    runTests, is, isnt, like, unlike, pass, fail, ok,
    skip, skipUnless, toDo, 
    diag, bailOut
    ) where


import System.IO
import System.Exit
import Control.Monad.State
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
  exitCode = 0 
}

type TAP a = StateT TAPState IO a


planTests :: Int -> Maybe String -> TAP Int
planTests n s = do 
    _assertNotPlanned
    when (n == 0) $ _die "You said to run 0 tests!  You've got to run something."
    lift $ _printPlan n s 
    modify (\x -> x {planSet = True, expectedTests = n})
    return n


planNoPlan :: TAP Int
planNoPlan = do 
    _assertNotPlanned
    modify (\x -> x {planSet = True, noPlan = True})
    return 0


planSkipAll :: Maybe String -> TAP Int
planSkipAll s = do 
    _assertNotPlanned
    lift . _printPlan 0 . Just $ "Skip " ++ 
        case s of
            Just s -> s
            otherwise -> ""
    modify (\x -> x {planSet = True, skipAll = True})
    _exit $ Just 0


_assertNotPlanned :: TAP ()
_assertNotPlanned = do
    ts <- get
    when (planSet ts) $ _die "You tried to plan twice!"


_assertPlanned :: TAP ()
_assertPlanned = do
    ts <- get
    when (not $ planSet ts) $ _die "You tried to run a test without a plan!  Gotta have a plan."


_printPlan :: Int -> Maybe String -> IO Int
_printPlan n s = do
    putStrLn $ "1.." ++ show n ++
        case s of
           Just s -> " # " ++ s
           otherwise -> ""
    return n


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
    when (not rc) $ diag $ "    '" ++ target ++ "' doesn't match '" ++ pattern ++ "'"
    return rc


unlike :: String -> String -> Maybe String -> TAP Bool
unlike target pattern msg = do
    rc <- ok (not $ _matches target pattern) msg
    when (not rc) $ diag $ "    '" ++ target ++ "' matches '" ++ pattern ++ "'"
    return rc


pass :: Maybe String -> TAP Bool
pass s = ok True s


fail :: Maybe String -> TAP Bool
fail s = ok False s


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
        lift $ putStr "not "
        modify (\x -> x {failedTests = failedTests x + 1})

    ts <- get
    lift . putStr $ "ok " ++ (show $ executedTests ts)

    case msg of
        Just s -> lift . putStr $ " - " ++ s
        otherwise -> return ()
  
    -- TODO
 
    lift $ putStrLn ""

    -- STACK TRACE

    return result


_matches :: String -> String -> Bool
_matches "" _ = False
_matches _ "" = False
_matches target pattern = target =~ pattern :: Bool


_is_diag :: (Show a) => a -> a -> TAP ()
_is_diag result expected = do
    diag $ "         got: '" ++ (show result) ++ "'"
    diag $ "    expected: '" ++ (show expected) ++ "'"


skip :: Int -> Maybe String -> TAP Int
skip n reason = do
    let msg = case reason of Just s -> s
                             otherwise ->  ""
    forM_ [1 .. n] (\n' -> do
        modify (\x -> x {executedTests = executedTests x + 1})
        ts <- get
        lift . putStrLn $ "ok " ++ (show $ executedTests ts) ++ " # skip: " ++ msg)
    return n


skipUnless :: Bool -> Int -> Maybe String -> TAP a -> TAP Int
skipUnless cond n reason tap = do
    if cond
        then do
            tap
            return 0
        else do 
            skip n reason


diag :: String -> TAP ()
diag s = lift . putStrLn $ "# " ++ s


_die :: String -> TAP a
_die s = do 
    lift $ hPutStrLn stderr s
    modify (\x -> x {testDied = True})
    _exit $ Just 255


bailOut :: String -> TAP a
bailOut s = do
    lift $ hPutStrLn stderr s
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
                diag $ "Looks like you planned " ++ (show $ expectedTests ts) ++ " test" ++ (s $ expectedTests ts)
                    ++ " but ran " ++ (show extra) ++ " extra."
                modify (\x -> x {exitCode = -1})

            when ((not $ noPlan ts)&&((expectedTests ts) > (executedTests ts))) $ do
                diag $ "Looks like you planned " ++ (show $ expectedTests ts) ++ " test" ++ (s $ expectedTests ts) 
                    ++ " but only ran " ++ (show $ executedTests ts)

            when (failedTests ts > 0) $ do
                diag $ "Looks like you failed " ++ (show $ failedTests ts) ++ " test" ++ (s $ failedTests ts) 
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
                      else return $ ((failedTests ts) + ((expectedTests ts) - (executedTests ts)))
        modify (\x -> x {exitCode = rc})

    _wrapup
    ts <- get
    let rc = exitCode ts
    lift . exitWith $ if (rc == 0) then ExitSuccess else ExitFailure rc


runTests :: TAP a -> IO (a, TAPState)
-- Add exception handling here?
runTests s = runStateT (s >> _exit Nothing) initState
