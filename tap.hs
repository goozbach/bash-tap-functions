module TAP where

import System.IO
import System.Exit
import Control.Monad.State
import Control.Exception
import Text.Regex.Posix


data TapState = TapState {
  planSet :: Bool,
  noPlan :: Bool,
  skipAll :: Bool,
  testDied :: Bool,
  expectedTests :: Int,
  executedTests :: Int,
  failedTests :: Int
} deriving (Show)

initState = TapState {
  planSet = False,
  noPlan = False,
  skipAll = False,
  testDied = False,
  expectedTests = 0,
  executedTests = 0,
  failedTests = 0
}

type TAP a = StateT TapState IO a


_checkPlanned :: TAP ()
_checkPlanned = do
    ts <- get
    when (planSet ts) $ do
        _die "You tried to plan twice!"


_checkNotPlanned :: TAP ()
_checkNotPlanned = do
    ts <- get
    when (not $ planSet ts) $ do
        _die "You tried to run a test without a plan!  Gotta have a plan."


planNoPlan :: TAP Int
planNoPlan = do 
    _checkPlanned
    modify (\x -> x {planSet = True, noPlan = True})
    return 0


planSkipAll :: Maybe String -> TAP Int
planSkipAll s = do 
    _checkPlanned
    lift . _printPlan 0 . Just $ "Skip " ++ 
        case s of
            Just s -> s
            otherwise -> ""
    modify (\x -> x {planSet = True, skipAll = True})
    _exit $ Just 0


planTests :: Int -> Maybe String -> TAP Int
planTests n s = do 
    _checkPlanned
    when (n == 0) $ do
        _die "You said to run 0 tests!  You've got to run something."
    lift $ _printPlan n s 
    modify (\x -> x {planSet = True, expectedTests = n})
    return n


_printPlan :: Int -> Maybe String -> IO Int
_printPlan n s = do
    putStrLn $ "1.." ++ show n ++ 
        case s of
             Just s -> " # " ++ s
             otherwise -> ""
    return n


pass :: Maybe String -> TAP Bool
pass s = do
    ok True s


fail :: Maybe String -> TAP Bool
fail s = do
    ok False s


ok :: Bool -> Maybe String -> TAP Bool
ok result msg = do
    _checkNotPlanned
    modify (\x -> x {executedTests = executedTests x + 1})

    case msg of
        Just s -> when (_matches s "^[0-9]+$") $ do
            diag $ "    You named your test '" ++ s ++ "'.  You shouldn't use numbers for your test names."
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
    when (not rc) $ do
       diag $ "    '" ++ target ++ "' doesn't match '" ++ pattern ++ "'"
    return rc


unlike :: String -> String -> Maybe String -> TAP Bool
unlike target pattern msg = do
    rc <- ok (not $ _matches target pattern) msg
    when (not rc) $ do
       diag $ "    '" ++ target ++ "' matches '" ++ pattern ++ "'"
    return rc


skip :: Int -> Maybe String -> TAP ()
skip n reason = do
    let msg = case reason of Just s -> s
                             otherwise ->  ""
    forM_ [1 .. n] (\n' -> do
        modify (\x -> x {executedTests = executedTests x + 1})
        ts <- get
        lift . putStrLn $ "ok " ++ (show $ executedTests ts) ++ " # skip: " ++ msg)
    return ()


diag :: String -> TAP ()
diag s = do 
    lift . putStrLn $ "# " ++ s


_die :: String -> TAP a
_die s = do 
    lift $ hPutStrLn stderr s
    modify (\x -> x {testDied = True})
    _exit $ Just 255


bailOut :: String -> TAP a
bailOut s = do
    lift $ hPutStrLn stderr s
    _exit $ Just 255


_cleanup :: Int -> TAP Int
_cleanup rc = do
    let s n = if (n > 1) then "s" else ""
    ts <- get
	if (not $ planSet ts)
        then do
            diag "Looks like your test died before it could output anything."
            return rc
        else if (testDied ts)
            then do
                diag $ "Looks like your test died just after " ++ (show $ executedTests ts)
                return rc
            else
                if (( _skip_all == 0 && _no_plan != 0 )) ; then
		_print_plan $_executed_tests
	fi

	when (((not $ noPlan ts)&&((_expected_tests) < (_executed_tests)) $ do
		local extra=$(( _executed_tests - _expected_tests ))
		diag "Looks like you planned $_expected_tests test$s but ran $extra extra."
		rc=-1 ;
	fi

	when ((not $ noPlan ts)&&((expectedTests ts) > (_executedTests ts))) $ do
		diag $ "Looks like you planned " ++ (show $ expectedTests ts)) ++ " test" ++ (s $ expectedTests ts) 
            ++ " but only ran " ++ (show $ executedTests ts)

	when (failedTests ts > 0) $ do
		diag $ "Looks like you failed " ++ (show $ failedTests ts)) ++ " test" ++ (s $ failedTests ts) 
            ++ " of " ++ (show $ executedTests ts)

	return rc


_exitStatus :: TAP Int
_exitStatus = do
    ts <- get
    if ((noPlan ts)||(not $ planSet ts)) 
        then return $ failedTests ts
        else if ((expectedTests ts) < (executedTests ts))
            then return $ (executedTests ts) - (expectedTests ts)
            else return $ ((failedTests ts) + ((expectedTests ts) - (executedTests ts)))


_exit :: Maybe Int -> TAP a
_exit mrc = do
    es <- _exitStatus
    let rc = _cleanup $ case mrc of Just rc -> rc
                                    otherwise -> es
    lift . exitWith $ if (rc == 0) 
                   then ExitSuccess
                   else ExitFailure rc
 

runTests :: TAP a -> IO (a, TapState)
runTests s = runStateT (s >> _exit Nothing) initState


main = runTests $ do
   planTests 8 Nothing
   ok True $ Just "64"
   is "a" "a" $ Just "a == a"
   isnt "a" "a" $ Just "a == a"
   like "abc" "a" Nothing
   unlike "abc" "a" Nothing
   skip 3 $ Just "rope"
   



{-

# This is the workhorse method that actually
# prints the tests result.
ok(){
	local result=${1:?}
	local name=${2:-''}

	(( _plan_set == 0 )) && _die "You tried to run a test without a plan!  Gotta have a plan."

	_executed_tests=$(( $_executed_tests + 1 ))

	if [[ -n "$name" ]] ; then
		if _matches "$name" "^[0-9]+$" ; then
			diag "    You named your test '$name'.  You shouldn't use numbers for your test names."
			diag "    Very confusing."
		fi
	fi

	if (( result != 0 )) ; then
		echo -n "not "
		_failed_tests=$(( _failed_tests + 1 ))
	fi
	echo -n "ok $_executed_tests"

	if [[ -n "$name" ]] ; then
		local ename=${name//\#/\\#}
		echo -n " - $ename"
	fi

	if [[ -n "$TODO" ]] ; then
		echo -n " # TODO $TODO" ;
		if (( result != 0 )) ; then
			_failed_tests=$(( _failed_tests - 1 ))
		fi
	fi

	echo
	if (( result != 0 )) ; then
		local file='tap-functions'
		local func=
		local line=

		local i=0
		local bt=$(caller $i)
		while [[ "$bt" =~ "tap-functions$" ]] ; do
			i=$(( $i + 1 ))
			bt=$(caller $i)
		done
		local backtrace=
		eval $(caller $i | (read line func file ; echo "backtrace=\"$file:$func() at line $line.\""))
			
		local t=
		[[ -n "$TODO" ]] && t="(TODO) "

		if [[ -n "$name" ]] ; then
			diag "  Failed ${t}test '$name'"
			diag "  in $backtrace"
		else
			diag "  Failed ${t}test in $backtrace"
		fi
	fi

	return $result
}


_cleanup(){
	local rc=0

	if (( _plan_set == 0 )) ; then
		diag "Looks like your test died before it could output anything."
		return $rc
	fi

	if (( _test_died != 0 )) ; then
		diag "Looks like your test died just after $_executed_tests."
		return $rc
	fi

	if (( _skip_all == 0 && _no_plan != 0 )) ; then
		_print_plan $_executed_tests
	fi

	local s=
	if (( _no_plan == 0 && _expected_tests < _executed_tests )) ; then
		s= ; (( _expected_tests > 1 )) && s=s
		local extra=$(( _executed_tests - _expected_tests ))
		diag "Looks like you planned $_expected_tests test$s but ran $extra extra."
		rc=-1 ;
	fi

	if (( _no_plan == 0 && _expected_tests > _executed_tests )) ; then
		s= ; (( _expected_tests > 1 )) && s=s
		diag "Looks like you planned $_expected_tests test$s but only ran $_executed_tests."
	fi

	if (( _failed_tests > 0 )) ; then
		s= ; (( _failed_tests > 1 )) && s=s
		diag "Looks like you failed $_failed_tests test$s of $_executed_tests."
	fi

	return $rc
}


_exit(){
	local rc=${1:-''}
	if [[ -z "$rc" ]] ; then
		_exit_status
		rc=$?
	fi

	_cleanup
	local alt_rc=$?
	(( alt_rc != 0 )) && rc=$alt_rc
	trap - EXIT
	exit $rc
}

-}
