module TAP where

import System.IO
import System.Exit
import Control.Monad.State
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
    lift $ _exit 0


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
ok r s = do
    _checkNotPlanned
    modify (\x -> x {executedTests = executedTests x + 1})

    case s of
        Just s -> when (_matches s "^[0-9]+$") $ do
            diag $ "    You named your test '" ++ s ++ "'.  You shouldn't use numbers for your test names."
            diag "    Very confusing."
        otherwise -> return ()

    when (not r) $ do
        lift $ putStr "not "
        modify (\x -> x {failedTests = failedTests x + 1})

    ts <- get
    lift . putStr $ "ok " ++ (show $ executedTests ts)

    case s of
        Just s -> lift . putStr $ " - " ++ s
        otherwise -> return ()
  
    -- TODO
 
    lift $ putStrLn ""

    -- STACK TRACE

    return r


_matches :: String -> String -> Bool
_matches "" _ = False
_matches _ "" = False
_matches target pattern = target =~ pattern :: Bool


diag :: String -> TAP ()
diag s = do 
  lift . putStrLn $ "# " ++ s


_die :: String -> TAP a
_die s = do 
  lift $ hPutStrLn stderr s
  modify (\x -> x {testDied = True})
  lift $ _exit 255


bailOut :: String -> TAP a
bailOut s = do
  lift $ hPutStrLn stderr s
  lift $ _exit 255


_exit :: Int -> IO a
_exit 0  = exitWith ExitSuccess
_exit rc = exitWith $ ExitFailure rc


runSuite :: TAP a -> IO (a, TapState)
runSuite s = runStateT s initState

main = runSuite $ do
   planTests 5 Nothing
   ok True $ Just "True"
   



{-

# Used to call _cleanup on shell exit
trap _exit EXIT


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


_matches(){
	local result=${1:?}
	local pattern=${2:?}

	if [[ -z "$result" || -z "$pattern" ]] ; then
		return 1
	elif [[ "$result" =~ "$pattern" ]] ; then
		return 0
	else 
		return 1
	fi
}


_is_diag(){
	local result=${1:?}
	local expected=${2:?}

	diag "         got: '$result'" 
	diag "    expected: '$expected'"
}


is(){
	local result=${1:?}
	local expected=${2:?}
	local name=${3:-''}

	_equals "$result" "$expected"
	(( $? == 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && _is_diag "$result" "$expected"
	return $r 
}


isnt(){
	local result=${1:?}
	local expected=${2:?}
	local name=${3:-''}

	_equals "$result" "$expected"
	(( $? != 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && _is_diag "$result" "$expected"
	return $r 
}


like(){
	local result=${1:?}
	local pattern=${2:?}
	local name=${3:-''}

	_matches "$result" "$pattern"
	(( $? == 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && diag "    '$result' doesn't match '$pattern'"
	return $r
}


unlike(){
	local result=${1:?}
	local pattern=${2:?}
	local name=${3:-''}

	_matches "$result" "$pattern"
	(( $? != 0 ))
	ok $? "$name"
	local r=$?
	(( r != 0 )) && diag "    '$result' matches '$pattern'"
	return $r
}


skip(){
	local condition=${1:?}
	local reason=${2:-''}
	local n=${3:-1}

	if (( condition == 0 )) ; then
		local i=
		for (( i=0 ; i<$n ; i++ )) ; do
			_executed_tests=$(( _executed_tests + 1 ))
			echo "ok $_executed_tests # skip: $reason" 
		done
		return 0
	else
		return
	fi
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


_exit_status(){
	if (( _no_plan != 0 || _plan_set == 0 )) ; then
		return $_failed_tests
	fi

	if (( _expected_tests < _executed_tests )) ; then
		return $(( _executed_tests - _expected_tests  ))
	fi

	return $(( _failed_tests + ( _expected_tests - _executed_tests )))
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
