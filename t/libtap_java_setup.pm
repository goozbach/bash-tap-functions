package t::setup ;
use strict ;

use Config ;
use ExtUtils::FakeConfig useithreads => undef ;
use Carp ;

use Inline::Java qw(caught) ;
use Inline (Java => './JTap.java') ;


my $jtap = new t::setup::JTap(0) ;
my $done = 0 ;

# Fake Test::More
$INC{'Test/More.pm'} = '-' ;


sub Test::More::import {
	no strict 'refs';
	my $module = 't::setup::JTap' ;
	for (keys %{"${module}::"}) {
		if (exists &{"${module}::$_"}){
			my $func = $_ ;
			my $sub = \&{"$module\::$func"} ;

			*{"main\::$func"} = sub {
				my $ret = undef ;
				eval {
					$jtap->todo_start($main::TODO) if $main::TODO ;
					$ret = $sub->($jtap, @_) ;
					$jtap->todo_end() if $main::TODO ;
    			} ;
				if ($@){
					if (caught('JTapException')){
						my $msg = $@->getMessage() ;
						if ($msg =~ /^exit (-?\d+)$/){
							$done = 1 ;
							CORE::exit($1) ;
						}
						elsif ($msg =~ /^skip /){
							last SKIP ;
						}
					}
					die $@ ;
				}
				return $ret ;
			} ;

		}
	}
	my $TODO = undef ;
	*{"main\::TODO"} = \$TODO ;
}


# Setup a Perl-like stub for the plan method...
sub main::plan {
	my $verb = shift ;

	if ($verb eq 'no_plan'){
		return main::plan_no_plan() ;
	}
	elsif ($verb eq 'skip_all'){
		return main::plan_skip_all(@_) ;
	}
	elsif ($verb eq 'tests'){
		return main::plan_tests(@_) ;
	}
	else {
		croak("Unsupported plan type '$verb'") ;
	}
}


END {
	main::exit() unless $done ;
}



1 ;
