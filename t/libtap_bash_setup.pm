package t::setup ;
use strict ;

use Config ;
use ExtUtils::FakeConfig useithreads => undef ;
use IO::Handle ;
use Carp ;
use Fcntl ;

my $done = 0 ;

# Fake Test::More
$INC{'Test/More.pm'} = '-' ;

my ($r, $w) = () ;
pipe $r, $w ;
autoflush $w, 1 ;
fcntl($w, F_SETFD, 0) ;
my $wno = fileno($w) ;
open(BASH, "| /bin/bash") or die("Error starting bash: $!") ;
autoflush BASH, 1 ;
open(TAP, "<tap-functions") or die("Error reading tap-functions: $!") ;
while (<TAP>){
	print BASH $_ ;
}
close(TAP) ;


sub Test::More::import {
	no strict 'refs';
	for (qw(plan_tests plan_no_plan plan_skip_all diag ok exit fail pass skip)) {
		my $func = $_ ;

		*{"main\::$func"} = sub {
			print BASH "TODO=\"$main::TODO\"\n" if $main::TODO ;
			my @args = @_ ;
			if ($func eq 'ok'){
				$args[0] = (!$args[0] ? 1 : 0) ;
			}
			print BASH "$func " . join(' ', map { "\"$_\"" } @args) . " ; echo \$? >&$wno\n" ;
			my $ret = <$r> ;
			if (! defined($ret)){
				wait() ;
				$done = 1 ;
				CORE::exit($? >> 8) ;
			}
			else {
				chomp($ret) ;
			}
			$ret = (!$ret ? 1 : 0) unless $func eq 'plan_tests' ;
			print BASH "TODO=\n" if $main::TODO ;
			return $ret ;
		} ;
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
