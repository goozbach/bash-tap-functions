#!/usr/bin/perl

use strict ;
use File::Find ;
use Test::More ;


my @test_scripts = () ;
find(\&wanted, '.') ;
plan(tests => scalar(@test_scripts)) ;

my $cnt = 0 ;
foreach my $t (@test_scripts){
	$cnt++ ;
	next if (($ARGV[0])&&($ARGV[0] != $cnt)) ;

	my $perl_output = `$^X $t 2>&1` ;
	$perl_output = fix_diags($perl_output) . "RC: $?\n" ;

	# my $java_output = `$^X -I. -Mt::libtap_java_setup $t 2>&1` ;
	# $java_output = fix_diags($java_output) . "RC: $?\n" ;

	my $java_output = `$^X -I. -Mt::libtap_bash_setup $t 2>&1` ;
	$java_output = fix_diags($java_output) . "RC: $?\n" ;

	is($java_output, $perl_output, $t) ;
}


sub wanted {
	my $name = $_ ;
	my $dir = $File::Find::dir ;
	my $path = $File::Find::name ;

	if ($name eq 'test.pl'){
		# push @test_scripts, {name => $name, dir => $dir, path => $path} ;
		push @test_scripts, $path ;
	}
}


sub fix_diags {
	my $output = shift ;

	$output =~ s/^#   (at|in) .*$//mg ;
	$output =~ s/\s*(at|in) .*$//mg ;
	$output =~ s/\s*!.*$//mg ;
	$output =~ s/\n+/\n/g ;

	return $output ;
}


