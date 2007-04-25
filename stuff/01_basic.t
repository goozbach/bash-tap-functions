#!/usr/bin/perl
use strict ;
use t::JTapTester ;
# Test::More must be loaded after t::JTapTester since it 
# tweaks $Config to disable useithreads
use Test::More tests => 16 ;


# Normal test suite
my $t = new JTapTester() ;
$t->plan_tests(5) ;
ok($t->pass("free pass")) ;
ok(! $t->fail("free fail")) ;
ok($t->ok(1, "ok1")) ;
ok($t->ok(2, "ok2")) ;
ok($t->ok(3, "ok3")) ;
is(check_exit(sub {$t->exit()}), 1) ;
is(strip_failed_diags($t->get_out_buffer()), <<TAP) ;
1..5
ok 1 - free pass
not ok 2 - free fail
#     Failed test
ok 3 - ok1
ok 4 - ok2
ok 5 - ok3
# Looks like you failed 1 tests of 5.
TAP
is($t->get_err_buffer(), '') ;

# No plan
$t = new JTapTester() ;
$t->plan_no_plan() ;
ok($t->pass("free pass")) ;
ok(! $t->fail("free fail")) ;
is(check_exit(sub {$t->exit()}), 1) ;
is(strip_failed_diags($t->get_out_buffer()), <<TAP) ;
ok 1 - free pass
not ok 2 - free fail
#     Failed test
1..2
# Looks like you failed 1 tests of 2.
TAP
is($t->get_err_buffer(), '') ;

# Skip all
$t = new JTapTester() ;
is(check_exit(sub {$t->plan_skip_all('test')}), 0) ;
is(strip_failed_diags($t->get_out_buffer()), <<TAP) ;
1..0 # Skip test
TAP
is($t->get_err_buffer(), '') ;
