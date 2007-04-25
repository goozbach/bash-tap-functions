#!/usr/bin/perl

use strict ;
use Config ;
use ExtUtils::FakeConfig useithreads => undef ;

use Test::More ;
use Inline (Java => './JTap.java') ;
use Inline (Java => 'DATA') ;

main::t02->test() ;


__DATA__
__Java__
class t02 {
	static public void test(){
		JTap t = new JTap() ;
		t.plan_tests(14) ;

		t.pass("free pass") ;
		t.ok(true) ;

		try {
			t.skip("tests skipped to test skip functionnality :)", 2) ;
		}
		catch (JTapSkipException e){} ;

		t.todo_start("todo test") ;
		t.fail("todo failure") ;
		t.todo_end() ;

		t.like("abc", "\\w+", "like regexp test") ;
		t.unlike("abc", "[z]", "unlike regexp test") ;

		t.is("a\nb", "a\nb") ;
		t.is(1, 1) ;
		t.is(1.5, 1.5) ;
		t.isnt("a", "b") ;
		t.isnt(1, 2) ;
		t.isnt(1.5, 1.9) ;

		t.isa_ok("a", java.lang.String.class, "instanceOf") ;
	}
}

