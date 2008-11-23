# Here's a naive Makefile for building the regression test system.

run-tests : 
	if test ! -d bin/.heap ; then mkdir bin/.heap; fi
	cd src && ml-build sources.cm Main.main ../bin/.heap/run-tests

