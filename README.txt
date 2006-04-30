NOTE: Catch is a work in progress.

It might eat your hard drive, use your credit card to buy expensive stuff on the internet or start a war with a nearby super-power.

It will NOT work reliably.




CATCH
=====

Overview
--------

Catch is a Case and Termination Checker for Haskell (that currently has no support for termination checking). The web page of Catch is located at http://www.cs.york.ac.uk/~ndm/projects/catch.php. Catch is written by Neil Mitchell as part of his PhD, at the University of York.


Tutorial
--------

Follow these steps to check a simple program.

* Install Yhc (http://www-users.cs.york.ac.uk/~ndm/yhc/) and build it. Place it somewhere so that typing yhc will run it.

* Build Catch, with GHC thats a case of "cd src && ghc --make -o catch"

* Create your test file in src/Example, for an example see Risers.hs. Make sure this file compiles correctly with Yhc, and only requires the Prelude.

* Make sure the current directory is src

* Type at the command line "catch Risers -case"

* Wait while Catch builds Risers, and the Prelude, and transforms them, then checks them.

* See the answer, safe :-)

* For more details see src/Risers.log which is created.
