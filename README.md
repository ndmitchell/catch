Welcome to Catch
================

_Catch was part of my PhD. It is unmaintained, and unlikely to work._

The best description of Catch is available from [my website](http://ndmitchell.com) under the publications "Not All Patterns, But Enough - an automatic verifier for partial but sufficient pattern matching" and "Transformation and Analysis of Functional Programs".

A Haskell program may fail at runtime with a pattern-match error if the program has any incomplete (non-exhaustive) patterns in definitions or case alternatives. The Catch tool is a static checker that allows non-exhaustive patterns to exist, yet ensures that a pattern-match error does not occur.

Unfortunately version incompatiblities mean that currently its very difficult to get Catch to compile. Hopefully at some point Catch can be ported to work on GHC Core, and Catch can be used on any programs which are compiled by GHC.

Users
--------------

The following programs and libraries have all had some of their source code verified by Catch. If you verify a released Haskell project with Catch, please let me know your experiences.

* <a href="http://xmonad.org/">XMonad</a> - the <a href="http://darcs.haskell.org/~sjanssen/xmonad/StackSet.hs">StackSet</a> module has been verified several times, as the design changes - see <a href="http://neilmitchell.blogspot.com/2007/05/does-xmonad-crash.html">this blog post</a> for the first instance.
* <a href="http://hackage.haskell.org/package/filepath">filepath library</a> - see <a href="http://neilmitchell.blogspot.com/2007/04/exploding-patterns-in-filepath.html">this blog post</a>.
* <a href="http://hackage.haskell.org/package/safe">safe library</a>
* Data.FiniteMap - see the Haskell Symposium 2008 paper.
* <a href="http://www.cs.york.ac.uk/fp/darcs/hscolour/">HsColour</a> - see the Haskell Symposium 2008 paper.

Related work
----

<ul>
	<li><a href="http://www.cl.cam.ac.uk/~nx200/research/escH-hw.ps">ESC/Haskell</a>, by <a href="http://www.cl.cam.ac.uk/~nx200/">Dana Xu</a> - explicit pre/post condition annotations.</li>
    <li><a href="http://www.haskell.org/haskellwiki/Non-empty_list">Non-empty lists</a> - moving the safety into the types.</li>
</ul>


A walk through of Risers
------------------------

Open the command line and type

> catch Risers

Note that it goes through various stages, and comes to the answer at the end

> Answer: _

Think of this answer as a pattern match, _ means that regardless of the inputs to Risers, it will not crash.


Preparing your code
-------------------

To prepare your code for analysis, make sure there is a main function. If the main function is a standard one (i.e. :: IO ()) then any module name will do. If you want to make any function the root of Catch checking, name it as main and change the module name to something other than Main - so Haskell does not attempt to make it Main.main.

If your code does not compile with Yhc, it will not compile with Catch.

One often used technique is to add arguments to main, and then pass them to the function under test. I strongly recommend that main does not have any classes on it, and does not take any higher order functions - although this restriction is being relaxed currently.


Reading constraints
-------------------

Here are some sample constraints, and their meanings:

_ = anything is safe

0 = nothing is safe

{True} = must be the constructor True

{[]} = must be the constructor []

{Just {True}} | {Nothing} = must be Just True or Nothing

{#2 _ {True}} = main takes 2 arguments, the second must be True

{: _ * _} = must be the constructor (:). The first _ is the restriction on the head, the * is merely a separator, and the second _ is the restriction on all tails.

{: {True} * {: {True} | []}} | [] = must be a list of any size, with True for all elements



Some useful flags
-----------------

-errors = check each error separately, I recommend including this one

-partial = give a record of each partial function found, this is useful if your program is unsafe

-screen = view the logs of what is happening, not that useful (unless you are me), until it gets to the end where it gives all the preconditions

-quiet = remove the Task: messages

-nolog = can make the program go faster, by turning off disk logging

-text = output intermediate stages, see ycr/<file>.shortctors.txt to see the final code just before analysis


Bugs and comments
---------------------

If you would like the added humiliation of watching me cry, call me over once you have managed to break Catch. If not, feel free to email me the code that doesn't work.


