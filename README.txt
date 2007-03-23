Welcome to Catch
================

You are privilaged to be one of the first alpha testers for Catch.

Catch has now been installed in /grp/haskell (at York), and provided you use the standard bash-paths script on your system, you can run Catch.


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


