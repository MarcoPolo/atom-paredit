[![Stories in Ready](https://badge.waffle.io/MarcoPolo/atom-paredit.png?label=ready&title=Ready)](https://waffle.io/MarcoPolo/atom-paredit)
# Atom Paredit

Brings Paredit to atom!

![A screenshot of your spankin' package](http://cl.ly/image/0w2o1j0E0r0E/Screen%20Shot%202014-07-16%20at%208.19.36%20PM.png)


## What's implemented

Forward Barf  `C-left`  
Forward Slurp `C-right`  

Backward Barf `C-M-right`  
Backward Slurp `C-M-left`  

Kill to end of line `C-k`  
Raise expression `M-r`  

M = Meta which may be your alt/option key.

## What's left

The rest of the paredit commands.  
Getting Existing commands to work with ""

## Contributing

Yes please!  

If your command isn't implemented, make an issue and begin
tell everyone else you are working on it.

If you want to get a feel for the codebase, but don't think
you can contribute just yet, we need tests on existing
functionality. The core is written with a handful of functions
that can easily be mocked. Look at `core-test.cljs` for the
mocks.

If in doubt open an issue and we can talk about it.

This is part of a bigger endeavor to build an
amazing Clojure IDE within atom. Contact me if you
are interested in working on that as well.
