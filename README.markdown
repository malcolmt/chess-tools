Chess tools library
===================

This Haskell library provides some useful data structures for working with
chess games. It provides the framework onto which can be added things like a
storage and analysis user interface, or a chess playing engine.

There are routines for setting up a position and rapidly generating all legal
moves in that position. This list can be used by other code to provide choices
to a user, or in some kind of search for the best move to play.

The core code is deliberately generic in order to make it applicable to more
than just western-style chess. The same data structures should be useful for
[Shogi](http://en.wikipedia.org/wiki/Shogi) (Japanese chess) and
[Xiangqi](http://en.wikipedia.org/wiki/Xiangqi) (Chinese chess), as well as
other variants.

Current status
---------------

It's currently very early days for this project. Basic board structures are in
place, but not a lot more. Developer (API) documentation exists, but
descriptive user-focused documentation will be written once things have been
fleshed out a little more.

The *TODO.otl* file should give curious people a rough idea of where things are heading.

This section will be updated as more features are implemented.

