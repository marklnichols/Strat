Strat is a strategy game AI library along with some example games which utilize this library.  The main purpose of this project was to provide me with a concrete way to learn and improve my Haskell skills.   

As a library, my first goal was to build a general code base that would allow me write multiple types of games. There could eventually be a core library that others can use as well, but I'm not currently trying to address that type of library design.

Driving the development of the library are a number of example games.  A Checkers game has already been written, and I've begun work on a Chess implementation. I hope to eventually try my hand at Go as well. 

## Installing and running the Checkers example
The following instructions assume you already have Stack installed.  If not, you can install Stack at `https://docs.haskellstack.org/en/stable/README/`

Once you have Stack, compiling the library and executable follows the standard pattern:

* Clone this repository
* Type `cd strat`
* Type `stack build` 

To run the checkers example, which uses a simple web client:
* Type `stack exec strat`
* Access the game via a web browser at `http://localhost:3000`

I have tested the program on a few browsers: Chrome, Firefox, IE, and Edge.

## Project organization
At a high level, the project directories are organized this way:

* `src/StratTree` - tree traversal algorithms
* `src/StratWeb`  - general code for implementing examples via. the web browser.  (Note: some checkers-specific code has temporarily crept into these modules, but this will eventually be game agnostic)
* `src/StartIO`   - misc non-web code requiring IO
* `src/GameRunner` - a driver for running a text-only version of the game in a command window.  
* `src/Strat-commandline` - entry point for running the games - the defaul is to run the checkers game via. the web interface
* `src/Examples/Checkers` - the checkers implementation.  It plays a pretty good game, though it could use a bit more work in some end game situation where its play can become a bit aimless.
* `src/Examples/WIP/Chess` - the very initial stages of a chess example.  So far I have only begun to implement how the pieces move, etc.
* `test`   - various tests that exercise both the library functions and game specific behavior
* `test/TicTac` - A TicTacToe example that, while not particularly interesting, was very useful in getting the initial algorithms working. It can still be occasionally useful when trying out a new idea.

## High level next steps
* Fully implement the chess game
* Implement alpha-beta pruning in the move evaluation engine