#!/bin/sh
{
  cabal exec strat-exe -- -d4 -rdebug03
} 2>&1 | tee output.file
