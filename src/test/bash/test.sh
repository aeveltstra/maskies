#!/bin/bash
# Runs known wire tests. For each test,
# its name and result (SUCCESS or FAILURE)
# gets output.
# @author A.E.Veltstra: aev@sdf.org
# @since 2.20.902.0240
# @version 2.22.207.637
# This script assumes the following:
# 1. It lives in a grand-child folder inside
#    the one in which the game Maskies lives.
# 2. It has cousin files named "wire-1.txt"
#    and "name-1.txt" etc. in a sibling 
#    folder named "resources".
# 3. It has a parent program named 
#    MaskiesWireTest that checks the output
#    generated by Maskies based on a wire.
# 4. MaskiesWireTest knows each wire by name
#    and the output each expects.
#
# To build the Maskies game and the Maskies-
# WireTest program, you need:
# - GHC 8.8.3 or newer;
# - Cabal 3.2.0 or newer.
#
# At a terminal prompt, run this command to 
# download required dependencies and ensure
# the game builds:
#
# $ cabal build
# $ ghc --make -O -o ./bin/Maskies ./src/haskell/Main.hs

list=../resources/wire-*.txt
for wirefile in $list; do
    wire=$(basename -as .txt "$wirefile")
    cat "$wirefile" | ../../../bin/Maskies | tail -7 | ../MaskiesWireTest "$wire"
done

list=../resources/name-*.txt
for namefile in $list; do
    name=$(basename -as .txt "$namefile")
    cat "$namefile" | ../../../bin/Maskies | tail -6 | ../MaskiesWireTest "$name"
done
