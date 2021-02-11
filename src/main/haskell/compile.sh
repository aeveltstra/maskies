# Compiles Maskies from scratch using ghc.
# Assumes all dependencies already got downloaded and are accessible.
#
# Requires:
# - GHC 8.8.3 or newer.

ghc --make -package random -package word-wrap -package terminal-size -O2 -o ../../../bin/Maskies ./Main.hs
