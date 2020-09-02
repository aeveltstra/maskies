{-# LANGUAGE OverloadedStrings #-}

{-
  This testing application was written to test the 
  game Maskies. The game accepts key presses via stdin.
  Given a certain list of those, we expect a certain 
  outcome in the game's stdout. This testing app can
  compare that output to expectations. To use it, 
  run it on a terminal prompt like so:

  $ cat ./wire-1.txt | ./Maskies | tail -3 | ./MaskiesWireTest "wire-1"
  and the output is either SUCCESS or FAILURE.

  Success depends on 3 factors:
  1. The wire was configured properly;
  2. This test application knows the wire's expected outcome;
  3. Maskies operates according to expectation.

  @author A.E.Veltstra
  @since 2.20.901.2100
  @version 2.20.902.0205
-}
module Main where

import Prelude
import qualified System.Environment as Env
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as Search

{- This is the entry point of the application. -}
main :: IO ()
main = do
    args <- Env.getArgs --to pick up the wire name
    contents <- getContents --to pick up the stdin
    print $ (test (args!!0) contents)

type ID = T.Text
type ExpectedOutput = T.Text

data KnownWire = Wire 
    { wireID :: ID
      , expectedWireOutput :: ExpectedOutput
    }
    deriving (Show, Eq)

--The language extension OverloadedStrings
--allows us to write Strings and have haskell
--compile them to Data.Text.Text.
wire1 = Wire "wire-1" "Come back to play another day, Wire 1? Bring your friends! Bye!"
wire2 = Wire "wire-2" "That makes Maskie's a popular place."
wire3 = Wire "wire-3" "This is not the right time for the ice cream song!"
wire4 = Wire "wire-4" "Not so clean anymore. You have nothing to protect yourself."
wire5 = Wire "wire-5" "That's going to make a mess!"
wire6 = Wire "wire-6" "Best ice cream in the country, aye? Not so sure about that anymore, are you, "
wire7 = Wire "wire-7" "Much better. What a relief!"
wire8 = Wire "wire-8" "If only you had had something to protect yourself! But you don't."
wire9 = Wire "wire-9" "Items you need tomorrow? Better go find them!"
wire10 = Wire "wire-10" "Its metal face looks human, but not quite."
wire11 = Wire "wire-11" "Welcome back, Wire 11. This is night 3."
wire12 = Wire "wire-12" "You're in the hallway. Alone. To turn left, press a."
wire13 = Wire "wire-13" "You have nothing to protect yourself and no map to tell you where to go."

knownWires = [wire1, wire2, wire3, wire4, wire5, wire6, wire7, wire8, wire9, wire10, wire11, wire12, wire13]

findKnownWire :: T.Text -> Maybe KnownWire
findKnownWire name = List.find(\x -> wireID x == name) knownWires

isWireKnown :: T.Text -> Bool
isWireKnown name = case findKnownWire name of
                       Nothing -> False
                       Just _ -> True

getExpectedWireOutput :: T.Text -> T.Text
getExpectedWireOutput name = case findKnownWire name of
                                 Nothing -> ""
                                 Just wire -> expectedWireOutput wire

containsOutputFrom :: T.Text -> T.Text -> Bool
containsOutputFrom name actual = [] /= Search.indices expected actual where
    expected = getExpectedWireOutput name

data TestResult
    = SUCCESS
    | FAILURE
    deriving (Show, Eq)

test :: String -> String -> TestResult 
test [] [] = error "Received no arguments. Expected: wire identifier, stdout from applying that wire to Maskies."
test name contents
    | containsOutputFrom (T.pack name) (T.pack contents) = SUCCESS
    | otherwise = FAILURE --show [name, contents]
