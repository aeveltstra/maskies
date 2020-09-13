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
  @version 2.20.912.1500
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
    if 0 == length args 
        then error "Expected a wire name as 1st arg."
        else if 0 == length contents
            then error "Expected stdout from Maskies to get piped into this test app."
            else print $ (test (args!!0) contents)

type ID = T.Text
type ExpectedOutput = T.Text

data KnownWire = Wire 
    { 
        wireID :: ID
        , expectedWireOutput :: ExpectedOutput
    }
    deriving (Show, Eq)

--The language extension OverloadedStrings
--allows us to write Strings and have haskell
--compile them to Data.Text.Text.
knownWires = [
    Wire "wire-1" "Come back to play another day, Wire 1? Bring your friends! Bye!"
    , Wire "wire-2" "Making Maskie's Ice Cream very popular."
    , Wire "wire-3" "Penguins can't sing the ice cream song! Can they?"
    , Wire "wire-4" "Not so clean anymore. You have nothing to protect yourself."
    , Wire "wire-5" "That's going to make a mess!"
    , Wire "wire-6" "Best ice cream in the country, aye? Not so sure about that anymore, are you, "
    , Wire "wire-7" "Much better. What a relief!"
    , Wire "wire-8" "If only you had had something to protect yourself! But you don't."
    , Wire "wire-9" "Items you need tomorrow? Better go find them!"
    , Wire "wire-10" "It's a bunny? Blue and white, and fluffy... but big! And heavy!"
    , Wire "wire-11" "Welcome back, Wire 11. This is night 3."
    , Wire "wire-12" "You're in the hallway. Alone. You hear children's music. Where is it coming from?"
    , Wire "wire-13" "So how do you know where to go? Just... pick a direction?"
    , Wire "name-1" "That's a really long name, you know."
    , Wire "name-2" "Short and sweet, aye?"
    , Wire "name-3" "How interesting! Mind if I call you Pie?"
    , Wire "name-4" "Short and sweet, aye?"
    , Wire "name-5" "Hello, Brendan. You're in a dark hallway."
    ]

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
    | otherwise = FAILURE
