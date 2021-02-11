{-# LANGUAGE OverloadedStrings #-}

{- |
  This testing application was written to test the 
  game Maskies. The game accepts key presses via stdin.
  Given a certain list of those, we expect a certain 
  outcome in the game’s stdout. This testing app can
  compare that output to expectations. To use it, 
  run it on a terminal prompt like so:

  $ cat ./wire-1.txt | ./Maskies | tail -3 | ./MaskiesWireTest "wire-1"
  and the output is either SUCCESS or FAILURE.

  Success depends on 3 factors:
  1. The wire was configured properly;
  2. This test application knows the wire’s expected outcome;
  3. Maskies operates according to expectation.

  @author A.E.Veltstra
  @since 2.20.901.2100
  @version 2.21.209.1846
-}
module Main where

import Prelude
import qualified System.Environment as Env
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Internal.Search as Search

type Actual = T.Text
type Expected = T.Text
type ID = T.Text

{- This is the entry point of the application. -}
main :: IO ()
main = do
    args <- Env.getArgs --to pick up the wire name
    stdin <- getContents --to pick up the stdin
    if null args 
        then error "Expected a wire name as 1st arg."
        else if null stdin
            then error "Expected stdout from Maskies to get piped into this test app."
            else do 
                   let t = T.pack (args!!0)
                       actual = T.pack stdin
                       expected = getExpected t
                       p = test expected actual
                   case p of
                       SUCCESS -> TIO.putStrLn (T.concat ["SUCCESS. Test: ", t])
                       FAILURE -> TIO.putStrLn (T.concat ["FAILURE. Test: ", t, ". Expected result to contain this: [", expected, "]. Instead found: [", actual, "]."])

data KnownWire = Wire 
    { 
        wireID :: ID
        , expectedWireOutput :: Expected
    }
    deriving (Show, Eq)

--The language extension OverloadedStrings
--allows us to write Strings and have haskell
--compile them to Data.Text.Text.
knownWires = [
    Wire "wire-1" "Come back to play another day, Wire 1? Bring your friends! Bye!"
    , Wire "wire-2" "Making Maskie’s Ice Cream very popular."
    , Wire "wire-3" "Penguins can’t sing the ice cream song! Can they?"
    , Wire "wire-4" "Not so clean anymore. You have nothing to protect yourself."
    , Wire "wire-5" "That’s going to make a mess!"
    , Wire "wire-6" "Best ice cream in the country, aye? Not so sure about that anymore, are you, "
    , Wire "wire-7" "Much better. What a relief!"
    , Wire "wire-8" "If only you had had something to protect yourself! But you don’t."
    , Wire "wire-9" "Items you need tomorrow? Better go find them!"
    , Wire "wire-10" "It’s a bunny? Blue and white, and fluffy… but big! And heavy!"
    , Wire "wire-11" "Welcome back, Wire 11. This is night 3."
    , Wire "wire-12" "You’re in the hallway. Alone. You hear children’s music. Where is it coming from?"
    , Wire "wire-13" "So how do you know where to go? Just… pick a direction?"
    , Wire "wire-14" "Gross: your brains splattered all over the toilet and the stall walls. Wanna try that again?"
    , Wire "wire-15" "Just in time you duck out of the way. A red ball flies through the air, wielding a hammer, aiming for your head."
    , Wire "wire-16" "The hand that grabs you is too strong. Lifts you right up off the floor."
    , Wire "wire-17" "How hard is it to press S, Wire 17? Very hard, apparently. Did you think I was joking? You were wrong."
    , Wire "wire-18" "You feel your heart beating in your chest. Your ears are hot with blood."
    , Wire "wire-19" "Oh, dear. Erm. That’s a mess. Erm. So. I guess this is game over?"
    , Wire "wire-20" "Some people got left behind in the obstacle course yesterday. You should have found them and flushed them out. You failed."
    , Wire "wire-21" "This office really is a boring place, wouldn’t you agree, Wire 21? Let’s get out of here."
    , Wire "wire-22" "You found a map! That letter you"
    , Wire "name-1" "That’s a really long name, you know."
    , Wire "name-2" "Short and sweet, aye?"
    , Wire "name-3" "How interesting! Mind if I call you Pie?"
    , Wire "name-4" "Short and sweet, aye?"
    , Wire "name-5" "Hello, Brendan. You’re in a dark hallway."
    ]

findKnownWire :: ID -> Maybe KnownWire
findKnownWire name = List.find(\x -> wireID x == name) knownWires

isWireKnown :: ID -> Bool
isWireKnown name = case findKnownWire name of
                   Nothing -> False
                   Just _ -> True

getExpected :: ID -> Expected
getExpected name = case findKnownWire name of
                   Nothing -> ""
                   Just wire -> expectedWireOutput wire

containsOutputFrom :: Expected -> Actual -> Bool
containsOutputFrom expected actual = [] /= Search.indices expected actual

data TestResult
    = SUCCESS
    | FAILURE
    deriving (Bounded, Eq, Enum, Show)

test :: Expected -> Actual -> TestResult 
test "" "" = FAILURE
test name contents 
  | containsOutputFrom name contents = SUCCESS
  | otherwise = FAILURE
