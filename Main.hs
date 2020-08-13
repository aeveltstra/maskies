{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Text as T

putTxtLn :: T.Text -> IO ()
putTxtLn input = putStrLn $ T.unpack input

--Finite State Machine 
--Modelled after the example provided by
--https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html


type Name = T.Text
emptyName = ""

data Direction
    = Q
    | W
    | A
    | S
    | D
    deriving (Show, Eq)

direct :: Char -> Direction
direct 'w' = W
direct 'a' = A
direct 's' = S
direct 'd' = D
direct _ = Q

data Inputs
    = None
    | GiveName Name
    | GiveDirection Direction
    deriving (Show, Eq)

data A1Welcome
data A2DarkHallway
data A5wLightAppears
data A5aTheStorageRoom
data A5dTheParlor
data Quit

--Defines stage transitions (finite state machine)
--Requires language extension TypeFamilies.
class Next m where 
    type Stage m :: * -> *
    welcome :: m (Stage m A1Welcome)
    theHallway :: Stage m A1Welcome -> Name -> m (Stage m A2DarkHallway)
    whereTo :: Stage m A2DarkHallway -> Direction -> m (Stage m Quit)
    quit :: Stage m Quit -> m Quit
    

open001 :: T.Text
open001 = "Welcome to Matice Ice Cream. What is your name?"

openSetPrimaryStage = putTxtLn open001

open002 :: T.Text -> T.Text
open002 name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "Hello, {name}. You're in a dark hallway."

openSetStage2 name = putTxtLn $ open002 name

open003 :: T.Text
open003 = "Which way to go? Forward: press w. Left: a. Right: d. Give up and quit: q."

openGiveDirectionChoice003 = putTxtLn open003

open004 :: T.Text
open004 = "Come back to play another day? Bye!"

openGetStage :: Integer -> Char -> Integer
openGetStage last move = 1

openSetStage :: Integer -> IO ()
openSetStage next = putTxtLn open004


main :: IO ()
main = do
    openSetPrimaryStage
    nameStr <- getLine
    let name = T.pack nameStr
    openSetStage2 name
    openGiveDirectionChoice003
    goChoice <- getChar
    putStrLn ""
    let nextStage = openGetStage 0 goChoice
    openSetStage nextStage
