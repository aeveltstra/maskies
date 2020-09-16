{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    This file contains functions and data to validate
    a player's name and choose a different one if
    needed
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.915.2022
-}
module NameValidation where

import Prelude
import qualified Data.Text as T
import qualified TextHelper as TH
import qualified System.Random as R


{- These are the results recognized by the validation of the player's name. -}
data Result
    = AllGood
    | TooLong
    | TooShort
    | Pie
    | IsFnaf
    deriving (Show, Eq, Enum)

{- Ensures that the player name is neither too long nor too short. And a special case is made for pi, because a certain someone found it necessary to boast their knowledge of the first 100 digits of pi. You know who you are. -}
validate :: T.Text -> Result
validate ""  = TooShort
validate player 
    | 0 < (T.count "3.1415" player) = Pie
    | T.length player > 30  = TooLong
    | isFnaf player = IsFnaf
    | otherwise = AllGood

subsForTooLong :: [T.Text]
subsForTooLong = [
   "Aadhira"
   , "Aadesh"
   , "Saanvi"
   , "Aaditi"
   , "Aadit"
   , "Anika"
   , "Aisha"
   , "Ananya"
   , "Arav"
   , "Alisha"
   , "Kaavya"
   , "Saatvik"
   , "Ahan"
   , "Abhi"
   ]

subsForTooShort :: [T.Text]
subsForTooShort = [
    "Elikapeka"
   , "Kakalina"
   , "Leimomi"
   , "Mahaelani"
   , "Waiola"
   , "Ekewaka"
   , "Uluwehi"
   , "Healani"
   , "Lanakila"
   , "Leialoha"
   ]

fnaf :: [T.Text]
fnaf = [
    --"freddy"
    "fazbear"
    --, "gabriel"
    , "chica"
    , "susie"
    , "bonnie"
    --, "jeremy"
    , "foxy"
    --, "fritz"
    , "golden"
    , "nightmare"
    , "glitch"
    , "withered"
    , "goldie"
    , "molten"
    , "cassidy"
    , "puppet"
    , "marionette"
    , "nightmarion"
    , "afton"
    --, "charlie"
    --, "charlotte"
    --, "clara"
    , "mr. emily"
    , "ballora"
    , "circus baby"
    , "trap"
    , "scrap"
    , "lefty"
    , "helpy"
    , "balloonboy"
    , "phantom"
    , "deedee"
    , "funtimes"
    , "fun times"
    , "tape girl"
    , "vanny"
    , "purple guy"
    , "orange man"
    ]

isFnaf :: T.Text -> Bool
isFnaf player = any (\x -> 0 < (T.count x y)) fnaf where
    y = T.toLower player

{- Given a specific name validation result, this function returns the passed-in name or something else. -}
replace :: T.Text -> Result -> R.StdGen -> T.Text
replace player AllGood  _    = player
replace _      TooLong  seed = pick seed subsForTooLong
replace _      TooShort seed = pick seed subsForTooShort
replace _      Pie      _    = "Pie"
replace _      IsFnaf   _    = "Jeremy"

pick :: R.StdGen -> [a] -> a
pick seed xs 
    | length xs == 1  = xs!!0
    | otherwise       = x where
        x = xs!!fst (R.randomR (0, (length xs) -1) seed)
    

{- If the validation of the player's name says it should be changed, this function lets them know about it. -}
outputIfChanged :: T.Text -> Result -> IO ()
outputIfChanged _ AllGood = return ()
outputIfChanged newName TooLong = TH.ln (T.replace "{name}" newName "That's a really long name, you know. I will call you {name}.")
outputIfChanged newName TooShort = TH.ln (T.replace "{name}" newName "Short and sweet, aye? From now on, you will be known as {name}.")
outputIfChanged newName Pie = TH.ln (T.replace "{name}" newName "How interesting! Mind if I call you {name}?")
outputIfChanged newName IsFnaf = TH.ln (T.replace "{name}" newName "This isn't FNAF, you know... I will call you {name}.")

