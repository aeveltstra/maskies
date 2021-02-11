{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    This file contains functions and data to validate
    a player’s name and choose a different one if
    needed
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.21.211.1239
-}
module NameValidation where
import qualified Data.Text.Lazy as TL
import qualified TextWrapper as TW

import Prelude
import qualified System.Random as R

type UserName = TL.Text

{- | These are the results recognized by the validation of the player’s name. -}
data Result = AllGood
            | TooLong
            | TooShort
            | Pie
            | IsFnaf
            | IsBetty
            | IsJeremy
                deriving (Bounded, Read, Show, Eq, Enum)

{- | Ensures that the player name is neither too long nor too short. And a special case is made for pi, because a certain someone found it necessary to boast their knowledge of the first 100 digits of pi. You know who you are. -}
validate :: UserName -> Result
validate "" = TooShort
validate player
  | 0 < (TL.count "3.1415" player) = Pie
  | TL.length player > 30 = TooLong
  | isFnaf player = IsFnaf
  | isBetty player = IsBetty
  | isJeremy player = IsJeremy
  | otherwise = AllGood

subsForTooLong :: [UserName]
subsForTooLong
  = ["Aadhira", "Aadesh", "Saanvi", "Aaditi", "Aadit", "Anika", "Aisha",
     "Ananya", "Arav", "Alisha", "Kaavya", "Saatvik", "Ahan", "Abhi"]

subsForTooShort :: [UserName]
subsForTooShort
  = ["Elikapeka", "Kakalina", "Leimomi", "Mahaelani", "Waiola", 
     "Ekewaka", "Uluwehi", "Healani", "Lanakila", "Leialoha"]

fnaf :: [UserName]
fnaf
  --"freddy", "gabriel", "jeremy", "fritz", "charlie", "charlotte", "clara"
  = ["fazbear", "chica", "bonnie", "foxy", "golden", "nightmare",
     "glitch", "withered", "goldie", "molten", "cassidy", "puppet",
     "marionette", "nightmarion", "afton", "mr. emily", "ballora",
     "circus baby", "trap", "scrap", "lefty", "helpy", "balloonboy", 
     "phantom", "deedee", "funtimes", "fun times", "tape girl", 
     "vanny", "purple guy", "orange man"]

isFnaf :: UserName -> Bool
isFnaf player = any (\ x -> 0 < (TL.count x y)) fnaf
  where y = TL.toLower player

isBetty :: UserName -> Bool
isBetty player = "betty" == (TL.toLower player)

isJeremy :: UserName -> Bool
isJeremy player = "jeremy" == (TL.toLower player)

{- | Given a specific name validation result, this function returns the passed-in name or something else. -}
replace :: UserName -> Result -> R.StdGen -> UserName
replace _ TooLong seed = pick seed subsForTooLong
replace _ TooShort seed = pick seed subsForTooShort
replace _ Pie _ = "Pie"
replace _ IsFnaf _ = "Jeremy"
replace player _ _ = player

pick :: R.StdGen -> [a] -> a
pick seed xs
  | length xs == 1 = xs !! 0
  | otherwise = x
  where x = xs !! fst (R.randomR (0, (length xs) - 1) seed)

{- | If the validation of the player’s name says it should be changed, this function lets them know about it. -}
show :: Result -> UserName -> IO ()
show AllGood _ = return ()
show TooLong newName = TW.wrap (TL.replace "{name}" newName
  "That’s a really long name, you know. I will call you {name}.")
show TooShort newName = TW.wrap (TL.replace "{name}" newName
  "Short and sweet, aye? From now on, you will be known as {name}.")
show Pie newName = TW.wrap (TL.replace "{name}" newName 
  "How interesting! Mind if I call you {name}?")
show IsFnaf newName = TW.wrap (TL.replace "{name}" newName
  "This isn’t FNAF, you know... I will call you {name}.")
show IsBetty _ = TW.wrap "When you call me, you can call me Al."
show IsJeremy _ = TW.wrap "I know a tester by that name."
