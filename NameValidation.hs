{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    This file contains functions and data to validate
    a player's name and choose a different one if
    needed
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.906.1122
-}
module NameValidation where

import Prelude
import qualified Data.Char
import qualified Data.Text as T
import qualified TextHelper as TH
import qualified System.IO
import qualified System.Random as R


{- These are the results recognized by the validation of the player's name. -}
data PlayerNameValidationResult
    = AllGood
    | TooLong
    | TooShort
    | Pie
    deriving (Show, Eq)

{- Ensures that the player name is neither too long nor too short. And a special case is made for pi, because a certain someone found it necessary to boast their knowledge of the first 100 digits of pi. You know who you are. -}
validatePlayerName :: T.Text -> PlayerNameValidationResult
validatePlayerName ""  = TooShort
validatePlayerName player 
    | 0 < (T.count "3.1415" player) = Pie
    | T.length player > 30  = TooLong
    | otherwise = AllGood

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

{- Given a specific name validation result, this function returns the passed-in name or something else. -}
replaceName :: T.Text -> PlayerNameValidationResult -> R.StdGen -> T.Text
replaceName player AllGood _ = player
replaceName _ TooLong seed = pick seed subsForTooLong
replaceName _ TooShort seed = pick seed subsForTooShort
replaceName _ Pie _ = "Pie"

pick :: R.StdGen -> [a] -> a
pick seed xs 
    | length xs == 1  = xs!!0
    | otherwise       = x where
        x = xs!!fst (R.randomR (0, (length xs) -1) seed)
    

{- If the validation of the player's name says it should be changed, this function lets them know about it. -}
outputNameIfChanged :: T.Text -> PlayerNameValidationResult -> IO ()
outputNameIfChanged newName AllGood = return ()
outputNameIfChanged newName TooLong = TH.putTxtLn (T.replace "{name}" newName "That's a really long name, you know. I will call you {name}.")
outputNameIfChanged newName TooShort = TH.putTxtLn (T.replace "{name}" newName "Short and sweet, aye? From now on, you will be known as {name}.")
outputNameIfChanged newName Pie = TH.putTxtLn (T.replace "{name}" newName "How interesting! Mind if I call you {name}?")

