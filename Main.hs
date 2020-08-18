{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified System.Exit

putTxtLn :: T.Text -> IO ()
putTxtLn input = putStrLn $ T.unpack input

data Direction
    = Q
    | W
    | A
    | S
    | D
    | Y
    | N
    deriving (Show, Eq)

go :: Char -> Direction
go 'w' = W
go 'a' = A
go 's' = S
go 'd' = D
go 'y' = Y
go 'n' = N
go  _  = Q


welcomeMsg = "Welcome to Masquee's Ice Cream. What is your name?"
showWelcome = putTxtLn welcomeMsg

open002 :: T.Text -> T.Text
open002 name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "Hello, {name}. You're in a dark hallway."

showDarkHallway name = putTxtLn $ open002 name

goMsg = "Which way to go? Forward: press w. Left: a. Right: d. Give up and quit: q."
askDirection = putTxtLn goMsg

confirmMsg = "Yes: press y. No: press n. Give up and quit: q."
askConfirmation = putTxtLn confirmMsg

storage001 :: T.Text -> T.Text
storage001 name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "You are in a storage room, {name}. It is empty."
showStorage name = putTxtLn $ storage001 name

lightAppears001 :: T.Text -> T.Text
lightAppears001 name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize."
showLightAppears name = putTxtLn $ lightAppears001 name

hallwayDeath001 :: T.Text -> T.Text
hallwayDeath001 name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It's an ice cream cone. He turns around and sees you. You tear him to shreds so fast he can't even scream. Now the ice cream image is dripping with blood."
showTheHallwayDeath name = putTxtLn $ hallwayDeath001 name

theParlor001 :: T.Text -> T.Text
theParlor001 name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon."
showTheParlor name = putTxtLn $ theParlor001 name

quitMsg :: T.Text -> T.Text
quitMsg name = T.replace needle name haystack
    where needle = "{name}"
          haystack = "Come back to play another day, {name}? Bye!"
quit name = putTxtLn $ quitMsg name

data Stage = 
  Init
  | Waiting
  | A1DarkHallway
  | A1aStorage
  | A1wLightAppears
  | A1dTheParlor
  | A2HallwayDeath
  | Quit
  deriving (Show, Eq)

next :: Stage -> Direction -> Stage
next _ Q = Quit
next Waiting _ = Waiting
next Init _ = A1DarkHallway
next A1DarkHallway A = A1aStorage
next A1DarkHallway D = A1dTheParlor
next A1DarkHallway _ = A1wLightAppears
next A1wLightAppears A = A1aStorage
next A1wLightAppears D = A1dTheParlor
next A1wLightAppears _ = A2HallwayDeath
next A1aStorage _ = A1wLightAppears
next A1dTheParlor _ = A1wLightAppears

showStage :: Stage -> T.Text -> IO ()
showStage Init _ = showWelcome
showStage Quit name = quit name
showStage A1DarkHallway name = showDarkHallway name
showStage A1aStorage name = showStorage name
showStage A1wLightAppears name = showLightAppears name
showStage A1dTheParlor name = showTheParlor name
showStage A2HallwayDeath name = showTheHallwayDeath name

loop :: Stage -> T.Text -> IO ()
loop stage input = do 
     showStage stage input
     if stage == Quit
         then System.Exit.exitSuccess
         else do
             askDirection
             choice <- getChar
             putStrLn ""
             loop (next stage (go choice)) input


main :: IO ()
main = do
    showStage Init ""
    nameStr <- getLine
    loop A1DarkHallway (T.pack nameStr)
    
