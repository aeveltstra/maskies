{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.906.1136
-}
module Main where

import Prelude
import qualified Data.Char
import qualified Data.Text as T
import qualified System.IO
import qualified System.Random as R
import qualified TextHelper as TH
import qualified Keys as K
import qualified Stages as S
import qualified NameValidation as NV

{- This is the game loop. It keeps looping stages until the player quits. It assumes that the stage will give the player options for keys to press to have actions performed. It then asks the player for their input, uses that to figure out which stage to show next, and recurses in on itself. -}
loop :: S.Stage -> T.Text -> IO ()
loop S.Quit player = TH.putTxtLn $ S.stage S.Quit player
loop theStage player = do 
    TH.putTxtLn $ S.stage theStage player
    choice <- getChar
    putStrLn ""
    loop (S.next theStage (K.key choice)) player

{- Sanitizes and validated the player's name. If needed, reprimands the player for choosing a foolish name, and announces a substitute. Returns either the sanitized input or the substitute. -}
checkPlayerName :: R.StdGen -> String -> IO T.Text
checkPlayerName randomizerSeed taintedName = do
    let unvalidated = T.strip $ T.filter Data.Char.isPrint $ T.pack taintedName
    let result = NV.validatePlayerName unvalidated
    let player = NV.replaceName unvalidated result randomizerSeed
    NV.outputNameIfChanged player result 
    return player

{- This is the entry point of the application. It shows the Init stage, which asks the player to enter their name. Then it captures that name and calls it 'player'. Using that, it kicks off the game loop. -}
main :: IO ()
main = do
    TH.putTxtLn $ S.stage S.Init ""
    taintedName <- getLine
    randomizerSeed <- R.newStdGen
    player <- checkPlayerName randomizerSeed taintedName

    -- This turns off input buffering. 
    -- Buffering makes it so that the executable will 
    -- wait for an enter key press after any input.
    -- Our application wouild see the enter key and 
    -- act on it, too, acting twice on the same stage. 
    -- By turning off buffering, that is avoided.
    System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
    loop S.A1DarkHallway player

