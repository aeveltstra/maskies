{-# LANGUAGE CPP, OverloadedStrings #-}

{- | Maskies is a text adventure horror survival game.
     Try not to die.
     @author A.E.Veltstra
     @copyright A.E.Veltstra & T.R.Veltstra
     @version 2.23.821.1649
-}
module Main where
import qualified Data.Char
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Keys
import qualified NameValidation as NV
import qualified TextWrapper as TW

import Prelude
import qualified Stages
import qualified System.Console.ANSI
import qualified System.IO
import qualified System.Random as R

{- | Abbreviation to reduce verbosity.
 -   This function clears the terminal screen,
 -   and sets the cursor to the top left corner,
 -   so that the text of the next stage is shown
 -   at the top left corner rather than at the
 -   bottom left corner, which is the default.
 -   Not the history - merely the latest output.
 -}
cls :: IO ()
cls = do
  System.Console.ANSI.setCursorPosition 0 0
  System.Console.ANSI.clearScreen

{- | This is the game loop. It keeps looping stages until the 
 -   player quits. It assumes that the stage will give the player 
 -   options for keys to press to have actions performed. It then 
 -   asks the player for their input, uses that to figure out 
 -   which stage to show next, and recurses in on itself. 
 -}
loop :: Stages.Stage -> Stages.UserName -> IO ()
loop Stages.Quit player = do
  cls
  TW.wrap $ Stages.show Stages.Quit player
#if defined(mingw32_HOST_OS)
loop stage player = do
  cls
  let t = Stages.show stage player
  TW.wrap t
  TLIO.getLine >> = 
    \k -> if 0 == TL.length k 
            then loop (Stages.next stage (Keys.read '-')) player
            else loop (Stages.next stage (Keys.read ((TL.unpack k)!!0))) player
#else
loop stage player = do
  cls
  let t = Stages.show stage player
  TW.wrap t
  getChar >>= \k -> loop (Stages.next stage (Keys.read k)) player
#endif

{- | Sanitizes and validates the player's name. If needed, 
 -   reprimands the player for choosing a foolish name, and 
 -   announces a substitute. Returns either the sanitized 
 -   input or the substitute. 
 -}
checkPlayerName :: R.StdGen -> Stages.UserName -> IO Stages.UserName
checkPlayerName randomizerSeed taintedName = do 
    let unvalidated = TL.strip $ TL.filter Data.Char.isPrint taintedName
        result = NV.validate unvalidated
        player = NV.replace unvalidated result randomizerSeed
    NV.show result player 
    return player

{- | Turns off input buffering for non-Windows OS-es. 
 -   Buffering makes it so that the executable will 
 -   wait for an enter key press after any input.
 -   Our application wouild see the enter key and 
 -   act on it, too, acting twice on the same stage. 
 -   By turning off buffering, that is avoided.
 -}
disableBuffering :: IO ()
#if defined(mingw32_HOST_OS)
disableBuffering = cls >> System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
#else
disableBuffering = cls >> System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
#endif

{- | Entry point of the application. It shows the Init stage, 
 -   which asks the player to enter their name. It captures and 
 -   validates that name, and uses the result to kick off the 
 -   game loop. 
 -}
main :: IO ()
main
  = cls >> (TW.wrap $ Stages.show Stages.Init "") >> TLIO.getLine >>=
      \ p -> disableBuffering >> R.newStdGen >>=
        \ s -> checkPlayerName s p >>= loop Stages.A1DarkHallway

