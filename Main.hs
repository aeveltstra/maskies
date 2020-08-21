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
    | H
    deriving (Show, Eq)

go :: Char -> Direction
go 'w' = W
go 'a' = A
go 's' = S
go 'd' = D
go 'y' = Y
go 'n' = N
go 'h' = H
go  _  = Q


welcomeMsg = "Welcome to Maskie's Ice Cream. What is your name?"
showWelcome = putTxtLn welcomeMsg

a1DarkHallway :: T.Text -> T.Text
a1DarkHallway name = T.replace "{name}" name "Hello, {name}. You're in a dark hallway. It is night. Need help? Press h. To go forward: press w. To give up and quit: press q."
showA1DarkHallway name = putTxtLn $ a1DarkHallway name

a1HelpMsg _  = "This is a text adventure game. It requires you to read a lot. After each scene, the game gives you a set of choices for what to do next. Enter your choice to continue, or q to quit. Take your time. Take as long as you need. To go back, press w."
showA1Help name = putTxtLn $ a1HelpMsg name

a1LightAppears :: T.Text -> T.Text
a1LightAppears name = T.replace "{name}" name "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize. To go forward: press w. To give up and quit: q."
showA1LightAppears name = putTxtLn $ a1LightAppears name

a1HallwayDeath :: T.Text -> T.Text
a1HallwayDeath name = T.replace "{name}" name "The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It's an ice cream cone. He turns around and sees you. You tear him to shreds so fast he can't even scream. Now the ice cream image is dripping with blood. Press w."
showA1HallwayDeath name = putTxtLn $ a1HallwayDeath name

a2DarkHallway :: T.Text -> T.Text
a2DarkHallway name = T.replace "{name}" name "Still here, {name}? You're back in the dark hallway. It is night. Again. This is night 2. Need help? Press h. To go forward: press w. Press a to turn left. To turn right, press d."
showA2DarkHallway name = putTxtLn $ a2DarkHallway name

a2HelpMsg :: T.Text -> T.Text
a2HelpMsg name = T.replace "{name}" name "Maskies sells ice cream. Due to a pandemic killing lots of people, the owner replaced all human employees with animatronics. They're like robots, but better. No people touch the ice cream, so no viruses get transmitted. That makes Maskies a popular place. So popular that thieves like to come and steal the ice cream. You are a security guard, {name}. Your job is to keep the place safe at night. Press w to continue."
showA2Help name = putTxtLn $ a2HelpMsg name

a2Storage :: T.Text -> T.Text
a2Storage name = T.replace "{name}" name "You are in a storage room. There is a closet, a desk, and a water fountain. To look in the closet, press a. To look at the desk, press d. To drink from the fountain, press w. To go back to the hallway, press s."
showA2Storage name = putTxtLn $ a2Storage name

a2StorageDesk name = T.replace "{name}" name "There's a letter on the desk, with your name on it, {name}. Would you like to read it? If so, press y. If not, press n."
showA2StorageDesk name = putTxtLn $ a2StorageDesk name

a2ReadLetter :: T.Text -> T.Text
a2ReadLetter name = T.replace "{name}" name "Dear {name}, if you read this, I will be dead. Chances are you will die too. Something wanders the hallways at night. Did John Masky tell you that you need to keep out thieves? He lied to you. It isn't thieves that kill us. It's something much worse. But don't take my word for it. Check it out yourself. Maybe you can find a way to stop it. Good luck."
showA2ReadLetter name = putTxtLn $ a2ReadLetter name

a2TheParlor :: T.Text -> T.Text
a2TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon. There's an ice cream dispenser behind the counter. You want that ice cream, don't you? Press y if you just can't help yourself. Got some self-control? Press n to resist."
showA2TheParlor name = putTxtLn $ a2TheParlor name

a2HallwayEnd :: T.Text -> T.Text
a2HallwayEnd name = T.replace "{name}" name "On the far wall at the end of the hallway, you see a painting of an ice cream cone. Everything is squeaky clean. The cleaning crew must be doing a good job. Surely that's needed to keep the animatronics in good shape. On top of the ice cream in the painting, you spot a red ball with a yellow smiley face. Like a gumball. Wouldn't you like to eat some ice cream now? Press y for yes, n for no."
showA2HallwayEnd name = putTxtLn $ a2HallwayEnd name

a2EatIcecream :: T.Text -> T.Text
a2EatIcecream name = T.replace "{name}" name "This truly is the best ice cream ever. It's clear why people would want to come here and have some. The ice cream tastes so good, that you don't wonder why the dispenser hadn't been turned off. It's midnight, after all. Shouldn't it have been cleaned for hygiene? If you agree, press y. If not, press n."
showA2EatIcecream name = putTxtLn $ a2EatIcecream name

quitMsg :: T.Text -> T.Text
quitMsg name = T.replace "{name}" name "Come back to play another day, {name}? Bye!"
quit name = putTxtLn $ quitMsg name

data Stage = 
  Init
  | A1Help
  | A1DarkHallway
  | A1LightAppears
  | A1HallwayDeath
  | A2DarkHallway
  | A2Help
  | A2Storage
  | A2StorageDesk
  | A2ReadLetter
  | A2StorageCloset
  | A2StorageFountain
  | A2TheParlor
  | A2HallwayEnd
  | A2EatIcecream
  | Quit
  deriving (Show, Eq)

next :: Stage -> Direction -> Stage
next _ Q = Quit
next Init _ = A1DarkHallway
next A1DarkHallway H = A1Help
next A1Help _ = A1LightAppears
next A1DarkHallway _ = A1LightAppears
next A1LightAppears _ = A1HallwayDeath
next A1HallwayDeath _ = A2DarkHallway
next A2DarkHallway H = A2Help
next A2DarkHallway A = A2Storage
next A2Storage D = A2StorageDesk
next A2Storage A = A2StorageCloset
next A2Storage W = A2StorageFountain
next A2Storage _ = A2DarkHallway
next A2StorageDesk Y = A2ReadLetter
next A2StorageDesk _ = A2Storage
next A2DarkHallway D = A2TheParlor
next A2DarkHallway W = A2HallwayEnd
next A2Help _ = A2HallwayEnd
next A2ReadLetter _ = A2HallwayEnd
next A2HallwayEnd Y = A2TheParlor
next A2HallwayEnd _ = A2DarkHallway
next A2TheParlor N = A2DarkHallway
next A2TheParlor Y = A2EatIcecream


showStage :: Stage -> T.Text -> IO ()
showStage Init _ = showWelcome
showStage Quit name = quit name
showStage A1DarkHallway name = showA1DarkHallway name
showStage A1Help name = showA1Help name
showStage A1LightAppears name = showA1LightAppears name
showStage A1HallwayDeath name = showA1HallwayDeath name
showStage A2DarkHallway name = showA2DarkHallway name
showStage A2Help name = showA2Help name
showStage A2Storage name = showA2Storage name
showStage A2TheParlor name = showA2TheParlor name
showStage A2ReadLetter name = showA2ReadLetter name
showStage A2HallwayEnd name = showA2HallwayEnd name
showStage A2EatIcecream name = showA2EatIcecream name
showStage A2StorageDesk name = showA2StorageDesk name
showStage A2StorageCloset name = showA2StorageCloset name
showStage A2StorageFountain name = showA2StorageFountain name


loop :: Stage -> T.Text -> IO ()
loop stage input = do 
     showStage stage input
     if stage == Quit
         then System.Exit.exitSuccess
         else do
             choice <- getChar
             putStrLn ""
             loop (next stage (go choice)) input


main :: IO ()
main = do
    showStage Init ""
    nameStr <- getLine
    loop A1DarkHallway (T.pack nameStr)
    
