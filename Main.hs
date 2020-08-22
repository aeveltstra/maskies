{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Char as D
import qualified Data.Text as T
import qualified System.Exit
import qualified System.IO

putTxtLn :: T.Text -> IO ()
putTxtLn input = putStrLn $ T.unpack input

data Key
    = Q
    | W
    | A
    | S
    | D
    | Y
    | N
    | H
    | Wait
    deriving (Show, Eq)

key :: Char -> Key
key 'w' = W
key 'a' = A
key 's' = S
key 'd' = D
key 'y' = Y
key 'n' = N
key 'h' = H
key 'q' = Q
key  _  = Wait

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
  | A2StorageCloset
  | A2StorageClosetDeath
  | A2StorageFountain
  | A2ViewMirror
  | A2TheParlor
  | A2TheParlorDeath
  | A2DarkHallwayEnd
  | A2HallwayDeath
  | A3Storage
  | A3StorageFountain
  | A3ViewMirror
  | A3StorageDesk
  | A3ReadLetter
  | A3StorageCloset
  | A3InspectSupplies
  | A3FindShield
  | A3LitHallway
  | A3LitHallwayEnd
  | A3HallwayDeath
  | A3TheParlor
  | A3EatIcecream
  | A4Storage
  | A5Storage
  | Quit
  deriving (Show, Eq)

next :: Stage -> Key -> Stage
next stage Wait = stage
next _ Q = Quit
next Init _ = A1DarkHallway
next A1DarkHallway H = A1Help
next A1Help _ = A1LightAppears
next A1DarkHallway _ = A1LightAppears
next A1LightAppears _ = A1HallwayDeath
next A1HallwayDeath _ = A2DarkHallway
next A2DarkHallway H = A2Help
next A2DarkHallway A = A2Storage
next A2DarkHallway W = A2DarkHallwayEnd
next A2Help _ = A2DarkHallway
next A2Storage D = A2StorageDesk
next A2Storage A = A2StorageCloset
next A2Storage W = A2StorageFountain
next A2Storage _ = A2DarkHallway
next A2StorageDesk Y = A3Storage
next A2StorageDesk _ = A2Storage
next A2StorageCloset D = A2StorageDesk
next A2StorageCloset W = A2StorageClosetDeath
next A2StorageClosetDeath A = A2Storage
next A2StorageClosetDeath _ = Quit
next A2StorageCloset _ = A2Storage
next A2StorageFountain W = A2ViewMirror
next A2StorageFountain _ = A2Storage
next A2ViewMirror _ = A2Storage
next A2DarkHallwayEnd D = A2TheParlor
next A2DarkHallwayEnd S = A2DarkHallway
next A2DarkHallwayEnd _ = A2HallwayDeath
next A2HallwayDeath A = A2DarkHallway
next A2DarkHallway D = A2TheParlor
next A2HallwayDeath _ = Quit
next A2TheParlor W = A2TheParlorDeath
next A2TheParlor _ = A2DarkHallway
next A2TheParlorDeath A = A2DarkHallway
next A2TheParlorDeath _ = Quit
next A3LitHallway A = A3Storage
next A3LitHallway W = A3LitHallwayEnd
next A3LitHallway D = A3TheParlor
next A3LitHallway _ = A3LitHallway
next A3Storage S = A3LitHallway
next A3StorageCloset Y = A3InspectSupplies
next A3StorageCloset _ = A3Storage
next A3InspectSupplies S = A4Storage
next A3InspectSupplies _ = A3FindShield
next A3FindShield _ = A5Storage
next A3Storage D = A3StorageDesk
next A3Storage A = A3StorageCloset
next A3Storage W = A3StorageFountain
next A3StorageFountain W = A3ViewMirror
next A3StorageFountain _  = A3Storage
next A3StorageDesk Y = A3ReadLetter
next A3ReadLetter _ = A3Storage
next A3ViewMirror S = A3Storage
next A3ViewMirror W = A3HallwayDeath
next A3HallwayDeath A = A3LitHallway
next A3HallwayDeath _ = Quit
next A3LitHallwayEnd Y = A3TheParlor
next A3LitHallwayEnd _ = A3LitHallway
next A3TheParlor Y = A3EatIcecream
next A3TheParlor _ = A3LitHallway
next _ _ = error "Yet to wire up."

stage :: Stage -> T.Text -> IO ()

stage Init _ = putTxtLn "Welcome to Maskie's Ice Cream. What is your name? Type your name and press Enter:"

stage Quit name = quit name

stage A1DarkHallway name = putTxtLn $ T.replace "{name}" name "Hello, {name}. You're in a dark hallway. It is night. Need help? Press h. To go forward: press w. To give up and quit: press q."

stage A1Help _ = putTxtLn $ "This is a text adventure game. It makes you read a lot. After each scene, you get a choice for what to do next. Enter your choice to continue, or q to quit. Take your time. Take as long as you need. No really: think it through. To go back, press w."

stage A1LightAppears name = putTxtLn $ T.replace "{name}" name "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize. To go forward: press w. To give up and quit: q."

stage A1HallwayDeath name = putTxtLn $ T.replace "{name}" name "The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It's an ice cream cone. He turns around and sees you. You tear him to shreds so fast he can't even scream. Now the ice cream image is dripping with blood. That will take some time to clean. Better get to it before the parlor opens again. Press w."

stage A2DarkHallway name = putTxtLn $ T.replace "{name}" name "Still here, {name}? You're back in the dark hallway. It is night. Again. This is night 2. Need help? Press h. To go forward: press w. Press a to turn left. There's a light there. To turn right, press d."

stage A2Help name = putTxtLn $ T.replace "{name}" name "Maskies sells ice cream. Due to a pandemic killing lots of people, the owner replaced all human employees with animatronics. They're like robots, but better. No people touch the ice cream, so no viruses get transmitted. That makes Maskies a popular place. So popular that thieves like to come and steal the ice cream. You are a security guard, {name}. Your job is to keep out the thieves. Press w to continue."

stage A2Storage name = putTxtLn $ T.replace "{name}" name "You are in a lit storage room. There is a desk, a water fountain, and a closet. Is it hot in here? Press w to drink water from the fountain. Press d to look at the desk. To inspect in the closet, press a. To go back to the hallway, press s."

stage A2TheParlor name = putTxtLn $ T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon. And what's that? In the shadows behind the counter? Is that a person? To do your duty and inspect the situation, press w. However, you might be a coward. Press s to go back to the hallway. It won't be any better, though. Now choose."

stage A2TheParlorDeath name = putTxtLn $ T.replace "{name}" name "You get attacked. You can't see by whom or what: there is not enough light here. Wasn't there a lantern in the storage room? You really could have used that now. The attacker grabs you by the neck, and pulls you really close to his face. ITs face. It's an animatronic. Why is it attacking you? Why is it activated? It's night time! Shouldn't they be off? Good questions. But you have no time to reflect: the animatronic cracked your head like an eggshell. And now you can't even see how it pulled out your brain and eats it. That's going to make a mess! Now choose: to reincarnate and try again, press a. To give up and quit, press q."

stage A2DarkHallwayEnd name = putTxtLn $ T.replace "{name}" name "On the far end of the hallway, you see a wall. There might be a painting there, but it's too dark to make out. As a matter of fact, you can hardly see anything. Better go back, {name}. Press s to go back. Or press d to go right. Want to go left? Press a. To give up and quit, press q."

stage A2HallwayDeath name = putTxtLn $ T.replace "{name}" name "What's that? In the shadows? It's moving fast. It's coming for you, {name}! You try and get away, but it's dark. It jumps on you. You fall over backwards, cracking your head on the floor. The squeaky clean floor. Not so clean anymore. You have nothing to protect yourself. The attacker weighs down on your chest. You can't breathe. You can't see anything anymore. Desparately you grasp for air and try to push away your attacker. To no avail. Good bye, {name}. Press a to reincarnate and try again. Press q to give up and quit."

stage A2StorageDesk name = putTxtLn $ T.replace "{name}" name "The desk holds a lantern. It's the only light in the room. You probably should pick it up and carry it around as you roam the premises. You know: so you don't stumble over anything in the dark. To pick up the lantern, press y. Otherwise, press n."

stage A2StorageCloset name = putTxtLn $ T.replace "{name}" name "The closet is really dark. You can't see anything. Wouldn't you feel better with a lantern? Didn't you see one on the desk? To check the desk, press d. To rummage around the closet anyway, press w. To give up and quit, press q."

stage A2StorageClosetDeath name = putTxtLn $ T.replace "{name}" name "Ack! Something just grabbed you! You can't see what it is: it's too dark! It jumps on you. You fall over backwards, cracking your head on the edge of the desk. The desk with the lantern. The light makes your attacker visible. It's... an animatronic!? Why is it attacking you? And why is it singing? This is not the right time for the ice cream song! It's shredding your clothes and your skin. You have nothing to protect yourself. You succumb to pain and blood loss. Good try, {name}. Don't give up now. Press a to reincarnate and try again. Press q to give up and quit."

stage A2StorageFountain name = putTxtLn $ T.replace "{name}" name "Aah, fresh, clean water! You needed that. It is getting hot in here! There's a mirror on the wall in front of you. To lean forward and look into the mirror, press w. To return to the storage room, press s."

stage A2ViewMirror name = putTxtLn $ T.replace "{name}" name "You see yourself, {name}. But it's dark and your face shows many shadows and wrinkles. How do you feel? Tired? Hot? The cold water helped a bit. You probably need to come back here to stay cool. Why is it hot in here anyway? It's an ice cream parlor. Maybe the heat helps sell more ice cream? Beads of sweat drip down the side of your face, onto your shirt. You wipe them off. Enough self-reflection. Let's get back to work. Press s. Or, if you prefer to give up and quit, press q."

stage A3LitHallway name = putTxtLn $ T.replace "{name}" name "You're back in the hallway. It's still night 2. Your lantern helps you see. Where to? Forward: press w. Left: a. Right: d. You could give up now and press q. But you've come so far!"

stage A3LitHallwayEnd name = putTxtLn $ T.replace "{name}" name "On the far wall at the end of the hallway, you see a painting of an ice cream cone. Everything is squeaky clean. The cleaning crew must be doing a good job. Surely that's needed to keep the animatronics in good shape. On top of the ice cream in the painting, you spot a red ball with a yellow smiley face. Like a gumball. Wouldn't you like to eat some ice cream now? It is getting hot in here, after all. Press y for yes, n for no."

stage A3Storage name = putTxtLn $ T.replace "{name}" name "You are in a storage room. Your lantern lights it up. There is a desk, a water fountain, and a closet. Press w to cool down with some cold water from the fountain. Press d to check the desk. To inspect in the closet, press a. To go back to the hallway, press s."

stage A3StorageCloset name = putTxtLn $ T.replace "{name}" name "The closet contains cleaning supplies and 2 sets of clothes: a spare security guard uniform, and a cleaner's outfit. Would you like to inspect the cleaning supplies? If so, press y. If not, press n."

stage A3InspectSupplies name = putTxtLn $ T.replace "{name}" name "A broom... a mop... a bucket with rags... You shouldn't need these, {name}. You're a security guard. That's odd: something shiny is sticking out of the rags. Would a security guard ignore that? To ignore it and close the closet, press s. Otherwise, pick where to rummage: a, w, or d:"

stage A3FindShield name = putTxtLn $ T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop on the front. But maybe it will come in handy, later. So you keep the shield. Press s to close the closet and return to the storage room."

stage A3StorageDesk name = putTxtLn $ T.replace "{name}" name "There's a letter on the desk, with your name on it, {name}. Would you like to read it? If so, press y. If not, press n."

stage A3ReadLetter name = putTxtLn $ T.replace "{name}" name "The letter reads: \"Dear {name}, if you read this, I will be dead. Chances are you will die too. Something wanders the hallways at night. Did John Masky tell you that you need to keep out thieves? He lied to you. It isn't thieves that kill us. It's something much worse. But don't take my word for it. Check it out yourself. Maybe you can find a way to stop it. Good luck.\" Press s to go back and keep playing. Scared? Give up and quit! Press q."

stage A3StorageFountain name = putTxtLn $ T.replace "{name}" name "Aah, fresh, clean water! You needed that. The heat is getting unbearable! There's a mirror on the wall in front of you. Let's check your hair. Got to look the part, after all. To lean forward and look into the mirror, press w. To return to the storage room, press s."

stage A3ViewMirror name = putTxtLn $ T.replace "{name}" name "You see yourself, {name}. The light from the lantern does not do you any favors. You straighten your hair. And spot something shoot by from the corner of your eye. What do you do? Chase it? Press w. Stay in the room and keep looking for other things that may help? Press s."

stage A3TheParlor name = putTxtLn $ T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off, but your lantern spreads the light. You can make out tables and cabinets. And what's that? In the shadows behind the counter? Is that an ice cream dispenser? Let's have some! Surely that will cool you down. Press y to eat the ice cream. Press s to return to the hallway. To quit, press q."

stage A3EatIcecream name = putTxtLn $ T.replace "{name}" name "This truly is the best ice cream ever. It's clear why people would want to come here and have some. The ice cream tastes so good, that you forgot to wonder why the dispenser hadn't been turned off. It's midnight, after all. Shouldn't it have been cleaned for hygiene? If you agree, press y. If not, press n."

stage A3HallwayDeath name = putTxtLn $ T.replace "{name}" name "Oh how courageous! You ran after whatever it was, into the hallway. You can see it clearly now: it's an animatronic! It rides around on a wheel. And it's coming for you really fast! If only you had had something to protect yourself! But you don't. Did you not see the shield in the storage room closet? Did you not take it? Too late! The animatronic just ran you over. You notice how heavy it is. Way heavier than a human. You hear your bones crushing and breaking as the animatronic spins in circles on top of you. You want to scream, but the pain is too much. You faint. And then you die. Where does the animatronic go? Why did it attack you? No clue. Want to find out? Press a to reincarnate and try again. Or run away and quit, by pressing q."

stage A4Storage name = putTxtLn $ T.replace "{name}" name "Yet to add. Press q to quit."

stage A5Storage name = putTxtLn $ T.replace "{name}" name "Yet to add. Press q to quit."


quitMsg :: T.Text -> T.Text
quitMsg name = T.replace "{name}" name "Come back to play another day, {name}? Bye!"
quit name = putTxtLn $ quitMsg name

loop :: Stage -> T.Text -> IO ()
loop Quit input = do
    stage Quit input
    System.Exit.exitSuccess
loop theStage input = do 
    stage theStage input
    choice <- getChar
    putStrLn ""
    loop (next theStage (key choice)) input


main :: IO ()
main = do
    stage Init ""
    nameStr <- getLine
    --This turns off input buffering. Buffering makes it so that the
    --the executable will wait for an enter key press after any input.
    --Our application will see the enter key and act on it, 
    --acting twice on the same stage.
    System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
    loop A1DarkHallway (T.pack nameStr)
    
