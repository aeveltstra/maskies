{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified System.Exit

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


welcomeMsg = "Welcome to Maskie's Ice Cream. What is your name?"
showWelcome = putTxtLn welcomeMsg

a1DarkHallway :: T.Text -> T.Text
a1DarkHallway name = T.replace "{name}" name "Hello, {name}. You're in a dark hallway. It is night. Need help? Press h. To go forward: press w. To give up and quit: press q."
showA1DarkHallway name = putTxtLn $ a1DarkHallway name

a1HelpMsg _  = "This is a text adventure game. It makes you read a lot. After each scene, you get a choice for what to do next. Enter your choice to continue, or q to quit. Take your time. Take as long as you need. No really: think it through. To go back, press w."
showA1Help name = putTxtLn $ a1HelpMsg name

a1LightAppears :: T.Text -> T.Text
a1LightAppears name = T.replace "{name}" name "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize. To go forward: press w. To give up and quit: q."
showA1LightAppears name = putTxtLn $ a1LightAppears name

a1HallwayDeath :: T.Text -> T.Text
a1HallwayDeath name = T.replace "{name}" name "The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It's an ice cream cone. He turns around and sees you. You tear him to shreds so fast he can't even scream. Now the ice cream image is dripping with blood. That will take some time to clean. Better get to it before the parlor opens again. Press w."
showA1HallwayDeath name = putTxtLn $ a1HallwayDeath name

a2DarkHallway :: T.Text -> T.Text
a2DarkHallway name = T.replace "{name}" name "Still here, {name}? You're back in the dark hallway. It is night. Again. This is night 2. Need help? Press h. To go forward: press w. Press a to turn left. There's a light there. To turn right, press d."
showA2DarkHallway name = putTxtLn $ a2DarkHallway name

a2HelpMsg :: T.Text -> T.Text
a2HelpMsg name = T.replace "{name}" name "Maskies sells ice cream. Due to a pandemic killing lots of people, the owner replaced all human employees with animatronics. They're like robots, but better. No people touch the ice cream, so no viruses get transmitted. That makes Maskies a popular place. So popular that thieves like to come and steal the ice cream. You are a security guard, {name}. Your job is to keep out the thieves. Press w to continue."
showA2Help name = putTxtLn $ a2HelpMsg name

a2Storage :: T.Text -> T.Text
a2Storage name = T.replace "{name}" name "You are in a lit storage room. There is a desk, a water fountain, and a closet. Is it hot in here? Press w to drink water from the fountain. Press d to look at the desk. To inspect in the closet, press a. To go back to the hallway, press s."
showA2Storage name = putTxtLn $ a2Storage name

a2StorageDesk name = T.replace "{name}" name "The desk holds a lantern. It's the only light in the room. You probably should pick it up and carry it around as you roam the premises. You know: so you don't stumble over anything in the dark. To pick up the lantern, press y. Otherwise, press n."
showA2StorageDesk name = putTxtLn $ a2StorageDesk name

a3StorageDesk name = T.replace "{name}" name "There's a letter on the desk, with your name on it, {name}. Would you like to read it? If so, press y. If not, press n."
showA3StorageDesk name = putTxtLn $ a3StorageDesk name

a3ReadLetter :: T.Text -> T.Text
a3ReadLetter name = T.replace "{name}" name "The letter reads: \"Dear {name}, if you read this, I will be dead. Chances are you will die too. Something wanders the hallways at night. Did John Masky tell you that you need to keep out thieves? He lied to you. It isn't thieves that kill us. It's something much worse. But don't take my word for it. Check it out yourself. Maybe you can find a way to stop it. Good luck.\" Press s to go back and keep playing. Scared? Give up and quit! Press q."
showA3ReadLetter name = putTxtLn $ a3ReadLetter name

a2TheParlor :: T.Text -> T.Text
a2TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon. And what's that? In the shadows behind the counter? Is that a person? To do your duty and inspect the situation, press w. However, you might be a coward. Press s to go back to the hallway. It won't be any better, though. Now choose."
showA2TheParlor name = putTxtLn $ a2TheParlor name

a2TheParlorDeath :: T.Text -> T.Text
a2TheParlorDeath name = T.replace "{name}" name "You get attacked. You can't see by whom or what: there is not enough light here. Wasn't there a lantern in the storage room? You really could have used that now. The attacker grabs you by the neck, and pulls you really close to his face. ITs face. It's an animatronic. Why is it attacking you? Why is it activated? It's night time! Shouldn't they be off? Good questions. But you have no time to reflect: the animatronic cracked your head like an eggshell. And now you can't even see how it pulled out your brain and eats it. That's going to make a mess! Now choose: to reincarnate and try again, press a. To give up and quit, press q."
showA2TheParlorDeath name = putTxtLn $ a2TheParlorDeath name

a2DarkHallwayEnd :: T.Text -> T.Text
a2DarkHallwayEnd name = T.replace "{name}" name "On the far end of the hallway, you see a wall. There might be a painting there, but it's too dark to make out. As a matter of fact, you can hardly see anything. Better go back, {name}. Press s to go back. Or press d to go right. Want to go left? Press a. To give up and quit, press q."
showA2DarkHallwayEnd name = putTxtLn $ a2DarkHallwayEnd name

a2HallwayDeath :: T.Text -> T.Text
a2HallwayDeath name = T.replace "{name}" name "What's that? In the shadows? It's moving fast. It's coming for you, {name}! You try and get away, but it's dark. It jumps on you. You fall over backwards, cracking your head on the floor. The squeaky clean floor. Not so clean anymore. You have nothing to protect yourself. The attacker weighs down on your chest. You can't breathe. You can't see anything anymore. Desparately you grasp for air and try to push away your attacker. To no avail. Good bye, {name}. Press a to reincarnate and try again. Press q to give up and quit."
showA2HallwayDeath name = putTxtLn $ a2HallwayDeath name

a3LitHallwayEnd :: T.Text -> T.Text
a3LitHallwayEnd name = T.replace "{name}" name "On the far wall at the end of the hallway, you see a painting of an ice cream cone. Everything is squeaky clean. The cleaning crew must be doing a good job. Surely that's needed to keep the animatronics in good shape. On top of the ice cream in the painting, you spot a red ball with a yellow smiley face. Like a gumball. Wouldn't you like to eat some ice cream now? Press y for yes, n for no."
showA3LitHallwayEnd name = putTxtLn $ a3LitHallwayEnd name

a3TheParlor :: T.Text -> T.Text
a3TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off, but your lantern spreads the light. You can make out tables and cabinets. And what's that? In the shadows behind the counter? Is that an ice cream dispenser? Let's have some! Surely that will cool you down. Press y to eat the ice cream. Press s to return to the hallway. To quit, press q."
showA3TheParlor name = putTxtLn $ a3TheParlor name

a3EatIcecream :: T.Text -> T.Text
a3EatIcecream name = T.replace "{name}" name "This truly is the best ice cream ever. It's clear why people would want to come here and have some. The ice cream tastes so good, that you forgot to wonder why the dispenser hadn't been turned off. It's midnight, after all. Shouldn't it have been cleaned for hygiene? If you agree, press y. If not, press n."
showA3EatIcecream name = putTxtLn $ a3EatIcecream name

a2StorageCloset name = T.replace "{name}" name "The closet is really dark. You can't see anything. Wouldn't you feel better with a lantern? Didn't you see one on the desk? To check the desk, press d. To rummage around the closet anyway, press w. To give up and quit, press q."
showA2StorageCloset name = putTxtLn $ a2StorageCloset name

a2StorageClosetDeath :: T.Text -> T.Text
a2StorageClosetDeath name = T.replace "{name}" name "Ack! Something just grabbed you! You can't see what it is: it's too dark! It jumps on you. You fall over backwards, cracking your head on the edge of the desk. The desk with the lantern. The light makes your attacker visible. It's... an animatronic!? Why is it attacking you? And why is it singing? This is not the right time for the ice cream song! It's shredding your clothes and your skin. You have nothing to protect yourself. You succumb to pain and blood loss. Good try, {name}. Don't give up now. Press a to reincarnate and try again. Press q to give up and quit."
showA2StorageClosetDeath name = putTxtLn $ a2StorageClosetDeath name

a3StorageCloset name = T.replace "{name}" name "The closet contains cleaning supplies and 2 sets of clothes: a spare security guard uniform, and a cleaner's outfit. Would you like to inspect the cleaning supplies? If so, press y. If not, press n."
showA3StorageCloset name = putTxtLn $ a3StorageCloset name

a3InspectSupplies name = T.replace "{name}" name "A broom... a mop... a bucket with rags... You shouldn't need these, {name}. You're a security guard. That's odd: something shiny is sticking out of the rags. Would a security guard ignore that? To ignore it and close the closet, press s. Otherwise, pick where to rummage: a, w, or d:"
showA3InspectSupplies name = putTxtLn $ a3InspectSupplies name

a3FindShield name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop on the front. But maybe it will come in handy, later. So you keep the shield. Press s to close the closet and return to the storage room."
showA3FindShield name = putTxtLn $ a3FindShield name

a2StorageFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. It is getting hot in here! There's a mirror on the wall in front of you. To lean forward and look into the mirror, press w. To return to the storage room, press s."
showA2StorageFountain name = putTxtLn $ a2StorageFountain name

a2ViewMirror name = T.replace "{name}" name "You see yourself, {name}. But it's dark and your face shows many shadows and wrinkles. How do you feel? Tired? Hot? The cold water helped a bit. You probably need to come back here to stay cool. Why is it hot in here anyway? It's an ice cream parlor. Maybe the heat helps sell more ice cream? Beads of sweat drip down the side of your face, onto your shirt. You wipe them off. Enough self-reflection. Let's get back to work. Press s. Or, if you prefer to give up and quit, press q."
showA2ViewMirror name = putTxtLn $ a2ViewMirror name

a3Storage name = T.replace "{name}" name "You are in a storage room. Your lantern lights it up. There is a desk, a water fountain, and a closet. Press w to cool down with some cold water from the fountain. Press d to check the desk. To inspect in the closet, press a. To go back to the hallway, press s."
showA3Storage name = putTxtLn $ a3Storage name

a3StorageFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. The heat is getting unbearable! There's a mirror on the wall in front of you. Let's check your hair. Got to look the part, after all. To lean forward and look into the mirror, press w. To return to the storage room, press s."
showA3StorageFountain name = putTxtLn $ a3StorageFountain name

a3ViewMirror name = T.replace "{name}" name "You see yourself, {name}. The light from the lantern does not do you any favors. You straighten your hair. And spot something shoot by from the corner of your eye. What do you do? Chase it? Press w. Stay in the room and keep looking for other things that may help? Press s."
showA3ViewMirror name = putTxtLn $ a3ViewMirror name

a3HallwayDeath name = T.replace "{name}" name "Oh how courageous! You ran after whatever it was, into the hallway. You can see it clearly now: it's an animatronic! It rides around on a wheel. And it's coming for you really fast! If only you had had something to protect yourself! But you don't. Did you not see the shield in the storage room closet? Did you not take it? Too late! The animatronic just ran you over. You notice how heavy it is. Way heavier than a human. You hear your bones crushing and breaking as the animatronic spins in circles on top of you. You want to scream, but the pain is too much. You faint. And then you die. Where does the animatronic go? Why did it attack you? No clue. Want to find out? Press a to reincarnate and try again. Or run away and quit, by pressing q."
showA3HallwayDeath name = putTxtLn $ a3HallwayDeath name

a4Storage name = T.replace "{name}" name "Yet to add. Press q to quit."
showA4Storage name = putTxtLn $ a4Storage name

a5Storage name = T.replace "{name}" name "Yet to add. Press q to quit."
showA5Storage name = putTxtLn $ a5Storage name


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
next A3HallwayDeath A = A3Hallway
next A3HallwayDeath _ = Quit
next _ _ = error "Not yet wired up."


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
showStage A2TheParlorDeath name = showA2TheParlorDeath name
showStage A2DarkHallwayEnd name = showA2DarkHallwayEnd name
showStage A2HallwayDeath name = showA2HallwayDeath name
showStage A2StorageDesk name = showA2StorageDesk name
showStage A2StorageCloset name = showA2StorageCloset name
showStage A2StorageClosetDeath name = showA2StorageClosetDeath name
showStage A2StorageFountain name = showA2StorageFountain name
showStage A2ViewMirror name = showA2ViewMirror name
showStage A3Storage name = showA3Storage name
showStage A3StorageCloset name = showA3StorageCloset name
showStage A3InspectSupplies name = showA3InspectSupplies name
showStage A3FindShield name = showA3FindShield name
showStage A3StorageDesk name = showA3StorageDesk name
showStage A3ReadLetter name = showA3ReadLetter name
showStage A3StorageFountain name = showA3StorageFountain name
showStage A3ViewMirror name = showA3ViewMirror name
showStage A3EatIcecream name = showA3EatIcecream name
showStage A3HallwayDeath name = showA3HallwayDeath name
showStage A4Storage name = showA4Storage name
showStage A5Storage name = showA5Storage name


loop :: Stage -> T.Text -> IO ()
loop stage input = do 
     showStage stage input
     if stage == Quit
         then System.Exit.exitSuccess
         else do
             choice <- getChar
             putStrLn ""
             loop (next stage (key choice)) input


main :: IO ()
main = do
    showStage Init ""
    nameStr <- getLine
    loop A1DarkHallway (T.pack nameStr)
    
