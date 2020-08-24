{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified System.IO

-- Built-in Haskell function putStrLn operates on Strings.
-- Due to constraints in the String type and how Haskell 
-- deals with them, it was chosen to use Data.Text instead.
-- To output those to console, we have to unpack them.
-- To prevent having to do that every time, use this function.
putTxtLn :: T.Text -> IO ()
putTxtLn input = putStrLn $ T.unpack input


-- This is the game loop. It keeps looping stages until the
-- player quits. It assumes that the stage will give the player
-- options for keys to press to have actions performed. It then 
-- asks the player for their input, uses that to figure out
-- which stage to show next, and recurses in on itself.
loop :: Stage -> T.Text -> IO ()
loop Quit player = putTxtLn $ stage Quit player
loop theStage player = do 
    putTxtLn $ stage theStage player
    choice <- getChar
    putStrLn ""
    loop (next theStage (key choice)) player

-- This is the entry point of the application. It shows the 
-- Init stage, which asks the player to enter their name. 
-- Then it captures that name and calls it 'player'. Using that,
-- it kicks off the game loop.
main :: IO ()
main = do
    putTxtLn $ stage Init ""
    nameStr <- getLine
    let player = T.pack nameStr
    --This turns off input buffering. Buffering makes it so that the
    --the executable will wait for an enter key press after any input.
    --Our application wouild see the enter key and act on it, too, 
    --acting twice on the same stage. By turning off buffering, that 
    --is avoided.
    System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
    loop A1DarkHallway player
    

-- The game expects very specific key presses from the player.
-- Those are listed below. Consider them events. 
-- Note that 'Wait' is not a key press... but it is an event.
-- The Key events are used to determine which stage to show next.
-- The Wait Key event is used to make the same stage replay.
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

-- The application captures key presses. This function converts
-- those into Key events (see above). Unrecognized keys turn into
-- the Wait Key event. This is used to determine which stage
-- to show next: the Wait event makes the same stage to replay.
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

-- Let's use Haskell's type system to determine exactly which 
-- stages the game can have. If we forget one, the compiler 
-- will complain. Unfortunately it does not complain if we 
-- forget to wire up a stage in the function 'next'. But it 
-- does help when showing stages: the compiler automatically 
-- constrains us to only show stages that have been defined here.
data Stage = 
  Init
  | A1Help
  | A1DarkHallway
  | A1LightAppears
  | A1HallwayDeath
  | B1DarkHallway
  | B1Help
  | B1Storage
  | B1StorageDesk
  | B1StorageCloset
  | B1StorageClosetDeath
  | B1StorageFountain
  | B1ViewMirror
  | B1TheParlor
  | B1TheParlorDeath
  | B1DarkHallwayEnd
  | B1DarkToilets
  | B1DarkHallwayDeath
  | B2Storage
  | B2StorageFountain
  | B2ViewMirror
  | B2StorageDesk
  | B2ReadLetter
  | B2StorageCloset
  | B2InspectSupplies
  | B2aInspectSupplies
  | B2dInspectSupplies
  | B2wInspectSupplies
  | B2aFindShield
  | B2cFindShield
  | B2eFindShield
  | B2jFindShield
  | B2bFindMap
  | B2eFindMap
  | B2gFindMap
  | B2wFindMap
  | B2bFindMemoryStick
  | B2cFindMemoryStick
  | B2dFindMemoryStick
  | B2fFindMemoryStick
  | B2LitHallway
  | B2LitHallwayEnd
  | B2HallwayDeath
  | B2TheParlor
  | B2EatIcecream
  | B2FoodPoisoning
  | B2LitToilets
  | B2FeelBetterNow
  | B2DieFromFoodPoisoning
  | B3Storage
  | B3aStorage
  | B3dStorage
  | B3wStorage
  | B3adStorage
  | B3awStorage
  | B3dwStorage
  | B3adwStorage
  | B3StorageDesk
  | B3aStorageDesk
  | B3dStorageDesk
  | B3wStorageDesk
  | B3StorageFountain
  | B3PressedButton
  | B3StorageDeath
  | A5Storage
  | Quit
  deriving (Show, Eq)

-- This function wires Key events to stages, to determine which 
-- stage to show next. If the event is Wait, the same stage gets
-- returned as put in. If the event is Q, the function returns 
-- the Quit event. If the player enters a key that is not wired 
-- up to a stage here, the function throws an error.
next :: Stage -> Key -> Stage
next stage Wait = stage
next _ Q = Quit
next Init _ = A1DarkHallway
next A1DarkHallway H = A1Help
next A1Help _ = A1LightAppears
next A1DarkHallway _ = A1LightAppears
next A1LightAppears _ = A1HallwayDeath
next A1HallwayDeath _ = B1DarkHallway
next B1DarkHallway H = B1Help
next B1DarkHallway A = B1Storage
next B1DarkHallway W = B1DarkHallwayEnd
next B1Help _ = B1Storage
next B1Storage D = B1StorageDesk
next B1Storage A = B1StorageCloset
next B1Storage W = B1StorageFountain
next B1Storage _ = B1DarkHallway
next B1StorageDesk Y = B2Storage
next B1StorageDesk _ = B1Storage
next B1StorageCloset D = B1StorageDesk
next B1StorageCloset W = B1StorageClosetDeath
next B1StorageClosetDeath A = B1Storage
next B1StorageClosetDeath _ = Quit
next B1StorageCloset _ = B1Storage
next B1StorageFountain W = B1ViewMirror
next B1StorageFountain _ = B1Storage
next B1ViewMirror _ = B1Storage
next B1DarkHallwayEnd D = B1TheParlor
next B1DarkHallwayEnd S = B1DarkHallway
next B1DarkHallwayEnd A = B1DarkToilets
next B1DarkHallwayEnd _ = B1DarkHallwayDeath
next B1DarkHallwayDeath A = B1DarkHallway
next B1DarkToilets S = B1DarkHallway
next B1DarkToilets _ = B1DarkHallwayDeath
next B1DarkHallway D = B1TheParlor
next B1DarkHallwayDeath _ = Quit
next B1TheParlor W = B1TheParlorDeath
next B1TheParlor _ = B1DarkHallway
next B1TheParlorDeath A = B1DarkHallway
next B1TheParlorDeath _ = Quit
next B2LitHallway A = B2Storage
next B2LitHallway W = B2LitHallwayEnd
next B2LitHallway D = B2TheParlor
next B2LitHallway _ = B2LitHallway
next B2Storage S = B2LitHallway
next B2StorageCloset Y = B2InspectSupplies
next B2StorageCloset _ = B3Storage
next B2InspectSupplies A = B2aFindShield
next B2InspectSupplies W = B2wFindMap
next B2InspectSupplies D = B2dFindMemoryStick
next B2InspectSupplies _ = B3Storage
next B2aFindShield W = B2bFindMap
next B2aFindShield D = B2bFindMemoryStick
next B2aFindShield _ = B3aStorage
next B3aStorage A = B2aInspectSupplies
next B3aStorage _ = B3aStorageDesk
next B2wFindMap A = B2cFindShield
next B2wFindMap D = B2cFindMemoryStick
next B2wFindMap _ = B3wStorage
next B3wStorage W = B2wInspectSupplies
next B3wStorage _ = B3wStorageDesk
next B2dFindMemoryStick A = B2eFindShield
next B2dFindMemoryStick W = B2eFindMap
next B2dFindMemoryStick _ = B3dStorage
next B3dStorage D = B2dInspectSupplies
next B3dStorage _ = B3dStorageDesk
next B2bFindMap D = B2fFindMemoryStick
next B2bFindMap _ = B3awStorage
next B2bFindMemoryStick W = B2gFindMap
next B2bFindMemoryStick _ = B3adStorage
next B2cFindShield D = B2fFindMemoryStick
next B2cFindShield _ = B3awStorage
next B2cFindMemoryStick A = B2jFindShield
next B2cFindMemoryStick _ = B3dwStorage
next B2eFindShield W = B2gFindMap
next B2eFindShield _ = B3adStorage
next B2eFindMap A = B2jFindShield
next B2eFindMap _ = B3dwStorage
next B2fFindMemoryStick _ = B3adwStorage
next B2gFindMap _ = B3adwStorage
next B2jFindShield _ = B3adwStorage
next B2Storage D = B2StorageDesk
next B2Storage A = B2StorageCloset
next B2Storage W = B2StorageFountain
next B2StorageFountain W = B2ViewMirror
next B2StorageFountain _  = B2Storage
next B2StorageDesk Y = B2ReadLetter
next B2StorageDesk _ = B2Storage
next B2ReadLetter _ = B2Storage
next B2ViewMirror S = B2Storage
next B2ViewMirror W = B2HallwayDeath
next B2HallwayDeath A = B2LitHallway
next B2HallwayDeath _ = Quit
next B2LitHallwayEnd Y = B2TheParlor
next B2LitHallwayEnd _ = B2LitHallway
next B2TheParlor Y = B2EatIcecream
next B2EatIcecream N = B2FoodPoisoning
next B2FoodPoisoning W = B2LitToilets
next B2FoodPoisoning _ = B2DieFromFoodPoisoning
next B2LitToilets S = B2DieFromFoodPoisoning
next B2DieFromFoodPoisoning A = B2LitHallway
next B2DieFromFoodPoisoning _ = Quit
next B2LitToilets _ = B2FeelBetterNow
next B2FeelBetterNow _ = B2LitHallwayEnd
next B2EatIcecream _ = B2LitHallway
next B2TheParlor _ = B2LitHallway
next B3Storage D = B3StorageDesk
next B3Storage _ = B3StorageFountain
next B3StorageDesk Y = B3PressedButton
next B3StorageDesk _ = B3StorageFountain
next B3PressedButton Y = B3StorageFountain
next B3PressedButton _ = B3StorageDeath
next B3StorageFountain _ = B3StorageDeath
next B3StorageDeath _ = Quit

next _ _ = error "Yet to wire up."

-- These are the texts to show for each stage. This architecture
-- assumes that the game loop outputs these texts and captures 
-- input from the player, to return to an other stage.
stage :: Stage -> T.Text -> T.Text 

stage Init _ = "Welcome to Maskie's Ice Cream. What is your name? Type your name and press Enter:"

stage Quit name = T.replace "{name}" name "Come back to play another day, {name}? Bye!"

stage A1DarkHallway name = T.replace "{name}" name "Hello, {name}. You're in a dark hallway. It is night. Need help? Press h. To go forward: press w. To give up and quit: press q."

stage A1Help _ = "This is a text adventure game. It makes you read a lot. After each scene, you get a choice for what to do next. Enter your choice to continue, or q to quit. Take your time. Take as long as you need. No really: think it through. To go back, press w."

stage A1LightAppears name = T.replace "{name}" name "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize. To go forward: press w. To give up and quit: q."

stage A1HallwayDeath name = T.replace "{name}" name "The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It's an ice cream cone. He turns around and sees you. You tear him to shreds so fast he can't even scream. Now the ice cream image is dripping with blood. That will take some time to clean. Better get to it before the parlor opens again. Press w."

stage B1DarkHallway name = T.replace "{name}" name "Still here, {name}? You're back in the dark hallway. It is night. Again. This is night 2. To read the letter from your employer, press h. To go forward: press w. Press a to turn left. There's a light there. To turn right, press d."

stage B1Help name = T.replace "{name}" name "The letter reads: \"Dear {name}, Welcome to Maskie's Ice Cream! Great to have you on staff. Due a pandemic killing lots of people, I replaced all human employees with animatronics. They're like robots, but better. No people touch the ice cream, so no viruses get transmitted. That makes Maskie's a popular place. So popular that thieves like to come and steal the ice cream. That's where you come in, {name}. Your job is to keep out the thieves. See you tomorrow! Sincerely, John Masky, owner.\" Press w to continue."

stage B1Storage name = T.replace "{name}" name "You are in a lit storage room. There is a desk, a water fountain, and a closet. Is it hot in here? Press w to drink water from the fountain. Press d to look at the desk. To inspect the closet, press a. To go back to the hallway, press s."

stage B1TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon. And what's that? In the shadows behind the counter? Is that a person? To do your duty and inspect the situation, press w. However, you might be a coward. Press s to go back to the hallway. It won't be any better, though. Now choose."

stage B1TheParlorDeath name = T.replace "{name}" name "You get attacked. You can't see by whom or what: there is not enough light here. Wasn't there a lantern in the storage room? You really could have used that now. The attacker grabs you by the neck, and pulls you really close to his face. ITs face. It's an animatronic. Why is it attacking you? Why is it activated? It's night time! Shouldn't they be off? Good questions. But you have no time to reflect: the animatronic cracked your head like an eggshell. And now you can't even see how it pulled out your brain and eats it. That's going to make a mess! Now choose: to reincarnate and try again, press a. To give up and quit, press q."

stage B1DarkHallwayEnd name = T.replace "{name}" name "On the far end of the hallway, you see a wall. There might be a painting there, but it's too dark to make out. As a matter of fact, you can hardly see anything. Better go back, {name}. Press s to go back. Or press d to go right. Want to go left? Press a. To give up and quit, press q."

stage B1DarkToilets name = T.replace "{name}" name "You found the toilets. That will come in handy. It's a bit dark in here. If only you had some light. Stay here and take a wee in the dark? Press w. Go back into the hallway and look for a light? Press s."

stage B1DarkHallwayDeath name = T.replace "{name}" name "What's that? In the shadows? It's moving fast. It's coming for you, {name}! You try and get away, but it's dark. It jumps on you. You fall over backwards, cracking your head on the floor. The squeaky clean floor. Not so clean anymore. You have nothing to protect yourself. The attacker weighs down on your chest. You can't breathe. You can't see anything anymore. Desparately you grasp for air and try to push away your attacker. To no avail. Good bye, {name}. Press a to reincarnate and try again. Press q to give up and quit."

stage B1StorageDesk name = T.replace "{name}" name "The desk holds a lantern. It's the only light in the room. You probably should pick it up and carry it around as you roam the premises. You know: so you don't stumble over anything in the dark. To pick up the lantern, press y. Otherwise, press n."

stage B1StorageCloset name = T.replace "{name}" name "The closet is really dark. You can't see anything. Wouldn't you feel better with a lantern? Didn't you see one on the desk? To check the desk, press d. To rummage around the closet anyway, press w. To give up and quit, press q."

stage B1StorageClosetDeath name = T.replace "{name}" name "Ack! Something just grabbed you! You can't see what it is: it's too dark! It jumps on you. You fall over backwards, cracking your head on the edge of the desk. The desk with the lantern. The light makes your attacker visible. It's... an animatronic!? Why is it attacking you? And why is it singing? This is not the right time for the ice cream song! It's shredding your clothes and your skin. You have nothing to protect yourself. You succumb to pain and blood loss. Good try, {name}. Don't give up now. Press a to reincarnate and try again. Press q to give up and quit."

stage B1StorageFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. It is getting hot in here! There's a mirror on the wall in front of you. To lean forward and look into the mirror, press w. To return to the storage room, press s."

stage B1ViewMirror name = T.replace "{name}" name "You see yourself, {name}. But it's dark and your face shows many shadows and wrinkles. How do you feel? Tired? Hot? The cold water helped a bit. You probably need to come back here to stay cool. Why is it hot in here anyway? It's an ice cream parlor. Maybe the heat helps sell more ice cream? Beads of sweat drip down the side of your face, onto your shirt. You wipe them off. Enough self-reflection. Let's get back to work. Press s. Or, if you prefer to give up and quit, press q."

stage B2LitHallway name = T.replace "{name}" name "You're back in the hallway. It's still night 2. Your lantern helps you see. Where to? Forward: press w. Left: a. Right: d. You could give up now and press q. But you've come so far!"

stage B2LitHallwayEnd name = T.replace "{name}" name "On the far wall at the end of the hallway, you see a painting of an ice cream cone. Everything is squeaky clean. The cleaning crew must be doing a good job. Surely that's needed to keep the animatronics in good shape. On top of the ice cream in the painting, you spot a red ball with a yellow smiley face. Like a gumball. Wouldn't you like to eat some ice cream now? It is getting hot in here, after all. Press y for yes, n for no."

stage B2Storage name = T.replace "{name}" name "You are in a storage room. Your lantern lights it up. There is a desk, a water fountain, and a closet. Press w to cool down with some cold water from the fountain. Press d to check the desk. To inspect the closet, press a. To go back to the hallway, press s."

stage B2StorageCloset name = T.replace "{name}" name "The closet contains cleaning supplies and 2 sets of clothes: a spare security guard uniform, and a cleaner's outfit. Would you like to inspect the cleaning supplies? If so, press y. If not, press n."

stage B2InspectSupplies name = T.replace "{name}" name "A broom... a mop... a bucket with rags... You shouldn't need these, {name}. You're a security guard. That's odd: something shiny is sticking out of the rags. Would a security guard ignore that? To ignore it and close the closet, press s. Otherwise, pick where to rummage: press a, w, or d:"

stage B2aFindShield name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. Press s to close the closet and return to the storage room. Still curious? Maybe pressing w or d will get you something else?"

stage B2cFindShield name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. Press s to close the closet and return to the storage room. Still curious? You already found a map. Maybe pressing d will get you something else?"

stage B2eFindShield name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. Press s to close the closet and return to the storage room. Still curious? You already found a memory stick. Maybe pressing w will get you something else?"

stage B2jFindShield name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You also found a memory stick and a map. That's all there is to find. Press s to close the closet and return to the storage room."

stage B2bFindMap name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. Let's have a closer look. Press s to close the closet and return to the storage room. Still curious? You already found a shield. Maybe pressing d will get you something else?"

stage B2eFindMap name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. Let's have a closer look. Press s to close the closet and return to the storage room. Still curious? You already found a memory stick. Maybe pressing a will get you something else?"

stage B2gFindMap name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. Let's have a closer look. There is nothing else to find. Press s to close the closet and return to the storage room."

stage B2wFindMap name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. Let's have a closer look. Press s to close the closet and return to the storage room. Still curious? Maybe pressing a or d will get you something else?"

stage B2bFindMemoryStick name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. Press s to close the closet and return to the storage room. Still curious? You already found the shield. Maybe pressing w will get you something else?"

stage B2cFindMemoryStick name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. Press s to close the closet and return to the storage room. Still curious? You already found the map. Maybe pressing a will get you something else?"

stage B2dFindMemoryStick name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. Press s to close the closet and return to the storage room. Still curious? Maybe pressing a or w will get you something else?"

stage B2fFindMemoryStick name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. Press s to close the closet and return to the storage room. There is nothing else here."

stage B2StorageDesk name = T.replace "{name}" name "There's a letter on the desk, with your name on it, {name}. Would you like to read it? If so, press y. If not, press n."

stage B2ReadLetter name = T.replace "{name}" name "The letter reads: \"Dear {name}, if you read this, I will be dead. Chances are you will die too. Something wanders the hallways at night. Did John Masky tell you that you need to keep out thieves? He lied to you. It isn't thieves that kill us. It's something much worse. But don't take my word for it. Check it out yourself. Maybe you can find a way to stop it. Good luck.\" Press s to go back and keep playing. Scared? Give up and quit! Press q."

stage B2StorageFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. The heat is getting unbearable! There's a mirror on the wall in front of you. Let's check your hair. Got to look the part, after all. To lean forward and look into the mirror, press w. To return to the storage room, press s."

stage B2ViewMirror name = T.replace "{name}" name "You see yourself, {name}. The light from the lantern does not do you any favors. You straighten your hair. And spot something shoot by from the corner of your eye. What do you do? Chase it? Press w. Stay in the room and keep looking for other things that may help? Press s."

stage B2TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off, but your lantern spreads the light. You can make out tables and cabinets. And what's that? In the shadows behind the counter? Is that an ice cream dispenser? Let's have some! Surely that will cool you down. Press y to eat the ice cream. Press s to return to the hallway. To quit, press q."

stage B2EatIcecream name = T.replace "{name}" name "This truly is the best ice cream ever. It's clear why people would want to come here and have some. The ice cream tastes so good, that you forgot to wonder why the dispenser hadn't been turned off. It's midnight, after all. Shouldn't it have been cleaned for hygiene? If you agree, press y. If not, press n."

stage B2FoodPoisoning name = T.replace "{name}" name "You don't feel so good. Your stomach is growling. Better get to the bathroom, {name}, quickly! Press w to go visit the bathrooms. Or s to suck it up and ignore the pain."

stage B2DieFromFoodPoisoning name = T.replace "{name}" name "Best ice cream in the country, aye? Not so sure about that anymore, are you, {name}? Your belly swells up fast. It hurts! It explodes! Your guts get flung out all over the place. You fall down on your knees and then flat on your face. You would drown if the blood loss wouldn't kill you first. Press q to quit. Or, to reincarnate and try again, press a. Don't eat the ice cream, OK {name}?"

stage B2LitToilets name = T.replace "{name}" name "You found the toilets. Just in time! To go and do a number 2, pick any stall: a, w, or d. Or press s to back out now."

stage B2FeelBetterNow name = T.replace "{name}" name "Much better. What a relief! You sure could use cold drink of water now. Let's head back, {name}. Press w to continue."

stage B2HallwayDeath name = T.replace "{name}" name "Oh how courageous! You ran after whatever it was, into the hallway. You can see it clearly now: it's an animatronic! It rides around on a wheel. And it's coming for you really fast! If only you had had something to protect yourself! But you don't. Did you not see the shield in the storage room closet? Did you not take it? Too late! The animatronic just ran you over. You notice how heavy it is. Way heavier than a human. You hear your bones crushing and breaking as the animatronic spins in circles on top of you. You want to scream, but the pain is too much. You faint. And then you die. Where does the animatronic go? Why did it attack you? No clue. Want to find out? Press a to reincarnate and try again. Or run away and quit, by pressing q."

stage B3Storage name = T.replace "{name}" name "No need to get your hands dirty, right {name}? After all you aren't the cleaner. And the bucket won't be stealing no ice cream. Alright. Let's walk over to the fountain and get some cold water. Press w. Or press d to take a break and sit down at the desk. Feel like quitting? Press q!"

stage B3StorageDesk name = T.replace "{name}" name "Right. Let's take a break. Securing the premises is tiring! You set the lantern down on the desk. That causes a small round shadow to appear. A shadow? You recognize it is cast by a button. What would that do? Want to find out, {name}? Press y for yes. Or be boring and press n for no."

stage B3PressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. It starts playing a video. It's another security guard. He seems afraid and tired. He says: \"Get out! Get out now! The animatronics! They're not OK! Far from! Someone's messed them up. Find something to protect yourself and get out!\". Really now? That's a bit dramatic, isn't it? The video continues: something jumped the security guard and they fell out of view. He's screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B3StorageFountain name = T.replace "{name}" name "Suddenly you hear glass sliding on glass. The mirror mounted on the wall above the fountain slides sideways. The space behind the mirror is dark. Something in there is making its way out into the light. Where you are, {name}. Jump out of the way, quick! Press a!"

stage B3StorageDeath name = T.replace "{name}" name "Oh dear! It fell out, bumped on the fountain, and landed right on top of you! And it's heavy! So heavy! Its metal face looks human, but not quite. It studies you. Does it seem confused? Difficult to tell. You struggle to push it off. Can't... move! Can't... breathe! Is that laughter you hear? Crying? Can't tell. Can't... (Press q.)"

stage B3aStorage name = T.replace "{name}" name "You are in a storage room. You just found a shield in the supply bucket in the closet. Would it have anything else in store? Press a to rummage through it again. Feel like taking a break? Press s to sit down at the desk. Or press q to quit."

stage B3dStorage name = T.replace "{name}" name "You are in a storage room. You just found a memory stick in the supply bucket in the closet. Would it have anything else in store? Press d to rummage through it again. Feel like taking a break? Press s to sit down at the desk. Or press q to quit."

stage B3wStorage name = T.replace "{name}" name "You are in a storage room. You just found a map in the supply bucket in the closet. Would it have anything else in store? Press w to rummage through it again. Feel like taking a break? Press s to sit down at the desk. Or press q to quit."

stage B3adStorage name = T.replace "{name}" name "Yet to add. Press q to quit."

stage B3awStorage name = T.replace "{name}" name "Yet to add. Press q to quit."

stage B3adwStorage name = T.replace "{name}" name "Yet to add. Press q to quit."

stage A5Storage name = T.replace "{name}" name "Yet to add. Press q to quit."


