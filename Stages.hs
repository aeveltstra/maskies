{-# LANGUAGE OverloadedStrings #-}

{-
    Maskies is a text adventure horror survival game.
    Try not to die.
    This file contains the game's stages, their
    variations, and how they wire together.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.906.1133
-}
module Stages where

import Prelude
import qualified Data.Text as T
import qualified TextHelper as TH
import qualified Keys as K 

{- Let's use Haskell's type system to determine exactly which stages the game can have. If we forget one, the compiler will complain. Unfortunately it does not complain if we forget to wire up a stage in the function 'next'. But it does help when showing stages: the compiler automatically constrains us to only show stages that have been defined here. -}
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
  | B1Locker
  | B1LockerDeath
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
  | B2Locker
  | B2InspectSupplies
  | B2aFind
  | B2adFind
  | B2awFind
  | B2adwFind
  | B2awdFind
  | B2dFind
  | B2daFind
  | B2dwFind
  | B2dwaFind
  | B2wFind
  | B2waFind
  | B2wdFind
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
  | B3adwStorage
  | B3StorageDesk
  | B3aStorageDesk
  | B3adStorageDesk
  | B3awStorageDesk
  | B3adwStorageDesk
  | B3dStorageDesk
  | B3dwStorageDesk
  | B3wStorageDesk
  | B3StorageFountain
  | B3PressedButton
  | B3StorageDeath
  | B4aPressedButton
  | B4PressedButton
  | B4wPressedButton
  | B4adPressedButton
  | B4dwPressedButton
  | B4awPressedButton
  | B4adwPressedButton
  | B4EndOfShift
  | B4aEndOfShift
  | B4wEndOfShift
  | B4awEndOfShift
  | B4pEndOfShift
  | B4paEndOfShift
  | B4pwEndOfShift
  | B4pawEndOfShift
  | B4Ridicule
  | B4aRidicule
  | B4awRidicule
  | B4wRidicule
  | B4Storage
  | C1Welcome
  | C1aWelcome
  | C1wWelcome
  | C1awWelcome
  | C1MaskiesLetter
  | C1aMaskiesLetter
  | C1wMaskiesLetter
  | C1awMaskiesLetter
  | C1Hallway
  | C1aHallway
  | C1wHallway
  | C1awHallway
  | C1CourseStart
  | C1aCourseStart
  | C1wCourseStart
  | C1awCourseStart
  | C1Help
  | C1aHelp
  | C1wHelp
  | C1awHelp
  | C1
  | C1a
  | C1w
  | C1aw
  | C1f
  | C1fDeath
  | C1fa
  | C1faAttack
  | C1faDeath
  | C1faSurvive
  | C1fw
  | C1fwDeath
  | C1faw
  | C1fawAttack
  | C1fawDeath
  | C1fawSurvive
  | C2
  | C2a
  | C2w
  | C2aw
  | C3
  | C3a
  | C3w
  | C3aw
  | Quit
  deriving (Show, Eq)

{- This function wires Key events to stages, to determine which stage to show next. If the event is Wait, the same stage gets returned as put in. If the event is Q, the function returns the Quit event. If the player enters a key that is not wired up to a stage here, the function throws an error. -}
next :: Stage -> K.Key -> Stage
next stage K.Wait = stage
next _ K.Q = Quit
next Init _ = A1DarkHallway
next A1DarkHallway K.H = A1Help
next A1Help _ = A1LightAppears
next A1DarkHallway _ = A1LightAppears
next A1LightAppears _ = A1HallwayDeath
next A1HallwayDeath _ = B1DarkHallway
next B1DarkHallway K.A = B1Storage
next B1DarkHallway K.W = B1DarkHallwayEnd
next B1DarkHallway K.D = B1TheParlor
next B1DarkHallway _ = B1Help
next B1Help _ = B1Storage
next B1Storage K.D = B1StorageDesk
next B1Storage K.A = B1Locker
next B1Storage K.W = B1StorageFountain
next B1Storage _ = B1DarkHallway
next B1StorageDesk K.Y = B2Storage
next B1StorageDesk _ = B1Storage
next B1Locker K.D = B1StorageDesk
next B1Locker _ = B1LockerDeath
next B1LockerDeath K.A = B1Storage
next B1LockerDeath _ = Quit
next B1StorageFountain K.W = B1ViewMirror
next B1StorageFountain _ = B1Storage
next B1ViewMirror _ = B1Storage
next B1DarkHallwayEnd K.D = B1TheParlor
next B1DarkHallwayEnd K.S = B1DarkHallway
next B1DarkHallwayEnd K.A = B1DarkToilets
next B1DarkHallwayEnd _ = B1DarkHallwayDeath
next B1DarkHallwayDeath K.A = B1DarkHallway
next B1DarkHallwayDeath _ = Quit
next B1DarkToilets K.S = B1DarkHallway
next B1DarkToilets _ = B1DarkHallwayDeath
next B1TheParlor K.W = B1TheParlorDeath
next B1TheParlor _ = B1DarkHallway
next B1TheParlorDeath K.A = B1DarkHallway
next B1TheParlorDeath _ = Quit
next B2LitHallway K.A = B2Storage
next B2LitHallway K.W = B2LitHallwayEnd
next B2LitHallway K.D = B2TheParlor
next B2LitHallway _ = B2LitHallway
next B2Storage K.S = B2LitHallway
next B2Locker K.Y = B2InspectSupplies
next B2Locker _ = B3Storage
next B2InspectSupplies K.A = B2aFind
next B2InspectSupplies K.W = B2wFind
next B2InspectSupplies K.D = B2dFind
next B2InspectSupplies _ = B3Storage
next B2aFind K.W = B2awFind
next B2aFind K.D = B2adFind
next B2aFind _ = B3aStorageDesk
next B2wFind K.A = B2waFind
next B2wFind K.D = B2wdFind
next B2wFind _ = B3wStorageDesk
next B2dFind K.A = B2daFind
next B2dFind K.W = B2dwFind
next B2dFind _ = B3dStorageDesk
next B2awFind K.D = B2awdFind
next B2awFind _ = B3awStorageDesk
next B2adFind K.W = B2adwFind
next B2adFind _ = B3adStorageDesk
next B2daFind K.W = B2adwFind
next B2daFind _ = B3adStorageDesk
next B2dwFind K.A = B2dwaFind
next B2dwFind _ = B3dwStorageDesk
next B2waFind K.D = B2awdFind
next B2waFind _ = B3awStorageDesk
next B2wdFind K.A = B2dwaFind
next B2wdFind _ = B3dwStorageDesk
next B2awdFind _ = B3adwStorageDesk
next B2adwFind _ = B3adwStorageDesk
next B2dwaFind _ = B3adwStorageDesk
next B3aStorageDesk K.W = B2awFind
next B3aStorageDesk K.D = B2adFind
next B3aStorageDesk K.Y = B4aPressedButton
next B3aStorageDesk _ = B4aEndOfShift
next B3dStorageDesk K.A = B2daFind
next B3dStorageDesk K.W = B2dwFind
next B3dStorageDesk K.Y = B4PressedButton
next B3dStorageDesk _ = B4EndOfShift
next B3wStorageDesk K.A = B2waFind
next B3wStorageDesk K.D = B2wdFind
next B3wStorageDesk K.Y = B4wPressedButton
next B3wStorageDesk _ = B4wEndOfShift
next B3awStorageDesk K.D = B2awdFind
next B3awStorageDesk K.Y = B4awPressedButton
next B3awStorageDesk _ = B4awEndOfShift
next B3adStorageDesk K.W = B2adwFind
next B3adStorageDesk K.Y = B4adPressedButton
next B3adStorageDesk _ = B4aEndOfShift
next B3dwStorageDesk K.A = B2dwaFind
next B3dwStorageDesk K.Y = B4dwPressedButton
next B3dwStorageDesk _ = B4wEndOfShift
next B3adwStorageDesk K.Y = B4PressedButton
next B3adwStorageDesk _ = B4EndOfShift
next B2Storage K.D = B2StorageDesk
next B2Storage K.A = B2Locker
next B2Storage K.W = B2StorageFountain
next B2Storage _ = B2Storage
next B2StorageFountain K.W = B2ViewMirror
next B2StorageFountain _  = B2Storage
next B2StorageDesk K.Y = B2ReadLetter
next B2StorageDesk _ = B2Storage
next B2ReadLetter _ = B2Storage
next B2ViewMirror K.S = B2Storage
next B2ViewMirror _ = B2HallwayDeath
next B2HallwayDeath K.A = B2LitHallway
next B2HallwayDeath _ = Quit
next B2LitHallwayEnd K.Y = B2TheParlor
next B2LitHallwayEnd _ = B2LitHallway
next B2TheParlor K.Y = B2EatIcecream
next B2TheParlor _ = B2LitHallway
next B2EatIcecream K.N = B2FoodPoisoning
next B2EatIcecream _ = B2LitHallway
next B2FoodPoisoning K.W = B2LitToilets
next B2FoodPoisoning _ = B2DieFromFoodPoisoning
next B2LitToilets K.S = B2DieFromFoodPoisoning
next B2DieFromFoodPoisoning K.A = B2LitHallway
next B2DieFromFoodPoisoning _ = Quit
next B2LitToilets _ = B2FeelBetterNow
next B2FeelBetterNow _ = B2LitHallwayEnd
next B3Storage K.D = B3StorageDesk
next B3Storage _ = B3StorageFountain
next B3StorageDesk K.Y = B3PressedButton
next B3StorageDesk _ = B3StorageFountain
next B3PressedButton K.Y = B2Locker
next B3PressedButton _ = B3StorageFountain
next B3StorageFountain _ = B3StorageDeath
next B3StorageDeath _ = Quit
next B4PressedButton K.Y = B4pEndOfShift
next B4PressedButton _ = B4EndOfShift
next B4aPressedButton K.Y = B4paEndOfShift
next B4aPressedButton _ = B4aEndOfShift
next B4wPressedButton K.Y = B4pwEndOfShift
next B4wPressedButton _ = B4wEndOfShift
next B4awPressedButton K.Y = B4pawEndOfShift
next B4awPressedButton _ = B4awEndOfShift
next B4pEndOfShift K.Y = B4Ridicule
next B4paEndOfShift K.Y = B4aRidicule
next B4pwEndOfShift K.Y = B4wRidicule
next B4pawEndOfShift K.Y = B4awRidicule
next B4EndOfShift _ = C1Welcome
next B4aEndOfShift _ = C1aWelcome
next B4wEndOfShift _ = C1wWelcome
next B4awEndOfShift _ = C1awWelcome
next B4pEndOfShift _ = C1Welcome
next B4paEndOfShift _ = C1aWelcome
next B4pwEndOfShift _ = C1wWelcome
next B4pawEndOfShift _ = C1awWelcome
next B4Ridicule K.Y = C1Welcome
next B4aRidicule K.Y = C1aWelcome
next B4wRidicule K.Y = C1wWelcome
next B4awRidicule K.Y = C1awWelcome
next C1Welcome K.H = C1MaskiesLetter
next C1aWelcome K.H = C1aMaskiesLetter
next C1wWelcome K.H = C1wMaskiesLetter
next C1awWelcome K.H = C1awMaskiesLetter
next C1Welcome _ = C1Hallway
next C1aWelcome _ = C1aHallway
next C1wWelcome _ = C1wHallway
next C1awWelcome _ = C1awHallway
next C1MaskiesLetter _ = C1Hallway
next C1aMaskiesLetter _ = C1aHallway
next C1wMaskiesLetter _ = C1wHallway
next C1awMaskiesLetter _ = C1awHallway
next C1Hallway _ = C1CourseStart
next C1aHallway _ = C1aCourseStart
next C1wHallway _ = C1wCourseStart
next C1awHallway _ = C1awCourseStart
next C1CourseStart K.H = C1Help
next C1aCourseStart K.H = C1aHelp
next C1wCourseStart K.H = C1wHelp
next C1awCourseStart K.H = C1awHelp
next C1CourseStart _ = C1
next C1aCourseStart _ = C1a
next C1wCourseStart _ = C1w
next C1awCourseStart _ = C1aw
next C1Help _ = C1
next C1aHelp _ = C1a
next C1wHelp _ = C1w
next C1awHelp _ = C1aw
next C1 K.A = C2
next C1 _ = C1f
next C1a K.A = C2a
next C1a _ = C1fa
next C1w K.A = C2w
next C1w _ = C1fw
next C1aw K.A = C2aw
next C1aw _ = C1faw
next C1f K.S = C3
next C1f _ = C1fDeath
next C1fDeath K.A = B1DarkHallway
next C1fDeath _ = Quit
next C1fa K.S = C3a
next C1fa _ = C1faAttack
next C1faAttack K.D = C1faSurvive
next C1faAttack _ = C1faDeath
next C1faDeath K.A = C1aCourseStart
next C1faDeath _ = Quit
next C1faSurvive _ = C3a
next C1fw K.S = C3w
next C1fw _ = C1fwDeath
next C1fwDeath K.A = B1DarkHallway
next C1fwDeath _ = Quit
next C1faw K.S = C3aw
next C1faw _ = C1fawAttack
next C1fawAttack K.D = C1fawSurvive
next C1fawAttack _ = C1fawDeath
next C1fawDeath K.A = C1awCourseStart 
next C1fawDeath _ = Quit
next C1fawSurvive _ = C3aw
next _ _ = error "Yet to wire up."

{- These are the texts to show for each stage. This architecture assumes that the game loop outputs these texts and captures input from the player, to return to an other stage. -}
stage :: Stage -> T.Text -> T.Text 

stage Init _ = "Welcome to Maskie's Ice Cream. What is your name? Type your name and press Enter:"

stage Quit name = T.replace "{name}" name "Come back to play another day, {name}? Bring your friends! Bye!"

stage A1DarkHallway name = T.replace "{name}" name "Hello, {name}. You're in a dark hallway. It is night. Need help? Press h. To go forward: press w. To give up and quit: press q."

stage A1Help _ = "This is a text adventure game. It makes you read a lot. After each scene, you get a choice for what to do next. Enter your choice to continue, or q to quit. Take your time. Take as long as you need. No really: think it through. To go back, press w."

stage A1LightAppears name = T.replace "{name}" name "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize. The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It's an ice cream cone. He turns around and sees you. To go forward: press w. To give up and quit: q."

stage A1HallwayDeath name = T.replace "{name}" name "You take his lantern... but also his arm. Blood sprays from his torso onto the ice cream painting. He screams. It hurts your ears. You want to silence him. But his head is so very fragile. It breaks and falls off. The guard's body sags to the tiled floor. You step backwards to avoid the spreading blood. Walking through the hallway, you see a mirror above the fountain. Let's make sure no blood hit you. And if so, clean up. You see your face. You look kind-of like that guard. Not quite. Your eyes and ears are square, and your mouth has no lips to cover your teeth. Metal teeth. And so many! Confused you disappear into a dark room. \nPress w to keep playing. Want to quit? Press q."

stage B1DarkHallway name = T.replace "{name}" name "Hello, {name}. You're alone. The hallway is dark. It is night. This is night 2. Some children's music is playing from hidden speakers. Why? It's night time! They should turn that off. Your employer gave you a letter. Press h to read it. You don't have to read it. You can skip it. I wouldn't. But hey: your choice. To go forward: press w. Press a to turn left. There's a light there. To turn right, press d."

stage B1Help name = T.replace "{name}" name "The letter reads: \"Dear {name}, Welcome to Maskie's Ice Cream! Great to have you on staff. Due a pandemic killing lots of people, I replaced all human employees with animatronics. Lots of them! They're like robots, but better. And they look like stuffed animals for kids. That means no people touch the ice cream, thus no viruses get transmitted. Making Maskie's Ice Cream very popular. So popular that thieves like to come and steal the ice cream. That's where you come in, {name}. Your job is to keep out the thieves. See you tomorrow! Sincerely, John Masky, owner.\" Press w to continue."

stage B1Storage name = T.replace "{name}" name "You are in a lit storage room. By the right wall there is a desk, a water fountain by the far wall, and a locker to the left. Is it hot in here? Press w to drink water from the fountain. Press d to look at the desk. To inspect the locker, press a. To go back to the hallway, press s."

stage B1TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon. And what's that? In the shadows behind the counter? Is that a person? Could be a thief! To do your duty and inspect the situation, press w. However, you might be a coward. Press s to go back to the hallway. It won't be any better, though. Now choose."

stage B1TheParlorDeath name = T.replace "{name}" name "You get attacked! You can't see them: it's too dark! Wasn't there a lantern in the storage room? You really could've used that now. The attacker grabs you by the neck, and pulls you really close to their face. ITs face. It's an animatronic. With no lips on a mouth filled with sharp metal teeth. Cameras instead of eyes. With an ice cream cone for 1 ear, and some wires where the other should have been. With bulbous arms that grab you and instead of a hand, an ice scoop that scoops out your eyes. It rips out your brain and eats it. That's going to make a mess! Now choose: to reincarnate and try again, press a. To give up and quit, press q."

stage B1DarkHallwayEnd name = T.replace "{name}" name "On the far end of the hallway, you see a wall. There might be a painting there, but it's too dark to make out. As a matter of fact, you can hardly see anything. Better go back, {name}, and find some source of light. Press s to go back. Or press d to go right. Want to go left? Press a. To give up and quit, press q."

stage B1DarkToilets name = T.replace "{name}" name "You found the toilets. That will come in handy. It's a bit dark in here. It smells clean. The tiles squeak under your shoes. Very clean. Someone did a good job. If only you had some light to really enjoy that. You hear water trickling. It reminds you of your bladder. What do you do? Stay here and take a wee in the dark? Press w. Or ignore your bladder and go back into the hallway and look for a light? If so, press s."

stage B1DarkHallwayDeath name = T.replace "{name}" name "What's that? In the shadows? It's moving fast. It's coming for you, {name}! You try and get away, but it's dark. It jumps on you. You fall over backwards, cracking your head on the floor. The squeaky clean floor. Not so clean anymore. You have nothing to protect yourself. The attacker weighs down on your chest. You can't breathe. You can't see anything anymore. Desparately you grasp for air and try to push away your attacker. To no avail. Good bye, {name}. Press a to reincarnate and try again. Press q to give up and quit."

stage B1StorageDesk name = T.replace "{name}" name "The desk holds a lantern. It's the only light in the room. You probably should pick it up and carry it around as you roam the premises. You know: so you don't stumble over anything in the dark. To pick up the lantern, press y. Otherwise, press n."

stage B1Locker name = T.replace "{name}" name "The locker is really dark. You can't see anything. Wouldn't you feel better with a lantern? Didn't you see one on the desk? To check the desk, press d. To rummage around the locker anyway, press w. To give up and quit, press q."

stage B1LockerDeath name = T.replace "{name}" name "Ack! Something just grabbed you! You can't see what it is: it's too dark! It jumps on you. You fall over backwards, cracking your head on the edge of the desk. The desk with the lantern. The light makes your attacker visible. It's... an animatronic!? A penguin? Why is it singing? Penguins can't sing the ice cream song! Can they? Its beak shreds your clothes and your skin. You have nothing to protect yourself. Your blood shorts out the penguin. You faint. You die. Good try, {name}. Don't give up now. Press a to reincarnate and try again. Press q to give up and quit."

stage B1StorageFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. It is getting hot in here! There's a mirror on the wall in front of you. To lean forward and look into the mirror, press w. To return to the storage room, press s."

stage B1ViewMirror name = T.replace "{name}" name "You see yourself, {name}. But it's dark and your face shows many shadows and wrinkles. How do you feel? Tired? Hot? The cold water helped a bit. You probably need to come back here to stay cool. Why is it hot in here anyway? It's an ice cream parlor. Maybe the heat helps sell more ice cream? Beads of sweat drip down the side of your face, onto your shirt. You wipe them off. Enough self-reflection. Let's get back to work. Press s. Or, if you prefer to give up and quit, press q."

stage B2LitHallway name = T.replace "{name}" name "You're back in the hallway. You're still alone. It's still night 2. Your lantern helps you see. Where to? Forward: press w. Left: a. Right: d. You could give up now and press q. But you've come so far!"

stage B2LitHallwayEnd name = T.replace "{name}" name "On the far wall at the end of the hallway, you see a painting of an ice cream cone. Everything is squeaky clean. The cleaning crew must be doing a good job. Surely that's needed to keep the animatronics in good shape. On top of the ice cream in the painting, you spot a red ball with a yellow smiley face. Like a gumball. Wouldn't you like to eat some ice cream now? It is getting hot in here, after all. Press y for yes, n for no."

stage B2Storage name = T.replace "{name}" name "You are in a storage room. Your lantern lights it up. To the right you see a desk. Against the far wall you see a water fountain. K.A locker is set against the left wall. Press w to cool down with some cold water from the fountain. Press d to check the desk. To inspect the locker, press a. To go back to the hallway, press s."

stage B2Locker name = T.replace "{name}" name "The locker contains cleaning supplies and 2 sets of clothes: a spare security guard uniform, and a cleaner's outfit. Wouldn't you like to inspect the cleaning supplies? If so, press y. But you know that curiosity kills the cat. So maybe not? If not, press n."

stage B2InspectSupplies name = T.replace "{name}" name "A broom... a mop... a bucket with rags... You shouldn't need these, {name}. You're a security guard. That's odd: something shiny is sticking out of the rags. Would a security guard ignore that? To ignore it and close the locker, press s. Otherwise, pick where to rummage: press a, w, or d:"

stage B2aFind name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. Press s to study the shield at the desk. Still curious? Maybe pressing w or d will get you something else?"

stage B2waFind name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You already found a map. Press s to study them at the desk. Still curious? Maybe pressing d will get you something else?"

stage B2daFind name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You already found a memory stick. Press s to study them at the desk. Still curious? Maybe pressing w will get you something else?"

stage B2dwaFind name = T.replace "{name}" name "You found a shield! It's shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You also found a memory stick and a map. That's all there is to find. Press s to study them at the desk."

stage B2awFind name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. You already found a shield. Let's have a closer look. Press s to study them at the desk. Still curious? Maybe pressing d will get you something else?"

stage B2dwFind name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. You already found a memory stick. Let's have a closer look. Press s to study them at the desk. Still curious? Maybe pressing a will get you something else?"

stage B2adwFind name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. Let's have a closer look. Press s to study your finds at the desk."

stage B2wFind name = T.replace "{name}" name "You found a map! It's wrinkly and stained with water. People really have to take better care of their posessions. Let's have a closer look. Press s to study it at the desk. Still curious? Maybe pressing a or d will get you something else?"

stage B2adFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. Press s to close the locker and return to the storage room. Still curious? You already found the shield. Maybe pressing w will get you something else?"

stage B2wdFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. You already found a map. Press s to study them at the desk. Still curious? Maybe pressing a will get you something else?"

stage B2dFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. Press s to study it at the desk. Still curious? Maybe pressing a or w will get you something else?"

stage B2awdFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you'll need a USB port to plug that in. There is nothing else here. Press s to study your finds at the desk."

stage B2StorageDesk name = T.replace "{name}" name "There's a letter on the desk, with your name on it, {name}. Would you like to read it? If so, press y. If not, press n."

stage B2ReadLetter name = T.replace "{name}" name "The letter reads: \"Dear {name}, if you read this, I will be dead. Chances are you will die too. Something wanders the hallways at night. Did John Masky tell you that you need to keep out thieves? He lied to you. It isn't thieves that kill us. It's something much worse. And don't trust the animatronics. They may look like cuddly animals, but every now and then they scare me half to death. I've hidden some helpful items in the locker. You'll need them tomorrow. Good luck.\" Items you need tomorrow? Better go find them! Press s to go back and keep playing. Scared? Give up and quit! Press q."

stage B2StorageFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. The heat is getting unbearable! There's a mirror on the wall in front of you. Let's check your hair. Got to look the part, after all. To lean forward and look into the mirror, press w. To return to the storage room, press s."

stage B2ViewMirror name = T.replace "{name}" name "You see yourself, {name}. The light from the lantern does not do you any favors. You straighten your hair. And spot something shoot by from the corner of your eye. What do you do? Chase it? Press w. Stay in the room and keep looking for other things that may help? Press s."

stage B2TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off, but your lantern spreads the light. You can make out tables and cabinets. And what's that? In the shadows behind the counter? Is that an ice cream dispenser? Maskies does boast the best ice cream in the county! Let's have some! Surely that will cool you down. Press y to eat some. Press s to return to the hallway. To quit, press q."

stage B2EatIcecream name = T.replace "{name}" name "This truly is the best ice cream ever. It's clear why people would want to come here and have some. The ice cream tastes so good, that you forgot to wonder why the dispenser hadn't been turned off. It's midnight, after all. Shouldn't it have been cleaned for hygiene? If you agree, press y. If not, press n."

stage B2FoodPoisoning name = T.replace "{name}" name "You don't feel so good. Your stomach is growling. Better get to the bathroom, {name}, quickly! Press w to go visit the bathrooms. Or s to suck it up and ignore the pain."

stage B2DieFromFoodPoisoning name = T.replace "{name}" name "Best ice cream in the country, aye? Not so sure about that anymore, are you, {name}? Your belly swells up fast. It hurts! It explodes! Your guts get flung out all over the place. You fall down on your knees and then flat on your face. You would drown if the blood loss wouldn't kill you first. Game over. Press q to quit. Or, to reincarnate and try again, press a. Don't eat the ice cream, OK {name}?"

stage B2LitToilets name = T.replace "{name}" name "You found the toilets. Just in time! To go and do a number 2, pick any stall: a, w, or d. You're all alone anyway. Or press s to back out now."

stage B2FeelBetterNow name = T.replace "{name}" name "Much better. What a relief! You sure could use cold drink of water now. Let's head back, {name}. Press w to continue."

stage B2HallwayDeath name = T.replace "{name}" name "Oh how courageous! You ran after whatever it was, into the hallway. You can see it clearly now: it's an animatronic! It rides around on a wheel. And it's coming for you really fast! If only you had had something to protect yourself! But you don't. Didn't you see the shield in the storage room locker? Didn't you take it? Why not? Too late now! The animatronic runs you over. It's heavy. Way heavier than a human. Your bones crush and break as the animatronic spins circles on top of you. Your scream is drowned out by the music. So loud! Then you faint. And die. Where does the animatronic go? Why did it attack you? No clue... yet. Want to find out? Press a to reincarnate and try again. Or run away and quit, by pressing q."

stage B3Storage name = T.replace "{name}" name "No need to get your hands dirty, right {name}? After all you aren't the cleaner. And the bucket won't be stealing no ice cream. Alright. Let's cool off with some cold water from the fountain. Press w. Or press d to take a break and sit down at the desk. Feel like quitting? Press q!"

stage B3StorageDesk name = T.replace "{name}" name "Right. Let's take a break. Securing the premises is tiring! You set the lantern down on the desk. That causes a small round shadow to appear. A shadow? You recognize it is cast by a button. What would that do? Want to find out, {name}? Press y for yes. Or be boring and press n for no."

stage B3PressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. It has a USB port on the side. But you have nothing to plug into it. Doesn't it feel like you might be missing out on something important, {name}? Press y if so. If not, press n."

stage B3StorageFountain name = T.replace "{name}" name "Suddenly you hear glass sliding on glass. The mirror mounted on the wall above the fountain slides away. The space behind it is dark. Something in there is making its way out into the light! Where you are, {name}! Jump out of the way, quick! Press a!"

stage B3StorageDeath name = T.replace "{name}" name "Oh dear! It fell out, bumped on the fountain, and landed right on top of you! It's a bunny? Pink and white, and fluffy... but big! And heavy! So heavy! It studies you. With square eyes. Does it seem confused? Hard to tell. It sniffs at your face. You struggle to push it off. Can't... move! Can't... breathe! Is that singing you hear? Crying? Can't tell. Can't... (Game over. Press q.)"

stage B3aStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. That causes a small round shadow to appear, that wasn't there before. It's a button. What do you do? To press the button, press y. Or maybe look for other objects hidden in the wash bucket? Get your hands dirty? If so, press d or w."

stage B3dStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. That causes a small round shadow to appear, that wasn't there before. It's a button. What do you do? To press the button, press y. Or maybe look for other objects hidden in the wash bucket? Get your hands dirty? If so, press a or w."

stage B3wStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. That causes a small round shadow to appear, that wasn't there before. It's a button. What do you do? To press the button, press y. Or maybe look for other objects hidden in the wash bucket? Get your hands dirty? If so, press a or d."

stage B3adStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. It lights up a button. What do you do? To press the button, press y. Or maybe look for another object hidden in the wash bucket? Get your hands dirty? If so, press w."

stage B3dwStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. It lights up a button. What do you do? To press the button, press y. Or maybe look for another object hidden in the wash bucket? Get your hands dirty? If so, press a."

stage B3awStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. It lights up a button. What do you do? To press the button, press y. Or maybe look for another object hidden in the wash bucket? Get your hands dirty? If so, press d."

stage B3adwStorageDesk name = T.replace "{name}" name "You set your lantern down on the table. Next to it, you place the map and the memory stick. You keep the shield on your arm. Might come in handy. Now where can you plug in the memory stick? It looks like you need a USB port. On the desk you spot the round shadow of a button. Maybe that gives access to a USB port? To press the button, press y. To ignore it and resume guarding duty until the morning, press n."

stage B4Storage name = T.replace "{name}" name "Yet to add. Press q to quit."

stage B4aPressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. The screen starts playing a video. It's another security guard. He seems afraid and tired. He says: \"Get out! Get out now while you can! The animatronics! Ain't no good! Far from! Someone's done messed them up! Gotta figure out why and how to stop 'em! And the boss? Don't let him trick you into the obstacle course! Bring a shield! Did you find the map? You'll need it!\". Really now? That's a bit dramatic, isn't it, {name}? The video continues: something jumped the security guard and they fell out of view. He's screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B4PressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. It has a USB port on the side. You plug in the memory stick. The screen starts playing a video. It's another security guard. He seems afraid and tired. He says: \"Get out! Get out now! The animatronics! They ain't OK! Far from! Someone's done messed them up. Gotta figure out why and how to stop 'em! And the boss? Don't let him trick you into the obstacle course! Did you find the shield and the map? You'll need those!\". Really now? That's a bit dramatic, isn't it, {name}? The video continues: something jumped the security guard and they fell out of view. He's screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B4wPressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. The screen starts playing a video. It's another security guard. He seems afraid and tired. He says: \"Get out! Get out now! The animatronics! They ain't OK! No sir! Someone's done messed them up. Gotta figure out why and how to stop 'em! And the boss? Don't let him trick you into the obstacle course! Did you find the shield too? You'll need it!\". Really now? That's a bit dramatic, isn't it, {name}? The video continues: something jumped the security guard and they fell out of view. He's screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B4EndOfShift name = T.replace "{name}" name b4EoSMsg
stage B4aEndOfShift name = T.replace "{name}" name b4EoSMsg
stage B4wEndOfShift name = T.replace "{name}" name b4EoSMsg
stage B4awEndOfShift name = T.replace "{name}" name b4EoSMsg
stage B4pEndOfShift name = T.replace "{name}" name b4pEoSMsg
stage B4paEndOfShift name = T.replace "{name}" name b4pEoSMsg
stage B4pwEndOfShift name = T.replace "{name}" name b4pEoSMsg
stage B4pawEndOfShift name = T.replace "{name}" name b4pEoSMsg

stage B4Ridicule name = T.replace "{name}" name b4RidiculeMsg
stage B4aRidicule name = T.replace "{name}" name b4RidiculeMsg
stage B4wRidicule name = T.replace "{name}" name b4RidiculeMsg
stage B4awRidicule name = T.replace "{name}" name b4RidiculeMsg

stage C1Welcome name = T.replace "{name}" name c1Msg
stage C1aWelcome name = T.replace "{name}" name c1Msg
stage C1wWelcome name = T.replace "{name}" name c1Msg
stage C1awWelcome name = T.replace "{name}" name c1Msg

stage C1MaskiesLetter name = T.replace "{name}" name "The letter reads: \"How's it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don't know whether they're still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That's going to be difficult if you don't have a map. And you'll have to be careful to avoid the animatronics. Some of them haven't been stored properly. I'll be back tomorrow morning to debrief you! Signed, John Masky.\" Quit while you can! Press q. Or keep playing: press w."

stage C1aMaskiesLetter name = T.replace "{name}" name "The letter reads: \"How's it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don't know whether they're still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That's going to be difficult if you don't have a map. But don't worry about the animatronics: they've been stored for the night. I'll be back tomorrow morning to debrief you! Signed, John Masky.\" Quit while you can! Press q. Or keep playing: press w."

stage C1wMaskiesLetter name = T.replace "{name}" name "The letter reads: \"How's it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don't know whether they're still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That's going to be easy if you have a map. And try and avoid the animatronics: some of them haven't been stored properly. I'll be back tomorrow morning to debrief you! Signed, John Masky.\" Quit while you can! Press q. Or keep playing: press w."

stage C1awMaskiesLetter name = T.replace "{name}" name "The letter reads: \"How's it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don't know whether they're still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That's going to be easy if you have a map. And you won't have to worry about any animatronics: they've been stored for the night. I'll be back tomorrow morning to debrief you! Signed, John Masky.\" Quit while you can! Press q. Or keep playing: press w."

stage C1Hallway name = T.replace "{name}" name "You're in the hallway. Alone. You hear children's music. Where is it coming from? You haven't seen a radio anywhere, yet. To turn left, press a. To go forward, press w. Or press d to turn right."
stage C1aHallway name = T.replace "{name}" name "You're in the hallway. Alone. You hear children's music. Where is it coming from ? You haven't seen a radio anywhere, yet. Better strap up that shield. If only you had a map. That would've helped. To turn left, press a. To go forward, press w. Or press d to turn right."
stage C1wHallway name = T.replace "{name}" name "You're in the hallway. Alone. You hear children's music. Where is it coming from? You haven't seen a radio anywhere, yet. Anyway, better pull up that map you found. Maybe that will help avoid the animatronics. To turn left, press a. To go forward, press w. Or press d to turn right."
stage C1awHallway name = T.replace "{name}" name "You're in the hallway. Alone. You hear children's music. Where is it coming from ? You haven't seen a radio anywhere, yet. Anyway, better strap up that shield. Also pull up that map: it should help guide you and avoid the animatronics. To turn left, press a. To go forward, press w. Or press d to turn right."

stage C1CourseStart name = T.replace "{name}" name "Erm. What? Smoke is blocking your view. It's dissapating slowly. This is not at all where you expected to go. Did you take a wrong turn? Did you get teleported? This looks like the start of the obstacle course. Oh no. You have nothing to protect yourself and no map to tell you where to go. This is not going to be fun. Press q to quit now! Or, if you're brave, press a direction (a, w, d, or s). Right now is your chance to get some help though. Want it? Press h."

stage C1aCourseStart name = T.replace "{name}" name "Erm. What? Smoke is blocking your view. To make it dissapate more quickly, you flap your shield like a wing. This is not at all where you expected to go? Did you take a wrong turn? Did you get teleported? It looks like the start of the obstacle course. It's a good thing you have that shield strapped to your arm. If only you had a map. Press q to quit now! Or, if you're brave, press a direction (a, w, d, or s). Or maybe... just maybe... you want to read the help. If so, press h."

stage C1wCourseStart name = T.replace "{name}" name "Erm. What? Smoke is blocking your view. To make it dissapate more quickly, you wave your map. This is not at all where you expected to go! Did you take a wrong turn? Did you get teleported? It looks like the start of the obstacle course. It's a good thing you have a map. But you have nothing to protect yourself. That's not going to be fun. Press q to quit now! Or, if you're brave, press a direction (a, w, d, or s). Or maybe... just maybe... you want to read the help. If so, press h."

stage C1awCourseStart name = T.replace "{name}" name "Erm... what? Smoke is blocking your view. To make it dissapate more quickly, you wave your map. This is not at all where you expected to go... It looks like you got teleported into the obstacle course. You check the shield on your arm: that will protect you. You check your map: that will guide you. Alright. Feeling brave? Choose a direction by pressing a, w, d, or s now. No need to press h and read the help, is there?"

stage C1Help name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into the animatronics. And you know how dangerous that is, don't you? Especially since you have no shield to defend yourself, {name}. Also, without a map it it's going to be difficult to guess which way to go. You sure you want to do this? If so, press s. Or quit while you can by pressing q. There's no dishonor in a tactical retreat."

stage C1aHelp name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into an animatronic. Your shield will protect you, {name}, but without a map it's going to be difficult to guess which way to go. You sure you want to do this? If so, press s. Or quit while you can by pressing q. There's no shame in a tactical retreat."

stage C1wHelp name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into an animatronic. And you know how dangerous that is, don't you? Especially since you have no shield to defend yourself. Luckily you have a map to guide you, {name}. You sure you want to do this? If so, press s. Or quit while you can by pressing q. A tactical retreat does not make a coward."

stage C1awHelp name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into an animatronic. Your shield will protect you and your map will guide you, but you know how dangerous it can be, don't you, {name}? Press s to continue. Or quit by pressing q."

stage C1 name = T.replace "{name}" name "You arrived at a cliff. You can't see the bottom. Better not fall in. Even though this is an ice cream parlor. There's 3 platforms that you could reach... by jumping. To your left, forward, and right. They're too far away from each other. And each has a button. How do you know where to go? Just... pick a direction? To jump left, press a. Press w to jump forward. Or d for right."
stage C1a name = T.replace "{name}" name "You arrived at a cliff. You can't see the bottom. Better not fall in. Even though this is an ice cream parlor. There's 3 platforms that you could reach... by jumping. To your left, forward, and right. They're too far away from each other. And each has a button. How do you know where to go? Just... pick a direction? To jump left, press a. Press w to jump forward. Or d for right."
stage C1w name = T.replace "{name}" name "You arrived at a cliff. You can't see the bottom. Better not fall in. Even though this is an ice cream parlor. There's 3 platforms that you could reach... by jumping. To your left, forward, and right. They're too far away from each other. And each has a button. Let's check the map. It says to jump left. To jump left, press a. Press w to jump forward. Or d for right."
stage C1aw name = T.replace "{name}" name "You arrived at a cliff. You can't see the bottom. Better not fall in. Even though this is an ice cream parlor. There's 3 platforms that you could reach... by jumping. To your left, forward, and right. They're too far away from each other. And each has a button. Let's check the map. It says to jump left. To jump left, press a. Press w to jump forward. Or d for right."

stage C1f name = T.replace "{name}" name c1f1Msg
stage C1fa name = T.replace "{name}" name c1f1Msg
stage C1fw name = T.replace "{name}" name c1f1Msg
stage C1faw name = T.replace "{name}" name c1f1Msg

stage C1fDeath name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! How foolish! The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren't pillows. They're human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you died. A teddy bear clubbed you upside the head with an ice scoop. Wasn't there a shield you could've used to protect yourself? Press a to jump back to yesterday and find it! Or press q to give up and quit."

stage C1faAttack name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! Now hold up that shield. The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren't pillows. They're human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you got attacked. A teddy bear swung at your head with an ice scoop. To block with your shield and dodge, press d now!"

stage C1faDeath name = T.replace "{name}" name "You should've blocked and dodged. Ah well. Too late now. The teddy bear is scooping out your bones with its ice scoop. Which works surprisingly well. And is every bit as painful as you can imagine. And the bunny and cat have gathered around to observe and assist. You'd scream. If you hadn't succumbed to the pain. Time to reincarnate, {name}. Press a to do so. Or press q to quit."

stage C1faSurvive name = T.replace "{name}" name "Just in time! The ice scoop slides off the shield. Now do you feel like climbing up that ladder? Press s. Hurry up: that cat and bunny look like they're about to attack!"

stage C1fwDeath name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! How foolish! The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren't pillows. They're human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you died. A teddy bear clubbed you upside the head with an ice scoop. Your map didn't help at all! Wasn't there a shield you could have picked up yesterday? Press a to jump back in time, and find it! Or press q to give up and quit."

stage C1fawAttack name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! Now hold up that shield. The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren't pillows. They're human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you got attacked. A teddy bear swung at your head with an ice scoop. To block with your shield and dodge, press d now! Or to catch it with your map, press w."

stage C1fawDeath name = T.replace "{name}" name "You should've blocked and dodged. Ah well. Too late now. The teddy bear is scooping out your bones with its ice scoop. Which works surprisingly well. And is every bit as painful as you can imagine. And the bunny and cat have gathered around to observe and assist. You'd scream. If you hadn't succumbed to the pain. Time to reincarnate, {name}. Press a to do so. Or press q to quit."

stage C1fawSurvive name = T.replace "{name}" name "Just in time! The ice scoop slides off the shield. Now do you feel like climbing up that ladder? Press s. Hurry up: that cat and bunny look like they're about to attack!"

c1f1Msg = "You missed! You fall! Squishy pillows safely catch you. Around you, small lights appear in pairs, at eye level. The pairs start moving and approach you. A children's song gets louder as the lights close in. You recognize the whirring of machinery: animatronics. You also recognize the smell: blood. Uh-oh. Let's get out of here, {name}. You reach around and find a ladder. Press s to climb up to the platform."

b4EoSMsg = "The door bell rings. Would that be thieves? You walk to the ice cream parlor. Halfway there, you get met by a familiar face. It's your employer, John Masky. What's he doing here at this hour? He says: \"Hey there, {name}! How's it going? Shift's over for today. Anything problematic to report?\" Hmm... You didn't see that video, did you? So there's nothing to report, is there? Masky accepts your silence and continues: \"Good going. See you tomorrow night then! Same ice cream time, same ice cream channel!\" He laughs. Like he made a joke. Probably a quaint reference to some old book or TV show. Anyway: press w to continue into night 3!"

b4pEoSMsg = "The door bell rings. Would that be thieves? You walk to the ice cream parlor. Halfway there, you get met by a familiar face. It's your employer, John Masky. What's he doing here at this hour? He says: \"Hey there, {name}! How's it going? Shift's over for today. Anything problematic to report?\" Hmm... You saw that video, didn't you? Do you feel like telling Masky? If so, press y. Or press n to stay silent and pretend you didn't see nothing, and jump straight into night 3!"

b4RidiculeMsg = "So you tell Masky about the video. About the other security guard. About his concern for the animatronics. And how it looked like he got murdered. Masky takes it in, mulls it over, and responds: \"Murder? That's a serious allegation, {name}. By my animatronics, though? They're robots! They're only programmed to serve ice cream and sing children's songs! Do you really think that if they'd murder anyone, I'd still be in business? Come on now! Sounds like someone pranked you good! Tomorrow is going to be a busy day! See you at night fall! Same ice cream time, same ice cream channel!\" He laughs like he made a good joke. Probably some quaint reference to an old book or TV show. What do you do? Be upset with Masky and quit? If so, press q. Otherwise, press w to start night 3."

c1Msg = "Welcome back, {name}. This is night 3. Good to have you here. John Masky, your employer, promised today would be busy. He left you a letter. Maybe that will tell you what to expect? To read it, press h. You don't have to read it. Ignore it! Press w."


