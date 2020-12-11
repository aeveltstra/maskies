{-# LANGUAGE OverloadedStrings #-}

{- |
    Maskies is a text adventure horror survival game.
    Try not to die.
    This file contains the game’s stages, their variations, and how they wire together.
    @author A.E.Veltstra
    @copyright A.E.Veltstra & T.R.Veltstra
    @version 2.20.1210.1855
-}
module Stages where

import Prelude
import qualified Data.Text as T
import qualified TextHelper as TH
import qualified Keys as K 

{- | Let’s use Haskell’s type system to determine exactly which stages the game can have. If we forget one, the compiler will complain. Unfortunately it does not complain if we forget to wire up a stage in the function ‘next’. But it does help when showing stages: the compiler automatically constrains us to only show stages that have been defined here. -}
data Stage = 
  Init
  | A1Help
  | A1DarkHallway
  | A1LightAppears
  | A1HallwayDeath
  | B1Intro
  | B1DarkHallway
  | B1Help
  | B1Office
  | B1OfficeDesk
  | B1Locker
  | B1LockerDeath
  | B1OfficeFountain
  | B1ViewMirror
  | B1TheParlor
  | B1TheParlorDeath
  | B1DarkHallwayEnd
  | B1DarkToilets
  | B1DarkHallwayDeath
  | B2Office
  | B2OfficeFountain
  | B2ViewMirror
  | B2OfficeDesk
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
  | B3Office
  | B3adwOffice
  | B3OfficeDesk
  | B3aOfficeDesk
  | B3adOfficeDesk
  | B3awOfficeDesk
  | B3adwOfficeDesk
  | B3dOfficeDesk
  | B3dwOfficeDesk
  | B3wOfficeDesk
  | B3OfficeFountain
  | B3PressedButton
  | B3OfficeDeath
  | B4aPressedButton
  | B4dPressedButton
  | B4wPressedButton
  | B4awPressedButton
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
  | B4Office
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
  | C2f
  | C2fa
  | C2fw
  | C2faw
  | C2fAttack
  | C2faAttack
  | C2fwAttack
  | C2fawAttack
  | C2fDeath
  | C2faDeath
  | C2fwDeath
  | C2fawDeath
  | C2fSurvive
  | C2faSurvive
  | C2fwSurvive
  | C2fawSurvive
  | C3
  | C3a
  | C3w
  | C3aw
  | C3f
  | C3fa
  | C3fw
  | C3faw
  | C3fDeathBelow
  | C3faDeathBelow
  | C3fawDeathBelow
  | C3fwDeathBelow
  | C3fDeathAbove
  | C4
  | C4a
  | C4w
  | C4aw
  | C4f
  | C4fa
  | C4fw
  | C4faw
  | C5
  | C5a
  | C5w
  | C5aw
  | C6
  | C6a
  | C6w
  | C6aw
  | C6f
  | C6fa
  | C6faw
  | C6fw
  | C6Attack
  | C6aAttack
  | C6awAttack
  | C6wAttack
  | C6Death
  | C6aDeath
  | C6awDeath
  | C6Survive
  | C6aSurvive
  | C6awSurvive
  | C7
  | C7a
  | C7aw
  | C7w
  | C8
  | C8a
  | C8aw
  | C8w
  | C9
  | C9a
  | C9aw
  | C9w
  | C10
  | C10a
  | C10w
  | C10aw
  | C10Attack
  | C10aAttack
  | C10wAttack
  | C10awAttack
  | C10Death
  | C10aDeath
  | C10wDeath
  | C10awDeath
  | C11
  | C11a
  | C11w
  | C11aw
  | D1Intro
  | D1Help
  | D1DarkHallway
  | D1DarkHallwayEnd
  | D1DarkToilets
  | D1DarkToiletsAttack
  | D1DarkToiletsDeath
  | D1DarkToiletsSurvive
  | D1DarkParlor
  | D1DarkOffice
  | D1DarkDesk
  | D1DarkDeskHelp
  | D1DarkLocker
  | D1DarkLockerAttack
  | D1DarkFountain
  | D1DarkDrink
  | D1DarkMirror
  | D1DarkMirrorAttack
  | D1DarkMirrorAvoid
  | D1DarkMirrorDeath
  | D1DarkMazeEntrance
  | D1DarkMazeAttack
  | D1DarkMazeDeath
  | D2LitDesk
  | D2LitDeskHelp
  | D2LitLocker
  | D2LitLockerUniform
  | D2LitOffice
  | D2LitOfficeHelp
  | D2LitFountain
  | D2LitMirror
  | D2LitMirrorAttack
  | D2LitMirrorDeath
  | D2LitMirrorSurvive
  | D2LitMazeEntrance
  | D3DarkDesk
  | D3DarkDeskHelp
  | D3DarkDeskVideo
  | D3DarkOffice
  | D4LitLocker
  | D4LitOffice
  | D4LitOfficeHelp
  | D4LitFountain
  | D4LitMirror
  | D4LitMirrorAttack
  | D4LitMirrorSurvive
  | D4LitMazeEntrance
  | Quit
  deriving (Bounded, Read, Show, Eq, Enum)

{- | This function wires Key events to stages, to determine which stage to show next. If the event is Wait, the same stage gets returned as put in. If the event is Q, the function returns the Quit event. If the player enters a key that is not wired up to a stage here, the function throws an error. -}
next :: Stage -> K.Key -> Stage
next theStage K.Wait = theStage
next _ K.Q = Quit
next Init _ = A1DarkHallway
next A1DarkHallway K.H = A1Help
next A1Help K.J2 = B1DarkHallway
next A1Help K.J3 = C1aw
next A1Help K.J4 = D1Intro
next A1Help _ = A1LightAppears
next A1DarkHallway _ = A1LightAppears
next A1LightAppears _ = A1HallwayDeath
next A1HallwayDeath _ = B1Intro
next B1Intro K.A = B1Office
next B1Intro K.W = B1DarkHallwayEnd
next B1Intro K.D = B1TheParlor
next B1Intro _ = B1DarkHallway
next B1DarkHallway K.A = B1Office
next B1DarkHallway K.W = B1DarkHallwayEnd
next B1DarkHallway K.D = B1TheParlor
next B1DarkHallway _ = B1DarkHallway
next B1Help _ = B1Office
next B1Office K.D = B1OfficeDesk
next B1Office K.A = B1Locker
next B1Office K.W = B1OfficeFountain
next B1Office K.H = B1Help
next B1Office _ = B1DarkHallway
next B1OfficeDesk K.Y = B2OfficeDesk
next B1OfficeDesk _ = B1Office
next B1Locker K.D = B1OfficeDesk
next B1Locker _ = B1LockerDeath
next B1LockerDeath K.A = B1Office
next B1LockerDeath _ = Quit
next B1OfficeFountain K.W = B1ViewMirror
next B1OfficeFountain _ = B1Office
next B1ViewMirror _ = B1Office
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
next B2LitHallway K.A = B2Office
next B2LitHallway K.W = B2LitHallwayEnd
next B2LitHallway K.D = B2TheParlor
next B2LitHallway _ = B2LitHallway
next B2Office K.S = B2LitHallway
next B2Locker K.Y = B2InspectSupplies
next B2Locker _ = B3Office
next B2InspectSupplies K.A = B2aFind
next B2InspectSupplies K.W = B2wFind
next B2InspectSupplies K.D = B2dFind
next B2InspectSupplies _ = B3Office
next B2aFind K.W = B2awFind
next B2aFind K.D = B2adFind
next B2aFind _ = B3aOfficeDesk
next B2wFind K.A = B2waFind
next B2wFind K.D = B2wdFind
next B2wFind _ = B3wOfficeDesk
next B2dFind K.A = B2daFind
next B2dFind K.W = B2dwFind
next B2dFind _ = B3dOfficeDesk
next B2awFind K.D = B2awdFind
next B2awFind _ = B3awOfficeDesk
next B2adFind K.W = B2adwFind
next B2adFind _ = B3adOfficeDesk
next B2daFind K.W = B2adwFind
next B2daFind _ = B3adOfficeDesk
next B2dwFind K.A = B2dwaFind
next B2dwFind _ = B3dwOfficeDesk
next B2waFind K.D = B2awdFind
next B2waFind _ = B3awOfficeDesk
next B2wdFind K.A = B2dwaFind
next B2wdFind _ = B3dwOfficeDesk
next B2awdFind _ = B3adwOfficeDesk
next B2adwFind _ = B3adwOfficeDesk
next B2dwaFind _ = B3adwOfficeDesk
next B3aOfficeDesk K.W = B2awFind
next B3aOfficeDesk K.D = B2adFind
next B3aOfficeDesk K.Y = B4aPressedButton
next B3aOfficeDesk _ = B4aEndOfShift
next B3dOfficeDesk K.A = B2daFind
next B3dOfficeDesk K.W = B2dwFind
next B3dOfficeDesk K.Y = B4dPressedButton
next B3dOfficeDesk _ = B4EndOfShift
next B3wOfficeDesk K.A = B2waFind
next B3wOfficeDesk K.D = B2wdFind
next B3wOfficeDesk K.Y = B4wPressedButton
next B3wOfficeDesk _ = B4wEndOfShift
next B3awOfficeDesk K.D = B2awdFind
next B3awOfficeDesk K.Y = B4awPressedButton
next B3awOfficeDesk _ = B4awEndOfShift
next B3adOfficeDesk K.W = B2adwFind
next B3adOfficeDesk K.Y = B4aPressedButton
next B3adOfficeDesk _ = B4aEndOfShift
next B3dwOfficeDesk K.A = B2dwaFind
next B3dwOfficeDesk K.Y = B4wPressedButton
next B3dwOfficeDesk _ = B4wEndOfShift
next B3adwOfficeDesk K.Y = B4awPressedButton
next B3adwOfficeDesk _ = B4awEndOfShift
next B2Office K.D = B2OfficeDesk
next B2Office K.A = B2Locker
next B2Office K.W = B2OfficeFountain
next B2Office _ = B2Office
next B2OfficeFountain K.W = B2ViewMirror
next B2OfficeFountain _  = B2Office
next B2OfficeDesk K.Y = B2ReadLetter
next B2OfficeDesk _ = B2Office
next B2ReadLetter _ = B2Office
next B2ViewMirror K.S = B2Office
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
next B3Office K.D = B3OfficeDesk
next B3Office _ = B3OfficeFountain
next B3OfficeDesk K.Y = B3PressedButton
next B3OfficeDesk _ = B3OfficeFountain
next B3PressedButton K.Y = B2Locker
next B3PressedButton _ = B3OfficeFountain
next B3OfficeFountain _ = B3OfficeDeath
next B3OfficeDeath _ = Quit
next B4dPressedButton _ = B4pEndOfShift
next B4aPressedButton _ = B4paEndOfShift
next B4wPressedButton _ = B4pwEndOfShift
next B4awPressedButton _ = B4pawEndOfShift
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
next B4Ridicule _ = C1Welcome
next B4aRidicule _ = C1aWelcome
next B4wRidicule _ = C1wWelcome
next B4awRidicule _ = C1awWelcome
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
next C2 K.W = C3
next C2 K.S = C1f
next C2 _ = C2f
next C2a K.W = C3a
next C2a K.S = C1fa
next C2a _ = C2fa
next C2w K.W = C3w
next C2w K.S = C1fw
next C2w _ = C2fw
next C2aw K.W = C3aw
next C2aw K.S = C1faw
next C2aw _ = C2faw
next C2f K.D = C3
next C2f _ = C2fAttack
next C2fa K.D = C3a
next C2fa _ = C2faAttack
next C2fw K.D = C3w
next C2fw _ = C2fwAttack
next C2faw K.D = C3aw
next C2faw _ = C2fawAttack
next C2fAttack K.D = C2fSurvive
next C2fAttack _ = C2fDeath
next C2fDeath K.A = C1
next C2fDeath _ = Quit
next C2fSurvive K.A = C3
next C2fSurvive _ = C2fSurvive
next C2faAttack K.D = C2faSurvive
next C2faAttack _ = C2faDeath
next C2faDeath K.A = C1a
next C2faDeath _ = Quit
next C2faSurvive K.A = C3a
next C2faSurvive _ = C2faSurvive
next C2fwAttack K.D = C2fwSurvive
next C2fwAttack _ = C2fwDeath
next C2fwDeath K.A = C1w
next C2fwDeath _ = Quit
next C2fwSurvive K.A = C3w
next C2fwSurvive _ = C2fwSurvive
next C2fawAttack K.D = C2fawSurvive
next C2fawAttack _ = C2fawDeath
next C2fawDeath K.A = C1aw
next C2fawDeath _ = Quit
next C2fawSurvive K.A = C3aw
next C2fawSurvive _ = C2fawSurvive
next C3 K.D = C4
next C3 _ = C3f
next C3a K.D = C4a
next C3a _ = C3fa
next C3w K.D = C4w
next C3w _ = C3fw
next C3aw K.D = C4aw
next C3aw _ = C3faw
next C3f K.W = C3fDeathBelow
next C3fa K.W = C3faDeathBelow
next C3faw K.W = C3fawDeathBelow
next C3fw K.W = C3fwDeathBelow
next C3f _ = C3fDeathAbove
next C3fa _ = C3fDeathAbove
next C3faw _ = C3fDeathAbove
next C3fw _ = C3fDeathAbove
next C3fDeathBelow K.A = C1
next C3fDeathBelow _ = Quit
next C3faDeathBelow K.A = C1a
next C3faDeathBelow _ = Quit
next C3fawDeathBelow K.A = C1aw
next C3fawDeathBelow _ = Quit
next C3fwDeathBelow K.A = C1w
next C3fwDeathBelow _ = Quit
next C3fDeathAbove K.A = C1
next C3fDeathAbove _ = Quit
next C4 K.S = C3f
next C4 K.A = C5
next C4 K.D = C6
next C4 _ = C6f
next C4a K.S = C3fa
next C4a K.A = C5a
next C4a K.D = C6a
next C4a _ = C6fa
next C4w K.S = C3fw
next C4w K.A = C5w
next C4w K.D = C6w
next C4w _ = C6fw
next C4aw K.S = C3faw
next C4aw K.A = C5aw
next C4aw K.D = C6aw
next C4aw _ = C6faw
next C5 K.S = C6
next C5a K.S = C6a
next C5aw K.S = C6aw
next C5w K.S = C6w
next C5 K.W = C7
next C5a K.W = C7a
next C5aw K.W = C7aw
next C5w K.W = C7w
next C5 _ = C5
next C5a _ = C5a
next C5aw _ = C5aw
next C5w _ = C5w
next C6 K.W = C8
next C6a K.W = C8a
next C6aw K.W = C8aw
next C6w K.W = C8w
next C6 _ = C6f
next C6a _ = C6fa
next C6aw _ = C6faw
next C6w _ = C6fw
next C6f K.N = C6Survive
next C6f _ = C6Attack
next C6fa K.N = C6aSurvive
next C6fa _ = C6aAttack
next C6faw K.N = C6awSurvive
next C6faw _ = C6awAttack
next C6fw K.N = C6Survive
next C6fw _ = C6Attack
next C6Attack K.Y = C6Survive
next C6Attack _ = C6Death
next C6aAttack K.Y = C6aSurvive
next C6aAttack _ = C6aDeath
next C6awAttack K.Y = C6awSurvive
next C6awAttack _ = C6awDeath
next C6wAttack K.Y = C6Survive
next C6wAttack _ = C6Death
next C6Death K.A = C1
next C6Death _ = Quit
next C6aDeath K.A = C1a
next C6aDeath _ = Quit
next C6awDeath K.A = C1aw
next C6awDeath _ = Quit
next C6Survive K.W = C8
next C6Survive _ = C6Death
next C6aSurvive K.W = C8a
next C6aSurvive _ = C6aDeath
next C6awSurvive K.W = C8aw
next C6awSurvive _ = C6awDeath
next C7 K.N = C6
next C7 K.Y = C9
next C7 _ = C7
next C7a K.N = C6a
next C7a K.Y = C9a
next C7a _ = C7a
next C7w K.N = C6w
next C7w K.Y = C9w
next C7w _ = C7w
next C7aw K.N = C6aw
next C7aw K.Y = C9aw
next C7aw _ = C7aw
next C8 K.S = C1
next C8a K.S = C1a
next C8aw K.S = C1aw
next C8w K.S = C1w
next C8 _ = C11
next C8a _ = C11a
next C8w _ = C11w
next C8aw _ = C11aw
next C9 K.W = C10
next C9 _ = C6
next C9a K.W = C10a
next C9a _ = C6a
next C9w K.W = C10w
next C9w _ = C6w
next C9aw K.W = C10aw
next C9aw _ = C6aw
next C10 K.W = C6
next C10a K.W = C6a
next C10w K.W = C6w
next C10aw K.W = C6aw
next C10 K.S = C10Attack
next C10a K.S = C10aAttack
next C10aw K.S = C10awAttack
next C10w K.S = C10wAttack
next C10Attack K.Y = C6
next C10aAttack K.Y = C6a
next C10wAttack K.Y = C6w
next C10awAttack K.Y = C6aw
next C10Attack _ = C10Death
next C10aAttack _ = C10aDeath
next C10wAttack _ = C10wDeath
next C10awAttack _ = C10awDeath
next C10Death K.A = C1
next C10aDeath K.A = C1a
next C10wDeath K.A = C1w
next C10awDeath K.A = C1aw
next C11 _ = D1Intro
next C11a _ = D1Intro
next C11aw _ = D1Intro
next C11w _ = D1Intro
next D1Intro K.W = D1DarkHallway
next D1Intro _ = D1Help
next D1Help _ = D1DarkHallway
next D1DarkHallway K.H = D1Help
next D1DarkHallway K.W = D1DarkHallwayEnd
next D1DarkHallway K.A = D1DarkOffice
next D1DarkHallway K.D = D1DarkParlor
next D1DarkHallway _ = D1DarkHallway
next D1DarkHallwayEnd K.S = D1DarkHallway
next D1DarkHallwayEnd K.D = D1DarkParlor
next D1DarkHallwayEnd K.A = D1DarkToilets
next D1DarkHallwayEnd _ = D1Help
next D1DarkOffice K.A = D1DarkLocker
next D1DarkOffice K.W = D1DarkFountain
next D1DarkOffice K.D = D1DarkDesk
next D1DarkOffice _ = D1Help
next D1DarkLocker K.S = D1DarkOffice
next D1DarkLocker _ = D1DarkLockerAttack
next D1DarkLockerAttack K.A = D1Intro
next D1DarkLockerAttack _ = Quit
next D1DarkToilets K.S = D1DarkHallwayEnd
next D1DarkToilets _ = D1DarkToiletsAttack
next D1DarkToiletsAttack K.S = D1DarkToiletsSurvive
next D1DarkToiletsAttack _ = D1DarkToiletsDeath
next D1DarkToiletsSurvive K.D = D1DarkHallway
next D1DarkToiletsSurvive K.W = D1DarkParlor
next D1DarkToiletsSurvive _ = D1DarkToiletsDeath
next D1DarkToiletsDeath K.A = D1Intro
next D1DarkToiletsDeath _ = Quit
next D1DarkFountain K.S = D1DarkOffice
next D1DarkFountain K.D = D1DarkDrink
next D1DarkFountain K.W = D1DarkMirror
next D1DarkFountain _ = D1DarkFountain
next D1DarkDrink K.W = D1DarkToilets
next D1DarkDrink K.S = D1DarkOffice
next D1DarkDrink _ = D1DarkMirrorAttack
next D1DarkMirror K.A = D1DarkMirrorAvoid
next D1DarkMirror _ = D1DarkMirrorAttack
next D1DarkMirrorAvoid K.A = D1DarkDesk
next D1DarkMirrorAvoid _ = D1DarkMirrorAttack
next D1DarkDesk K.Y = D2LitDesk
next D1DarkDesk K.N = D3DarkDesk
next D1DarkDesk _ = D1DarkDeskHelp
next D1DarkDeskHelp K.S = D1DarkDesk
next D1DarkDeskHelp _ = D1DarkDeskHelp
next D1DarkMirrorAttack _ = D1DarkMirrorDeath
next D1DarkMirrorDeath K.A = D1Intro
next D1DarkMirrorDeath _ = Quit
next D1DarkParlor K.S = D1DarkHallway
next D1DarkParlor _ = D1DarkMazeEntrance
next D1DarkMazeEntrance K.S = D1DarkHallway
next D1DarkMazeEntrance _ = D1DarkMazeAttack
next D1DarkMazeAttack K.S = D1DarkHallway
next D1DarkMazeAttack _ = D1DarkMazeDeath
next D1DarkMazeDeath K.A = D1Intro
next D1DarkMazeDeath _ = Quit
next D3DarkDesk K.H = D3DarkDeskHelp
next D3DarkDesk _ = D3DarkDesk
next D3DarkDeskHelp _ = D3DarkDeskVideo
next D3DarkDeskVideo _ = D3DarkOffice
next D3DarkOffice K.A = D1DarkHallway
next D3DarkOffice K.W = D1DarkParlor
next D3DarkOffice K.H = D1Help
next D3DarkOffice K.D = Quit
next D3DarkOffice _ = D3DarkOffice
next D2LitDesk K.H = D2LitDeskHelp
next D2LitDesk K.A = D2LitLocker
next D2LitDesk _ = D2LitOffice
next D2LitLocker _ = D2LitOffice
next D2LitOffice K.A = D2LitLocker
next D2LitOffice K.D = D2LitDesk
next D2LitOffice K.W = D2LitFountain
next D2LitOffice K.S = D2LitMazeEntrance
next D2LitOffice _ = D2LitOfficeHelp
next _ _ = error "Yet to wire up."

{- | These are the texts to show for each stage. This architecture assumes that the game loop outputs these texts and captures input from the player, to return to an other stage. -}
-- Not using a Show instance here, because that messes up the “next” function.
stage :: Stage -> T.Text -> T.Text 

stage Init _ = "Welcome to Maskie’s Ice Cream. What is your name? Type your name and press Enter:"

stage Quit name = T.replace "{name}" name "Come back to play another day, {name}? Bring your friends! Bye!"

stage A1DarkHallway name = T.replace "{name}" name "Hello, {name}. You’re in a dark hallway. It is night. Need help? Press h. To go forward: press w. To give up and quit: press q."

stage A1Help name = T.replace "{name}" name "This is a text adventure game. It makes you read a lot. Each scene may offer some hints and clues. Sometimes you can pick up items that may or may not help, later. After each scene, you get a choice for what to do next. Enter your choice to continue, or q to quit. Take your time. Take as long as you need. No really, {name}: think it through. To go back, press w."

stage A1LightAppears name = T.replace "{name}" name "At the end of the hallway, a light moves in. It brightens the opposite wall, which shows an image. It is too far away to recognize. The light comes from a lantern, held by a security guard. He shines it at the image on the wall. It’s an ice cream cone. He turns around and sees you. To go forward: press w. To give up and quit: q."

stage A1HallwayDeath name = T.replace "{name}" name "You take his lantern… but also his arm. Blood sprays from his torso onto the ice cream painting. He screams. It hurts your ears. You want to silence him. But his head is so very fragile. It breaks and falls off. The guard’s body sags to the tiled floor. You step backwards to avoid the spreading blood. Walking through the hallway, you see a mirror above the fountain. Let’s make sure no blood hit you. And if so, clean up. You see your face. You look kind-of like that guard. Not quite. Your eyes and ears are square, and your mouth has no lips to cover your teeth. Metal teeth. And so many! Confused you drop the lantern and disappear into a dark room. \n\nPress w to keep playing. Want to quit? Press q."

stage B1Intro name = T.replace "{name}" name "Hello, {name}. You’re a security guard. You got hired to keep this place secure. The hallway is dark: it is night. Security lights show the way to the exit, and light up just enough of the hallway to see it’s absurdly clean, and to spot the walls and doors. You hear some children’s music playing. From where? Speakers in the ceiling? And why? It’s night time! They should turn that off. Where do you want to go next? To go forward: press w. Press a to turn left. There’s a light there. To turn right, press d."

stage B1DarkHallway name = T.replace "{name}" name "You’re in the hallway. It’s dark. Security lights hint where to go. Forward: press w. Press a to turn left. There’s a light there. To turn right, press d."

stage B1Help name = T.replace "{name}" name "The letter reads: “Dear {name}, \nWelcome to Maskie’s Ice Cream! Great to have you on staff. Due a pandemic killing lots of people, I replaced all human employees with animatronics. Lots of them! They’re like robots, but better. And they look like stuffed animals for kids. That means no people touch the ice cream, thus no viruses get transmitted. Making Maskie’s Ice Cream very popular. So popular that thieves like to come and steal the ice cream. That’s where you come in, {name}. Your job is to keep out the thieves. See you tomorrow! \nSincerely, \nJacques Masquie, owner.” \nPress w to continue."

stage B1Office name = T.replace "{name}" name "You are in a lit office. By the right wall there is a desk with a lantern, a water fountain by the far wall, and a locker to the left. Your employer gave you a letter. To read it, press h. You don’t have to read it. You could ignore it and instead press w to drink water from the fountain. Or press d to study the desk. To inspect the locker, press a. To go back to the hallway, press s."

stage B1TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off. You can make out tables and cabinets in the light of the moon. And what’s that? In the shadows behind the counter? Is that a person? Could be a thief! To do your duty and inspect the situation, press w. However, you might be a coward. Are you a coward, {name}? If so, press s to go back to the hallway. It won’t be any better, though. Now choose."

stage B1TheParlorDeath name = T.replace "{name}" name "You get attacked! You can’t see them: it’s too dark! Wasn’t there a lantern in the office? You really could’ve used that now. The attacker grabs you by the neck, and pulls you really close to their face. ITs face. It’s an animatronic. With no lips on a mouth filled with sharp metal teeth. Cameras instead of eyes. With an ice cream cone for 1 ear, and some wires where the other should have been. With bulbous arms that grab you and instead of a hand, an ice scoop that scoops out your eyes. It rips out your brain and eats it. That’s going to make a mess! Now choose: to reincarnate and try again, press a. To give up and quit, press q."

stage B1DarkHallwayEnd name = T.replace "{name}" name "At the far end of the hallway, you see a wall. A painting might be there, but it’s too dark to make out. As a matter of fact, you can hardly see anything. Better go back, {name}, and find some source of light. Press s to go back. Or press d to go right. Want to go left? Press a. There is a light there. To give up and quit, press q."

stage B1DarkToilets name = T.replace "{name}" name "You found the toilets. That will come in handy. It’s a bit dark in here. It smells clean. The tiles squeak under your shoes. Very clean. Someone did a good job. If only you had some light to really enjoy that. You hear water trickling. It reminds you of your bladder. What do you do? Stay here and take a wee in the dark? Press w. Or ignore your bladder and go back into the hallway and look for a light? If so, press s."

stage B1DarkHallwayDeath name = T.replace "{name}" name "What’s that? In the shadows? It’s moving fast. It’s coming for you, {name}! You try and get away, but it’s dark. It jumps on you. You fall over backwards, cracking your head on the floor. The squeaky clean floor. Not so clean anymore. You have nothing to protect yourself. The attacker weighs down on your chest. You can’t breathe. You can’t see anything anymore. Desparately you grasp for air and try to push away your attacker. To no avail. Good bye, {name}. Press a to reincarnate and try again. Press q to give up and quit."

stage B1OfficeDesk name = T.replace "{name}" name "The desk holds a lantern. It’s the only light in the room. You probably should pick it up and carry it around as you roam the premises. You know: so you don’t stumble over anything in the dark. To pick up the lantern, press y. Otherwise, press n."

stage B1Locker name = T.replace "{name}" name "The locker is really dark. You can’t see anything. Wouldn’t you feel better with a lantern? Didn’t you see one on the desk? To check the desk, press d. To rummage around the locker anyway, press w. To give up and quit, press q."

stage B1LockerDeath name = T.replace "{name}" name "Ack! Something just grabbed you! You can’t see what it is: it’s too dark! It jumps on you. You fall over backwards, cracking your head on the edge of the desk. The desk with the lantern. The light makes your attacker visible. It’s… an animatronic!? A penguin? Why is it singing? Penguins can’t sing the ice cream song! Can they? Its beak shreds your clothes and your skin. You have nothing to protect yourself. Your blood shorts out the penguin. You faint. You die. Good try, {name}. Don’t give up now. Press a to reincarnate and try again. Press q to give up and quit."

stage B1OfficeFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. It’s getting hot in here! There’s a mirror on the wall in front of you. To lean forward and look into the mirror, press w. To return to the office, press s."

stage B1ViewMirror name = T.replace "{name}" name "You see yourself, {name}. But it’s dark and your face shows many shadows and wrinkles. How do you feel? Tired? Hot? The cold water helped a bit. You probably need to come back here to stay cool. Why is it hot in here anyway? It’s an ice cream parlor. Maybe the heat helps sell more ice cream? Beads of sweat drip down the side of your face, onto your shirt. You wipe them off. Enough self-reflection. Let’s press s and get back to work. Or, if you prefer to give up and quit, press q."

stage B2LitHallway name = T.replace "{name}" name "You’re back in the hallway. You’re still alone. It’s still night 2. Your lantern helps you see. Where to? Forward: press w. Left: a. Right: d. You could give up now and press q. But you’ve come so far!"

stage B2LitHallwayEnd name = T.replace "{name}" name "On the far wall at the end of the hallway, you see a painting of an ice cream cone. Everything is squeaky clean. The cleaning crew must be doing a good job. Surely that’s needed to keep the animatronics in good shape. On top of the ice cream in the painting, you spot a red ball with a yellow smiley face. Like a gumball. Wouldn’t you like to eat some ice cream now? It is getting hot in here, after all. Press y for yes, n for no."

stage B2Office name = T.replace "{name}" name "You are in a office. Your lantern lights it up. To the right you see a desk. Against the far wall you see a water fountain. A locker is set against the left wall. Press w to cool down with some cold water from the fountain. Press d to check the desk. To inspect the locker, press a. To go back to the hallway, press s."

stage B2Locker name = T.replace "{name}" name "The locker contains cleaning supplies and 2 sets of clothes: a spare security guard uniform, and a cleaner’s outfit. Wouldn’t you like to inspect the cleaning supplies? If yes, press y. But you know that curiosity kills the cat. So maybe not? If not, press n."

stage B2InspectSupplies name = T.replace "{name}" name "A broom… a mop… a bucket with rags… You shouldn’t need these, {name}. You’re a security guard. That’s odd: something shiny is sticking out of the rags. Would a security guard ignore that? To ignore it and close the locker, press s. Otherwise, pick where to rummage: press a, w, or d:"

stage B2aFind name = T.replace "{name}" name "You found a shield! It’s shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. Press s to study the shield at the desk. Still curious? Maybe pressing w or d will get you something else?"

stage B2waFind name = T.replace "{name}" name "You found a shield! It’s shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You already found a map. Press s to study them at the desk. Still curious? Maybe pressing d will get you something else?"

stage B2daFind name = T.replace "{name}" name "You found a shield! It’s shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You already found a memory stick. Press s to study them at the desk. Still curious? Maybe pressing w will get you something else?"

stage B2dwaFind name = T.replace "{name}" name "You found a shield! It’s shiny and pointy, a triangle with the point down, and fits snugly around your arm. {name}: you look tough now! It makes you feel invulnerable. Except for the painting of a poop tuft on the front. But maybe it will come in handy, later. So you keep the shield. You also found a memory stick and a map. That’s all there is to find. Press s to study them at the desk."

stage B2awFind name = T.replace "{name}" name "You found a map! It’s wrinkly and stained with water. People really have to take better care of their posessions. You already found a shield. Let’s have a closer look. Press s to study them at the desk. Still curious? Maybe pressing d will get you something else?"

stage B2dwFind name = T.replace "{name}" name "You found a map! It’s wrinkly and stained with water. People really have to take better care of their posessions. You already found a memory stick. Let’s have a closer look. Press s to study them at the desk. Still curious? Maybe pressing a will get you something else?"

stage B2adwFind name = T.replace "{name}" name "You found a map! It’s wrinkly and stained with water. People really have to take better care of their posessions. Let’s have a closer look. Press s to study your finds at the desk."

stage B2wFind name = T.replace "{name}" name "You found a map! It’s wrinkly and stained with water. People really have to take better care of their posessions. Let’s have a closer look. Press s to study it at the desk. Still curious? Maybe pressing a or d will get you something else?"

stage B2adFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you’ll need a USB port to plug that in. Press s to close the locker and return to the office. Still curious? You already found the shield. Maybe pressing w will get you something else?"

stage B2wdFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you’ll need a USB port to plug that in. You already found a map. Press s to study them at the desk. Still curious? Maybe pressing a will get you something else?"

stage B2dFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you’ll need a USB port to plug that in. Press s to study it at the desk. Still curious? Maybe pressing a or w will get you something else?"

stage B2awdFind name = T.replace "{name}" name "You found a memory stick! What would be stored on it? Must be important! Looks like you’ll need a USB port to plug that in. There is nothing else here. Press s to study your finds at the desk."

stage B2OfficeDesk name = T.replace "{name}" name "Lantern in hand, you now see a letter on the desk, with your name on it. Would you like to read it, {name}? If yes, press y. If not, press n."

stage B2ReadLetter name = T.replace "{name}" name "The letter reads: “Dear {name}, \nIf you read this, I will be dead. Chances are you will die too. Something wanders the hallways at night. \nDid Jacques Masquie tell you that you need to keep out thieves? He lied to you. It isn’t thieves that kill us. It’s something much worse. And don’t trust the animatronics. They may look like cuddly animals, but every now and then they scare me half to death. \nI’ve hidden some helpful items in the locker. You’ll need them tomorrow. \nGood luck.” \nItems you need tomorrow? Better go find them! Press s to go back and keep playing. Scared? Give up and quit! Press q."

stage B2OfficeFountain name = T.replace "{name}" name "Aah, fresh, clean water! You needed that. The heat is getting unbearable! There’s a mirror on the wall in front of you. Let’s check your hair. Got to look the part, after all. To lean forward and look into the mirror, press w. To return to the office, press s."

stage B2ViewMirror name = T.replace "{name}" name "You see yourself, {name}. The light from the lantern does you no favors. You straighten your hair. And spot something shoot by from the corner of your eye. What do you do? Chase it? Press w. Stay in the room and keep looking for other things that may help? Press s."

stage B2TheParlor name = T.replace "{name}" name "You are in the ice cream parlor. It is closed for the night. The lights are off, but your lantern spreads the light. You can make out tables and cabinets. And what’s that? In the shadows behind the counter? Is that an ice cream dispenser? Maskies does boast the best ice cream in the county! Let’s have some! Surely that will cool you down. Press y to eat some. Press s to return to the hallway. To quit, press q."

stage B2EatIcecream name = T.replace "{name}" name "This truly is the best ice cream ever. It’s clear why people would want to come here and have some. The ice cream tastes so good, that you forgot to wonder why the dispenser hadn’t been turned off. It’s midnight, after all. Shouldn’t it have been cleaned for hygiene? If you agree, press y. If not, press n."

stage B2FoodPoisoning name = T.replace "{name}" name "You don’t feel so good. Your stomach is growling. Better get to the bathroom, {name}, quickly! Press w to go visit the bathrooms. Or s to suck it up and ignore the pain."

stage B2DieFromFoodPoisoning name = T.replace "{name}" name "Best ice cream in the country, aye? Not so sure about that anymore, are you, {name}? Your belly swells up fast. It hurts! It explodes! Your guts get flung out all over the place. You fall down on your knees and then flat on your face. You would drown if the blood loss wouldn’t kill you first. Game over. Press q to quit. Or, to reincarnate and try again, press a. Don’t eat the ice cream, OK {name}?"

stage B2LitToilets name = T.replace "{name}" name "You found the toilets. Just in time! To go and do a number 2, pick any stall: a, w, or d. You’re all alone anyway. Or press s to back out now."

stage B2FeelBetterNow name = T.replace "{name}" name "Much better. What a relief! You sure could use cold drink of water now. Let’s head back, {name}. Press w to continue."

stage B2HallwayDeath name = T.replace "{name}" name "Oh how courageous! You ran after whatever it was, into the hallway. You can see it clearly now: it’s a pink animatronic cat! And it’s coming for you really fast! If only you had had something to protect yourself! But you don’t. Didn’t you see the shield in the office locker? Didn’t you take it? Why not? Too late now! The cat runs you over. It’s heavy. Way heavier than a real cat. Your bones crush and break as the cat claws into your body. Your scream is drowned out by the music. So loud! Then you faint. And die. Where does the cat go? Why did it attack you? No clue… yet. Want to find out? Press a to reincarnate and try again. Or run away and quit, by pressing q."

stage B3Office name = T.replace "{name}" name "No need to get your hands dirty, right {name}? After all you aren’t the cleaner. And the bucket won’t be stealing no ice cream. Alright. Let’s cool off with some cold water from the fountain. Press w. Or press d to take a break and sit down at the desk. Feel like quitting? Press q!"

stage B3OfficeDesk name = T.replace "{name}" name "Right. Let’s take a break. Securing the premises is tiring! You set the lantern down on the desk. That causes a small round shadow to appear. A shadow? You recognize it is cast by a button. What would that do? Want to find out, {name}? Press y for yes. Or be boring and press n for no."

stage B3PressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. It has a USB port on the side. But you have nothing to plug into it. Doesn’t it feel like you might be missing out on something important, {name}? Press y if so. If not, press n."

stage B3OfficeFountain name = T.replace "{name}" name "Suddenly you hear glass sliding on glass. The mirror mounted on the wall above the fountain slides away. The space behind it is dark. Something in there is making its way out into the light! Where you are, {name}! Jump out of the way, quick! Press a!"

stage B3OfficeDeath name = T.replace "{name}" name "Oh dear! It fell out, bumped on the fountain, and landed right on top of you! It’s a bunny? Blue and white, and fluffy… but big! And heavy! So heavy! It studies you. With square eyes. Does it seem confused? Hard to tell. It sniffs at your face. You struggle to push it off. Can’t… move! Can’t… breathe! Is that singing you hear? Crying? Can’t tell. Can’t… (Game over. Press q.)"

stage B3aOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. That causes a small round shadow to appear, that wasn’t there before. It’s a button. What do you do? To press the button, press y. Or maybe look for other objects hidden in the wash bucket? Get your hands dirty? If so, press d or w."

stage B3dOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. That causes a small round shadow to appear, that wasn’t there before. It’s a button. What do you do? To press the button, press y. Or maybe look for other objects hidden in the wash bucket? Get your hands dirty? If so, press a or w."

stage B3wOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. That causes a small round shadow to appear, that wasn’t there before. It’s a button. What do you do? To press the button, press y. Or maybe look for other objects hidden in the wash bucket? Get your hands dirty? If so, press a or d."

stage B3adOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. It lights up a button. What do you do? To press the button, press y. Or maybe look for another object hidden in the wash bucket? Get your hands dirty? If so, press w."

stage B3dwOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. It lights up a button. What do you do? To press the button, press y. Or maybe look for another object hidden in the wash bucket? Get your hands dirty? If so, press a."

stage B3awOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. It lights up a button. What do you do? To press the button, press y. Or maybe look for another object hidden in the wash bucket? Get your hands dirty? If so, press d."

stage B3adwOfficeDesk name = T.replace "{name}" name "You set your lantern down on the table. Next to it, you place the map and the memory stick. You keep the shield on your arm. Might come in handy. Now where can you plug in the memory stick? It looks like you need a USB port. On the desk you spot the round shadow of a button. Maybe that gives access to a USB port? To press the button, press y. To ignore it and resume guarding duty until the morning, press n."

stage B4aPressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. The screen starts playing a video. It’s another security guard. He seems afraid and tired. He shouts: “Get out! Get out now while you can! The animatronics! Ain’t no good! Far from! Someone’s done messed them up! Gotta figure out why and how to stop ’em! \nAnd the boss? Don’t let him trick you into the obstacle course! Bring a shield! Did you find the map? You’ll need it!” \nReally now? That’s a bit dramatic, isn’t it, {name}? The video continues: something jumped the security guard and they fell out of view. He’s screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B4dPressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. It has a USB port on the side. You plug in the memory stick. The screen starts playing a video. It’s another security guard. He seems afraid and tired. He shouts: “Get out! Get out now! The animatronics! They ain’t OK! Far from! Someone’s done messed them up. Gotta figure out why and how to stop ’em! \nAnd the boss? Don’t let him trick you into the obstacle course! Did you find the shield and the map? You’ll need those!” \nReally now? That’s a bit dramatic, isn’t it, {name}? The video continues: something jumped the security guard and they fell out of view. He’s screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B4wPressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. The screen starts playing a video. It’s another security guard. He seems afraid and tired. He shouts: “Get out! Get out now! The animatronics! They ain’t OK! No sir! Someone’s done messed them up. Gotta figure out why and how to stop ’em! \nAnd the boss? Don’t let him trick you into the obstacle course! Bring a map! Did you find the shield, too? You’ll need them!”. \nReally now? That’s a bit dramatic, isn’t it, {name}? The video continues: something jumped the security guard and they fell out of view. He’s screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

stage B4awPressedButton name = T.replace "{name}" name "Out of the middle of the desk, a screen flips up. The screen starts playing a video. It’s another security guard. He seems afraid and tired. He shouts: “Get out! Get out now! The animatronics! They ain’t OK! No sir! Someone’s done messed them up. Gotta figure out why and how to stop ’em! \nAnd the boss? Don’t let him trick you into the obstacle course! Bring a map and a shield! You’ll need them!”. \nReally now? That’s a bit dramatic, isn’t it, {name}? The video continues: something jumped the security guard and they fell out of view. He’s screaming. Is that the sound of wet rags dropping on the floor? Oh dear, blood spatters? You look around the room: no blood anywhere, now. The video stopped. Was it a prank? It must have been a prank! Press y if so. If not, press n."

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

stage C1MaskiesLetter name = T.replace "{name}" name "The letter reads: “How’s it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don’t know whether they’re still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That’s going to be difficult if you don’t have a map. And you’ll have to be careful to avoid the animatronics. Some of them haven’t been stored properly. I’ll be back tomorrow morning to debrief you! Signed, Jacques Masquie.” Quit while you can! Press q. Or keep playing: press w."

stage C1aMaskiesLetter name = T.replace "{name}" name "The letter reads: “How’s it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don’t know whether they’re still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That’s going to be difficult if you don’t have a map. But don’t worry about the animatronics: they’ve been stored for the night. I’ll be back tomorrow morning to debrief you! Signed, Jacques Masquie.” Quit while you can! Press q. Or keep playing: press w."

stage C1wMaskiesLetter name = T.replace "{name}" name "The letter reads: “How’s it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don’t know whether they’re still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That’s going to be easy if you have a map. And try and avoid the animatronics: some of them haven’t been stored properly. I’ll be back tomorrow morning to debrief you! Signed, Jacques Masquie.” Quit while you can! Press q. Or keep playing: press w."

stage C1awMaskiesLetter name = T.replace "{name}" name "The letter reads: “How’s it going, {name}? Ready for this? The customers got a special treat today: an obstacle course! Unfortunately we kind-of lost track of some. We don’t know whether they’re still in there. It is your job tonight to inspect the obstacle course and flush out any stragglers. That’s going to be easy if you have a map. And you won’t have to worry about any animatronics: they’ve been stored for the night. I’ll be back tomorrow morning to debrief you! Signed, Jacques Masquie.” Quit while you can! Press q. Or keep playing: press w."

stage C1Hallway name = T.replace "{name}" name "You’re in the hallway. Alone. You hear children’s music. Where is it coming from? You haven’t seen a radio anywhere, yet. To turn left, press a. To go forward, press w. Or press d to turn right."
stage C1aHallway name = T.replace "{name}" name "You’re in the hallway. Alone. You hear children’s music. Where is it coming from? You haven’t seen a radio anywhere, yet. Better strap up that shield. If only you had a map. That would’ve helped. To turn left, press a. To go forward, press w. Or press d to turn right."
stage C1wHallway name = T.replace "{name}" name "You’re in the hallway. Alone. You hear children’s music. Where is it coming from? You haven’t seen a radio anywhere, yet. Anyway, better pull up that map you found. Maybe that will help avoid the animatronics. To turn left, press a. To go forward, press w. Or press d to turn right."
stage C1awHallway name = T.replace "{name}" name "You’re in the hallway. Alone. You hear children’s music. Where is it coming from? You haven’t seen a radio anywhere, yet. Anyway, better strap up that shield. Also pull up that map: it should help guide you and avoid the animatronics. To turn left, press a. To go forward, press w. Or press d to turn right."

stage C1CourseStart name = T.replace "{name}" name "Erm. What? Smoke is blocking your view. It’s dissapating slowly. This is not at all where you expected to go. Did you take a wrong turn? Did you get teleported? Your stomach certainly feels like it. You look around. This looks like the start of the obstacle course. Oh no. You have nothing to protect yourself and no map to tell you where to go. This is not going to be fun. Press q to quit now! Or, if you’re brave, press a direction (a, w, d, or s). Right now is your chance to get some help though. Want it? Press h."

stage C1aCourseStart name = T.replace "{name}" name "Erm. What? Smoke is blocking your view. To make it dissapate more quickly, you flap your shield like a wing. This is not at all where you expected to go? Did you take a wrong turn? Did you get teleported? Your stomach certainly feels like it. You look around. It looks like the start of the obstacle course. It’s a good thing you have that shield strapped to your arm. If only you had a map. Press q to quit now! Or, if you’re brave, press a direction (a, w, d, or s). Or maybe… just maybe… you want to read the help. If so, press h."

stage C1wCourseStart name = T.replace "{name}" name "Erm. What? Smoke is blocking your view. To make it dissapate more quickly, you waft your map. This is not at all where you expected to go! Did you take a wrong turn? Did you get teleported? Your stomach certainly feels like it. You look around. It looks like the start of the obstacle course. It’s a good thing you have a map. But you have nothing to protect yourself. That’s not going to be fun. Press q to quit now! Or, if you’re brave, press a direction (a, w, d, or s). Or maybe… just maybe… you want to read the help. If so, press h."

stage C1awCourseStart name = T.replace "{name}" name "Erm… what? Smoke is blocking your view. To make it dissapate more quickly, you waft your map. This is not at all where you expected to go… Your stomach feels like you got teleported into the obstacle course. You check the shield on your arm: that will protect you. You check your map: that will guide you. Alright. Feeling brave? Choose a direction by pressing a, w, d, or s now. No need to press h and read the help, is there?"

stage C1Help name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into the animatronics. And you know how dangerous that is, don’t you? Especially since you have no shield to defend yourself, {name}. Also, without a map it’s going to be difficult to guess which way to go. You sure you want to do this? If so, press s. Or quit while you can by pressing q. There’s no dishonor in a tactical retreat."

stage C1aHelp name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into an animatronic. Your shield will protect you, {name}, but without a map it’s going to be difficult to guess which way to go. You sure you want to do this? If so, press s. Or quit while you can by pressing q. There’s no shame in a tactical retreat."

stage C1wHelp name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into an animatronic. And you know how dangerous that is, don’t you? Especially since you have no shield to defend yourself. Luckily you have a map to guide you, {name}. You sure you want to do this? If so, press s. Or quit while you can by pressing q. A tactical retreat does not make a coward."

stage C1awHelp name = T.replace "{name}" name "The obstacle course will make you jump a lot. You have to jump just right. If you jump wrong, you will run into an animatronic. Your shield will protect you and your map will guide you, but you know how dangerous it can be, don’t you, {name}? Press s to continue. Or quit by pressing q."

stage C1 name = T.replace "{name}" name "You arrived at a cliff. You can’t see the bottom. Better not fall in. Even though this is an ice cream parlor. There’s 3 platforms that you could reach… by jumping. To your left, forward, and right. They’re too far away from each other. And too dark to see anything there unless you’re on them. So how do you know where to go? Just… pick a direction? To jump left, press a. Press w to jump forward. Or d for right."
stage C1a name = T.replace "{name}" name "You arrived at a cliff. You can’t see the bottom. Better not fall in. Even though this is an ice cream parlor. There’s 3 platforms that you could reach… by jumping. To your left, forward, and right. They’re too far away from each other. And too dark to see anything there unless you’re on them. So how do you know where to go? Just… pick a direction? To jump left, press a. Press w to jump forward. Or d for right."
stage C1w name = T.replace "{name}" name "You arrived at a cliff. You can’t see the bottom. Better not fall in. Even though this is an ice cream parlor. There’s 3 platforms that you could reach… by jumping. To your left, forward, and right. They’re too far away from each other. And too dark to see anything there unless you’re on them. Let’s check the map. It says to jump left. To do so, press a. Press w to jump forward. Or d for right."
stage C1aw name = T.replace "{name}" name "You arrived at a cliff. You can’t see the bottom. Better not fall in. Even though this is an ice cream parlor. There’s 3 platforms that you could reach… by jumping. To your left, forward, and right. They’re too far away from each other. And too dark to see anything there unless you’re on them. Let’s check the map. It says to jump left. To do so, press a. Press w to jump forward. Or d for right."

stage C1f name = T.replace "{name}" name c1fMsg
stage C1fa name = T.replace "{name}" name c1fMsg
stage C1fw name = T.replace "{name}" name c1fMsg
stage C1faw name = T.replace "{name}" name c1fMsg

stage C1fDeath name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! How foolish! The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren’t pillows. They’re human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you died. A teddy bear clubbed you upside the head with an ice scoop. Wasn’t there a shield you could’ve used to protect yourself? Press a to jump back to yesterday and find it! Or press q to give up and quit."

stage C1faAttack name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! Now hold up that shield. The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren’t pillows. They’re human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you got attacked. A teddy bear swung at your head with an ice scoop. To block with your shield and dodge, press d now!"

stage C1faDeath name = T.replace "{name}" name "You should’ve blocked and dodged. Ah well. Too late now. The teddy bear is scooping out your bones with its ice scoop. Which works surprisingly well. And is every bit as painful as you can imagine. And the bunny and cat have gathered around to observe and assist. You’d scream. If you hadn’t succumbed to the pain. Time to reincarnate, {name}. Press a to do so. Or press q to quit."

stage C1faSurvive name = T.replace "{name}" name "Just in time! The ice scoop slides off the shield. Now do you feel like climbing up that ladder? Press s. Hurry up, {name}: that cat and bunny look like they’re about to attack!"

stage C1fwDeath name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! How foolish! The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren’t pillows. They’re human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you died. A teddy bear clubbed you upside the head with an ice scoop. Your map didn’t help at all! Wasn’t there a shield you could have picked up yesterday? Press a to jump back in time, and find it! Or press q to give up and quit."

stage C1fawAttack name = T.replace "{name}" name "No ladder escape for you, {name}? How brave! Now hold up that shield. The animatronics got closer. The lights in their eyes reveal that the pillows that caught you, aren’t pillows. They’re human bodies, torn apart. What happened to their bones? You almost vomit. Almost. Not because you stopped it. But because you got attacked. A teddy bear swung at your head with an ice scoop. To block with your shield and dodge, press d now! Or to catch it with your map, press w."

stage C1fawDeath name = T.replace "{name}" name "You should’ve blocked and dodged. Ah well. Too late now. The teddy bear is scooping out your bones with its ice scoop. Which works surprisingly well. And is every bit as painful as you can imagine. And the bunny and cat have gathered around to observe and assist. You’d scream. If you hadn’t succumbed to the pain. Time to reincarnate, {name}. Press a to do so. Or press q to quit."

stage C1fawSurvive name = T.replace "{name}" name "Just in time! The ice scoop slides off the shield. Now do you feel like climbing up that ladder? Press s. Hurry up, {name}: that cat and bunny look like they’re about to attack!"

stage C2 name = T.replace "{name}" name "Nice jumping! Good guess. You land safely. This platform is gross. There’s chunks of something all over. Don’t look too closely. And it smells like blood. Let’s get off of here before something bad happens. You can see other platforms, but not where they lead. No map to guide you, so you get to choose. Press a to jump left, w to jump forward, or d to jump to the right. You could try jumping backwards by pressing s. I wouldn’t: you have no shield to protect you from hostile animatronics."

stage C2a name = T.replace "{name}" name "Nice jumping! Good guess. You land safely. This platform is gross. There’s chunks of something all over. Don’t look too closely. And it smells like blood. Let’s get off of here before something bad happens. You can see other platforms, but not where they lead. No map to guide you, so you get to choose. Press a to jump left, w to jump forward, or d to jump to the right. You could try jumping backwards by pressing s."

stage C2w name = T.replace "{name}" name "Nice jumping! Your map was right. You land safely. This platform is gross. There’s chunks of something all over. Don’t look too closely. And it smells like blood. Let’s get off of here before something bad happens. You can see other platforms, but not where they lead. Check your map: does that say to jump forward? If you trust the map, press w. Otherwise, press a to jump left, or d to jump to the right. You could try jumping backwards by pressing s. I wouldn’t: you have no shield to protect you from hostile animatronics."

stage C2aw name = T.replace "{name}" name "Nice jumping! Your map was right. You land safely. This platform is gross. There’s chunks of something all over. Don’t look too closely. And it smells like blood. Let’s get off of here before something bad happens. You can see other platforms, but not where they lead. Check your map: does that say to jump forward? If you trust the map, press w. Otherwise, press a to jump left, or d to jump to the right. You could try jumping backwards by pressing s."

stage C3 name = T.replace "{name}" name "Oh good, you made it! But Your shoes. They’re sticky. You feel their soles: some thick liquid. You smell your fingers: blood! You must have stepped in some, earlier. Will that make it harder to jump? Yes it will! You wipe your hands on your pants, for lack of anything better. You hear metal clanging below. Like something is climbing a ladder. Want to stick around and find out what that is? If so, press s. If not, let’s jump! There’s 3 more platforms within reach. Choose: a for left, w for forward, or d for right."

stage C3a name = T.replace "{name}" name "Oh good, you made it! But. Your shoes. They’re sticky. You feel their soles: some thick liquid. You smell your fingers: blood! You must have stepped in some, earlier. Will that make it harder to jump? Yes it will! You wipe your hands on your pants: no need to get your shield yucky. You hear metal clanging below. Like something is climbing a ladder. Want to stick around and find out what it is? If so, press s. If not, let’s jump! There’s 3 more platforms within reach. Choose: a for left, w for forward, or d for right."

stage C3w name = T.replace "{name}" name "Oh good, you made it! But. Your shoes. They’re sticky. You feel their soles: some thick liquid. You smell your fingers: blood! You must have stepped in some, earlier. Will that make it harder to jump? Yes it will! You wipe your hands on your map. Was that smart? Now you can’t see where to go next, {name}! You hear metal clanging below. Like something’s climbing a ladder. Want to stick around and see what it is? If so, press s. If not, let’s jump! There’s 3 more platforms within reach. Choose: a for left, w for forward, or d for right. Does the map say to go left? Hard to tell…"

stage C3aw name = T.replace "{name}" name "Oh good, you made it! But. Your shoes. They’re sticky. You feel their soles: some thick liquid. You smell your fingers: blood! You must have stepped in some, earlier. Will that make it harder to jump? Yes it will! You wipe your hands on your map. No need to smear that all over your shield. But that makes it hard see where to go next, {name}! You hear metal clanging below. Like something’s climbing a ladder. Want to stick around to find out what it is? If so, press s. If not, let’s jump! There’s 3 more platforms within reach. Choose: a for left, w for forward, or d for right. Does the map say to go left?"

stage C2f name = T.replace "{name}" name (T.concat [c2fMsg, T.pack "You have nothing to protect you. Duck to the ground and reach around to find a ladder: press a, w, d, or s. Or give up and quit by pressing q."])
stage C2fa name = T.replace "{name}" name (T.concat [c2fMsg, T.pack "You waft your shield to keep the smoke away, but it barely works. Duck to the ground and reach around to find a ladder: press a, w, d, or s. Or give up and quit by pressing q."])
stage C2fw name = T.replace "{name}" name (T.concat [c2fMsg, T.pack "You waft your map to keep the smoke away. It gives you some time to check the map for a way out. To your right is a ladder. Press d to climb up. Or choose a different direction to go: a, s, or w."])
stage C2faw name = T.replace "{name}" name (T.concat [c2fMsg, T.pack "You waft your shield to keep the smoke away. It gives you some time to check your map for a way out. To your right is a ladder. Press d to climb up. Or choose a different direction to go: a, s, or w."])

stage C2fAttack name = T.replace "{name}" name (T.concat [c2fAttackMsg, T.pack "It lands on top of you, and licks your face. You struggle to push it off… and fail. Your uniform catches fire from the sparks. The cat is too heavy. You scream. That scares the cat off. You’re burning. Now what? Look around for an extinguisher? To do that, press a. Maybe find a blanket to wrap in? Press w. Or roll on the floor? Press d."])
stage C2faAttack name = T.replace "{name}" name (T.concat [c2fAttackMsg, T.pack "It lands on top of your shield, and attempts to lick your face. It slides to the floor, but your uniform catches fire from the sparks. You’re burning. Now what? Look around for an extinguisher? To do that, press a. Maybe find a blanket to wrap in? Press w. Or roll on the floor? Press d."])
stage C2fwAttack name = T.replace "{name}" name (T.concat [c2fAttackMsg, T.pack "It lands on top of you, and licks your face. You struggle to push it off… and fail. Your uniform catches fire from the sparks. The cat is too heavy. You scream. That scares the cat off. You’re burning. Now what? Look around for an extinguisher? To do that, press a. Maybe wrap yourself in your map? Press w. Or roll on the floor? Press d."])
stage C2fawAttack name = T.replace "{name}" name (T.concat [c2fAttackMsg, T.pack "It lands on top of your shield, and tries to lick your face. It slides to the floor, but your uniform catches fire from the sparks. You’re burning. Now what? Look around for an extinguisher? To do that, press a. Maybe wrap yourself in your map? Press w. Or roll on the floor? Press d."])

stage C2fDeath name = T.replace "{name}" name c2fDeathMsg
stage C2faDeath name = T.replace "{name}" name c2fDeathMsg
stage C2fwDeath name = T.replace "{name}" name c2fwDeathMsg
stage C2fawDeath name = T.replace "{name}" name c2fwDeathMsg

stage C2fSurvive name = T.replace "{name}" name c2fSurviveMsg
stage C2faSurvive name = T.replace "{name}" name c2fSurviveMsg
stage C2fwSurvive name = T.replace "{name}" name c2fSurviveMsg
stage C2fawSurvive name = T.replace "{name}" name c2fSurviveMsg

stage C3f name = T.replace "{name}" name c3fMsg
stage C3fa name = T.replace "{name}" name c3fMsg
stage C3faw name = T.replace "{name}" name c3fMsg
stage C3fw name = T.replace "{name}" name c3fMsg

stage C3fDeathBelow name = T.replace "{name}" name "Cold metal claws wrap around your arms and pull you under. You have nothing to protect yourself, but even if you had, those claws are too strong. The soggy, disgusting not-bean clumps smack into your face. They feel and smell like cuts of veal… or worse… This is an ice cream parlor after all: no veal here. That and other thoughts race through your mind. Weird. So this is game over, {name}. Want to try again? Press a to reincarnate and try again. Or press q to quit."
stage C3faDeathBelow name = T.replace "{name}" name "Cold metal claws wrap around your arms and pull you under. Your shield does nothing to protect you: those claws are too strong. The soggy, disgusting not-bean clumps smack into your face. They feel and smell like cuts of veal… or worse… This is an ice cream parlor after all: no veal here. That and other thoughts race through your mind. Weird. So this is game over, {name}. Want to try again? Press a to reincarnate and try again. Or press q to quit."
stage C3fawDeathBelow name = T.replace "{name}" name "Cold metal claws wrap around your arms and pull you under. Your shield does nothing to protect you: those claws are too strong. The soggy, disgusting not-bean clumps smack into your face. They feel and smell like cuts of veal… or worse… This is an ice cream parlor after all: no veal here. That and other thoughts race through your mind. Weird. So this is game over, {name}. Want to try again? Press a to reincarnate and try again. Or press q to quit."
stage C3fwDeathBelow name = T.replace "{name}" name "Cold metal claws wrap around your arms and pull you under. Your map does nothing to protect you: those claws are too strong. The soggy, disgusting not-bean clumps smack into your face. They feel and smell like cuts of veal… or worse… This is an ice cream parlor after all: no veal here. That and other thoughts race through your mind. Weird. So this is game over, {name}. Want to try again? Press a to reincarnate and try again. Or press q to quit."

stage C3fDeathAbove name = T.replace "{name}" name c3fDeathAboveMsg 

stage C4 name = T.replace "{name}" name c4Msg
stage C4a name = T.replace "{name}" name c4Msg
stage C4aw name = T.replace "{name}" name c4wMsg
stage C4w name = T.replace "{name}" name c4wMsg

stage C5 name = T.replace "{name}" name c5Msg
stage C5a name = T.replace "{name}" name c5Msg
stage C5w name = T.replace "{name}" name c5Msg
stage C5aw name = T.replace "{name}" name c5Msg

stage C6 name = T.replace "{name}" name (T.concat [c6Msg, "Which is pretty amazing since you have neither a shield nor a map. ", c6Msg2])
stage C6a name = T.replace "{name}" name (T.concat [c6Msg, "Which is pretty amazing since you have no map. But luckily your shield protected you. ", c6Msg2])
stage C6w name = T.replace "{name}" name (T.concat [c6Msg, "Aren’t you happy you have that map? Might have been easier with a shield, too. ", c6Msg2])
stage C6aw name = T.replace "{name}" name (T.concat [c6Msg, "That map and shield really helped. Better hang on to them. ", c6Msg2])

stage C6f name = T.replace "{name}" name (T.concat [c6fMsg, "You have nothing to protect yourself. No shield. How brave. ", c6fMsg2])
stage C6fa name = T.replace "{name}" name (T.concat [c6fMsg, "You hold up your shield. ", c6fMsg2])
stage C6faw name = T.replace "{name}" name (T.concat [c6fMsg, "You hold up your shield. ", c6fMsg2])
stage C6fw name = T.replace "{name}" name (T.concat [c6fMsg, "You pull out your map, rip it up, and throw it into the air in an attempt to distract them. ", c6fMsg2])

stage C6Attack name = T.replace "{name}" name (T.concat [c6AttackMsg, "And you ain’t got no shield. No nothing. ", c6AttackMsg2])
stage C6aAttack name = T.replace "{name}" name (T.concat [c6AttackMsg, "You have a shield. Will it help? ", c6AttackMsg2])
stage C6wAttack name = T.replace "{name}" name (T.concat [c6AttackMsg, "And you ain’t got no shield. No map. ", c6AttackMsg2])
stage C6awAttack name = T.replace "{name}" name (T.concat [c6AttackMsg, "You have a shield and a map. Will they help? ", c6AttackMsg2])

stage C6Survive name = T.replace "{name}" name c6SurviveMsg
stage C6aSurvive name = T.replace "{name}" name c6SurviveMsg
stage C6awSurvive name = T.replace "{name}" name c6SurviveMsg

stage C6Death name = T.replace "{name}" name (T.concat [c6DeathMsg, "You try and fend them off as best you can. ", c6DeathMsg2])
stage C6aDeath name = T.replace "{name}" name (T.concat [c6DeathMsg, "You fend them off with your shield, but there’s too many! ", c6DeathMsg2])
stage C6awDeath name = T.replace "{name}" name (T.concat [c6DeathMsg, "You fend them off with your shield, but there’s too many! You even rip up your map and throw it into the air as a distraction. It doesn’t work. ", c6DeathMsg2])

stage C7 name = T.replace "{name}" name c7Msg
stage C7a name = T.replace "{name}" name c7Msg
stage C7w name = T.replace "{name}" name c7Msg
stage C7aw name = T.replace "{name}" name c7Msg

stage C9 name = T.replace "{name}" name c9Msg
stage C9a name = T.replace "{name}" name c9Msg
stage C9w name = T.replace "{name}" name c9Msg
stage C9aw name = T.replace "{name}" name c9Msg

stage C10 name = T.replace "{name}" name (T.concat [c10Msg, "Don’t you wish you had a shield to protect you? Better run away!", c10Msg2])
stage C10a name = T.replace "{name}" name (T.concat [c10Msg, "Luckily you have a shield to protect you. Use that to block any attack. ", c10Msg2])
stage C10aw name = T.replace "{name}" name (T.concat [c10Msg, "Luckily you have a shield to protect you. Use that to block any attack. ", c10Msg2])
stage C10w name = T.replace "{name}" name (T.concat [c10Msg, "Don’t you wish you had a shield to protect you? Better run away!", c10Msg2])

stage C8 name = T.replace "{name}" name c8Msg
stage C8a name = T.replace "{name}" name c8Msg
stage C8w name = T.replace "{name}" name c8Msg
stage C8aw name = T.replace "{name}" name c8Msg

stage C10Attack name = T.replace "{name}" name (T.concat [c10AttackMsg, "If only you’d have had some shield or anything to protect you! ", c10AttackMsg2])
stage C10aAttack name = T.replace "{name}" name (T.concat [c10AttackMsg, "You have a shield. That could protect you. But you know what’s a wise action? ", c10AttackMsg2])
stage C10wAttack name = T.replace "{name}" name (T.concat [c10AttackMsg, "You have a map. Will that protect you? Wouldn’t you rather have had a shield? ", c10AttackMsg2])
stage C10awAttack name = T.replace "{name}" name (T.concat [c10AttackMsg, "You have a map. That won’t protect you! You have a shield too. That could. But you know a good choice? ", c10AttackMsg2])

stage C10Death name = T.replace "{name}" name (T.concat [c10DeathMsg, "You duck to the ground to avoid the smoke. ", c10DeathMsg2])
stage C10aDeath name = T.replace "{name}" name (T.concat [c10AttackMsg, "You duck to the ground to avoid the smoke, and raise your shield above your head. ", c10DeathMsg])
stage C10wDeath name = T.replace "{name}" name (T.concat [c10DeathMsg, "You duck to the ground to avoid the smoke, and cover your mouth and nose with the map, hoping it acts as a filter. ", c10DeathMsg2])
stage C10awDeath name = T.replace "{name}" name (T.concat [c10DeathMsg, "You duck to the ground to avoid the smoke, cover your mouth and nose with the map to act as a filter, and raise the shield above your head. ", c10DeathMsg2])

stage C11 name = T.replace "{name}" name (T.concat [c11Msg, "Masquie sees you lack any kind of protective gear. “How brave! Weren't you scared?” ", c11Msg2])
stage C11a name = T.replace "{name}" name (T.concat [c11Msg, "Masquie sees your shield. “Nice touch. Did it help?” ", c11Msg2])
stage C11w name = T.replace "{name}" name (T.concat [c11Msg, "Masquie sees your map. “Nice touch. Did it help?” ", c11Msg2])
stage C11aw name = T.replace "{name}" name (T.concat [c11Msg, "Masquie sees your shield and map. “Nice touch. Did they help?” ", c11Msg2])

stage D1Intro name = T.replace "{name}" name "This is night 4. You’re back in Maskie’s Ice Cream Parlor. I guess we did a bad job at scaring you off. We’ll try again! Your employer Jacques Masquie ran yet another special event for the customers. Maybe he left a letter about it on the desk in the office. Maybe not. Wouldn’t you like to find out, {name}? If yes, press w. For help, press h. Or press q to quit."

stage D1Help name = T.replace "{name}" name "Maskie’s Ice Cream Parlor is a text adventure game. It makes you read a lot. In night 4 you get to navigate a maze. Your objective is to stay alive and reach the exit. And maybe save the lives of some customers. Make sure to read the hints and clues, and pick up anything that might help you survive. At any point in the game you can type q to quit. Other keys will be announced when available. Good luck, {name}. Press s to continue."

stage D1DarkHallway name = T.replace "{name}" name "You’re in the hallway, {name}. You hear children’s music. They really ought to turn that off after hours. The hallway is dark, but for a few security lights. They show doors to the left and right, and more hallway forward. On the wall at the end of the hallway you see a painting. It looks different from before. Wouldn’t you like to study it from close by? If yes, press w. To turn left, press a. There’s a light there. Press d to turn right."

stage D1DarkHallwayEnd name = T.replace "{name}" name "You reached the end of the hallway. The music is louder here. The toilets are to the left. The ice cream parlor is to the right. And a painting is mounted on the wall in front of you. It looks different from before. That red gumball with the yellow smiley face… is it sticking out? And is that the source of the music? It may have holes for a speaker… Hard to tell, with so little light. Maybe head back and fetch a light, {name}? If so, press s. Or go left by pressing a. Press d to turn right."

stage D1DarkOffice name = T.replace "{name}" name "This is the office. It’s dark. A desk stands at the right wall. A lantern is set atop the desk. Would you like to inspect the desk? If so, press d. Against the left wall you see a storage locker. Press a to look through the locker. The far wall features a fountain with a mirror. To have a cold drink, press w. It is getting hot in here, again. Or if you choose to quit, {name}, press q."

stage D1DarkDesk name = T.replace "{name}" name "The desk holds a lantern. It’s the only light in the office besides the security beacon above the door. Wouldn’t you like to pick it up, so you don’t stumble around in the dark? Press y to pick it up. Press n to ignore it. I wouldn’t. But hey, I’m not playing."

stage D1DarkDeskHelp name = T.replace "{name}" name "No. Your options were to press either y or n. Press s now to go back to the desk and press the correct key. Want to quit? Press q!"

stage D1DarkLocker name = T.replace "{name}" name "This is the storage locker. There really isn’t enough light here to see what’s in it. 2 Days ago it held some uniforms, a broom, a mop, and a bucket. A uniform is missing. Do you really want to poke around in the dark? If so, press w. But wouldn’t you rather close the locker and look for a light? Press s to do that, {name}."

stage D1DarkLockerAttack name = T.replace "{name}" name "Hey, that hurt! What’s poking you? And from all sides? And the door closed behind you! You want to push it open, but your hand gets stabbed. You move your hand, and it gets stabbed again! There’s spikes or knives all over! And they’re getting closer! You feel around for an opening. Maybe duck? Jump up? Oh no: the entire locker falls over with you in it! Oh, dear. Erm. That’s a mess. Erm. So. I guess this is game over? Press a to reincarnate and try again, or q to quit."

stage D1DarkFountain name = T.replace "{name}" name "Ah yes, the fountain. Good for a refreshing cold drink. Someone should check out the airconditioning. Not you. Are you a mechanic? Thought so. Not hired to be one here. Here you are a security guard. Let’s go, do some securing. Press s to head back, {name}. Or maybe have that cold drink, first. Press d to drink. Or would you rather study your face in the mirror? If yes, press w."

stage D1DarkDrink name = T.replace "{name}" name "Nothing better than a cold drink on a hot night, right, {name}? But now you have to pee. Better go find those toilets. You know where they are, don’t you? Press w to go there now. Or would you rather ignore your bladder and pretend to be strong? You know you can’t hold it forever, you know. Press s to self-hypnotize and not feel the pressure."

stage D1DarkMirror name = T.replace "{name}" name "You reflect upon your face. Is this a trick mirror? It seems you grew a few more gray hairs since last time. This job must be getting to you. And what’s with the wrinkles around your eyes? The poor light really deepens those shadows, {name}. Suddenly something red and small jumps into the corner of the mirror. Turn around to face it, quick! Press a!"

stage D1DarkMirrorAvoid name = T.replace "{name}" name "The office is empty. Whatever red thing jumped into your view in the mirror, now it is gone. You feel your heart beating in your chest. Your ears are hot with blood. Surely you’re blushing now, and no-one can see. Because it’s too dark. Why didn’t you pick up that lantern yet, {name}? It’s on the desk. Right there. Press a to check the desk."

stage D1DarkMirrorAttack name = T.replace "{name}" name "Your choice. Not wise, though. You get bumped by someone or something you can’t even see. They knock you onto the floor. And they’re singing? To children’s music? They sit on your back: you can’t even look at their face. Ah! Pain! You got punched! Oh no, worse! They’re hammering on your back! It hurts! Scream! Struggle! Try and shove them off! Press s!"

stage D1DarkMirrorDeath name = T.replace "{name}" name "So sad. You never even got to see their face. Anyway, you’re dead now. What a mess. The cleaning crew is not going to be happy about that. You really should have picked up that lantern, {name}. But hey, what do I know? I just work here. So I guess this is game over. Good job though, getting this far. As a reward, if you reincarnate, you’ll jump straight back into night 4. Press a to do that. Or give up and quit, by pressing q."

stage D1DarkToilets name = T.replace "{name}" name "You found the toilets. And just in time! You realize your bladder has been bugging you for a while. It smells clean in here, but it’s a bit too dark for your liking. What do you do? Go back to the office to pick up the lantern from the desk? Or stay here and pee in the dark? Press s to go back. Otherwise, {name}, pick a stall: a, w, or d."

stage D1DarkToiletsAttack name = T.replace "{name}" name "So. That stall. Why that one? Anyway, it’ll do, I’m sure. Alright. Go ahead. Do your thing. I’ll wait. \r\n\r\nDone yet, {name}? \r\n\r\nWhat’s that? Did you hear that? That buzzing? Almost like static electricity? And hammering now? What on earth? Pull up those pants! You gotta go! I mean: get out of here! Press s now!"

stage D1DarkToiletsDeath name = T.replace "{name}" name "Why? Is pressing the correct key so hard? You could have survived that, you know. But nooo, {name} has to rebel. Well. Tough. You’re dead. You got knocked upside the head by a flying red ball with a yellow smiley face. It hammered your head in. Gross: your brains splattered all over the toilet and the stall walls. Wanna try that again? Press a to reincarnate and have another go. Or give up! Press q to quit."

stage D1DarkToiletsSurvive name = T.replace "{name}" name "Just in time you duck out of the way. A red ball flies through the air, wielding a hammer, aiming for your head. It has a yellow smiley painted on. Haven’t you seen that somewhere before? Instead of your head, it flies into the door. And now it’s trying to hammer down that door. Better make a run for it. Press d to head back into the hallway, or w to jump into the ice cream parlor. Make up your mind, {name}! Choose now!"

stage D3DarkDesk name = T.replace "{name}" name "Strong-headed, are we? Good for you! Don’t let a game tell you what to do! Even though it strongly hints you in the best direction! Besides, there’s a letter on the desk that requires your attention. Wouldn’t you like to read it, {name}? Might help. If so, press h."

stage D3DarkDeskHelp name = T.replace "{name}" name "The letter is from your employer, Jacques Masquie. It reads: \r\n\r\n“Dear {name}, \r\nWe have received some customer complaints. Some people got left behind in the obstacle course yesterday. You should have found them and flushed them out. You failed. Some customers got hurt by our animatronics. We have had to turn those off, and pay the customers’ hospital bills. Do not let that happen again. \r\nSigned, \r\nJacques Masquie, owner.” \r\nErm. OK, then. Press s to put down the letter. Or admit defeat and press q to quit."

stage D3DarkDeskVideo name = T.replace "{name}" name "You twiddle your thumbs on the desk in real authentic security guard fashion. Accidentally your one thumb hits a button. You knew it was there from the other day. A screen rises up from the desk. It starts playing a video. It shows a different security guard. She wears a uniform like your’s. But something is wrong with her face: it’s too round, and too red, and her smile seems painted on in yellow. Oh no! That is not her face at all! Press s to close the screen. What does it mean?"

stage D3DarkOffice name = T.replace "{name}" name "Alright. Enough is enough. Let’s get out of here. This building has 2 exits: the side door and the parlor door. Press d to take a right turn and exit out the side door. Press w to go forward into the ice cream parlor. Press a to turn left instead. For reasons, I guess. Which way, {name}?"

stage D1DarkParlor name = T.replace "{name}" name "It’s a bit darker here than usual. The security lights are off? Why? You can’t really see where you’re going. The counter should be to your left, and a gratuity ice cream dispenser to the right. But you can’t see them. The front door should be forward. Can’t see that either. Maybe you should go fetch a light. Where do you want to go, {name}? Press a for left, r for right, w for forward, or s for back to the hallway."

stage D1DarkMazeEntrance name = T.replace "{name}" name "Sure. Why not, {name}? Walk on in the dark. You just bumped into a wall you didn’t even see. You can’t go any further forward. I guess that door isn’t where you thought it was. You stretch out your arms and feel left and right: there seem to be hallways there. Would you like to turn left? If so, press a. Press d to turn right. Or go back to the hallway: press s. Last chance to get out. Choose wisely."

stage D1DarkMazeAttack name = T.replace "{name}" name "You hear something. Voices. People talking. Children. Customers stuck in the maze? “Hello!” The voices stop. A spark lights up the hallway. And someone’s face. A girl. Another spark. She’s right in front of you! You back away. And fall over backwards. You hear the girl following you. And the smell: motor oil. That’s no girl. The hand that grabs you is too strong. Lifts you right up off the floor. To struggle loose, press s."

stage D1DarkMazeDeath name = T.replace "{name}" name "How hard is it to press S, {name}? Very hard, apparently. Did you think I was joking? You were wrong. That not-a-girl that lifted you up like you weigh nothing? It has a hammer. You can’t see it. But you can feel it. Again. And again. And then nothing. Because you’re dead. You made some bad choices there. Want to try again? Press a to reincarnate. Or quit? Press q."

stage D2LitDesk name = T.replace "{name}" name "Your lantern lights up the desk. It holds a letter addressed to you, {name}. Wouldn’t you like to read it? If you would, press w. You don’t have to. It might help, though. But you can ignore it. Maybe rummage around in the storage locker? Press a!"

stage D2LitDeskHelp name = T.replace "{name}" name "You found a map! That letter you’re holding contains a drawing of a maze. And some handwritten text: “Dear {name}, \r\nThank you for your great work the other day. The customers were quite happy with your performance. They made it a point today to let me know you make them feel safe. For that you deserve a promotion. Come and see me tomorrow? \r\nFor tonight your job is to make sure no customer got left behind in the maze. Follow the map. Good luck. \r\nSigned,\r\nJacques Masquie, owner.” \r\nWhat maze? Where? Press s to head back into the hallway and have a look."

stage D2LitLocker name = T.replace "{name}" name "The locker is pretty empty. 2 Days ago it held a security guard uniform. To day it is missing. Why would it be missing? You are the only guard employed, aren’t you, {name}? The cleaning bucket is gone too. Let’s keep an eye out for it. Press s to shut the door and return to the office. Would you rather inspect the cleaner’s uniform? If so, press w."

stage D2LitOffice name = T.replace "{name}" name "This office really is a boring place, wouldn’t you agree, {name}? Let’s get out of here. You don’t really have much of a choice, though. You can press d to go out into the hallway, w to have a drink at the fountain, or q to quit. Which is it?"

stage D2LitOfficeHelp name = T.replace "{name}" name "No, {name}. Your options were to press either d, w, or q. Not whatever that was. Try again."

stage D2LitFountain name = T.replace "{name}" name "Another drink at the fountain? You really like that water, don’t you? Ah well. I guess it helps you stay alive. Just don’t overdo it, {name}, OK? And while you’re here, press w to look in the mirror. Or press d to return to the office without looking. But you’ll never find out what you missed!"

stage D2LitMirror name = T.replace "{name}" name "A risk taker, aren’t you? I like it! So let’s have a look in that mirror. What do you see? Well. There’s you. And your uniform. Looking spiffy. And something just flew by in the corner of your eye. Do you duck instinctively? Press d! If not, press any of these keys: a, w, s."

stage D2LitMirrorAttack name = T.replace "{name}" name "Really? Why that key, {name}? Watch out! Here it is again! That red flying ball with the yellow smile! And it’s got a hammer aimed at your head! Duck, duck duck! Press d now! {name}! Now!"

stage D2LitMirrorSurvive name = T.replace "{name}" name "Ooh, that was close! It missed you by a hair! (You have hair, don’t you?) The ball is flying away. Where is it off to now? Quick, on your feet, and follow it. Press w."

stage D2LitMirrorDeath name = T.replace "{name}" name "Erm. Taking risks is one thing. Getting your head hammered in because you refuse to press d is another. Why, {name}? Why couldn’t you simply press d? Look at you now. Your head in the fountain. The sink turning red with your blood and covered in bits of brain. And a puddle of pee is forming around your feet. Gross! Well I guess this is good-bye. Unless you want to reincarnate and try again. In which case you can press a."

stage D2LitMazeEntrance name = T.replace "{name}" name "You find yourself in a hallway you have not yet seen. Maybe you have been here before when it was dark? There’s 4 exits: forward (w), left (a), right (d), and back (s). There’s a red ball flying to the right. Press d to follow it. Otherwise, press any of these: a, w, s."

stage D2LitLockerUniform name = T.replace "{name}" name "You feel around inside the uniform. It’s a bit sticky and moist. Eew, gross. That uniform should get washed. Better wash your hands soon. Hey, you found a roll of paper in the inside coat pocket! Another map? Nice! You pull back your hand to study the paper. It’s soaked with blood! And your hand is covered in it, too! Press w to go wash your hands!"

stage D4LitFountain name = T.replace "{name}" name "Washing your hands really was necessary. So refreshing! You can’t help but look in the mirror. Your face expresses relief, {name}. And then you see something fly at your head. A bird? Press d to duck, or s to ignore it and return to the office."

stage D4LitMirrorAttack name = T.replace "{name}" name "You ducked. The flying thing misses you. Which lets you have a good look. It’s a ball. A red ball. With a yellow smile. And a hammer. Which is hammering at the air. Because you’re not there. But you were. And happy to have ducked. Here it comes again. Press d, quick!"

stage D4LitMirrorSurvive name = T.replace "{name}" name "Good going! Now you remember: you have seen that ball before, {name}. On the painting at the end of the hallway. On the ice cream. Why did it get out? And did it attack you? And where is it heading now? Good questions! Let’s find out, shall we? Press w to follow the flying ball! Or press q to quit. Your choice, really."

stage D4LitMazeEntrance name = T.replace "{name}" name "You're in a hallway you had not yet seen before. Despite having been here for 4 days. Maybe you have been here before, but it was too dark to see anything? There’s 4 exits: forward (w), left (a), right (d), and back (s). A red ball flew to the right. Do you follow it? If so, press d. Otherwise, press w, a, or s."

c11Msg :: T.Text
c11Msg = "Before you get there, lights turn on and the familiar face of Jacques Masquie greets you: “Hey there, {name}! How you doing? Flushed out any stragglers from the obstacle course?” You think about the jumps. The platforms. What you saw. Did you see any customers left behind? "

c11Msg2 :: T.Text
c11Msg2 = "But he doesn't wait for your answer. “Go home! Get rest. Come back tomorrow. I got something super special planned.” Yeah, right. By now you know better. So press q to quit. Or, if you feel particularly brave, continue playing by pressing w."

c10DeathMsg :: T.Text
c10DeathMsg = "The animatronics attack you from every side. Cuddly kids’ animals no more. Claws ripping at your clothes. Sparks burning your skin. Ice scoops verrrry close to your eyes. Smoke stopping your breath. "

c10DeathMsg2 :: T.Text
c10DeathMsg2 = "You kick and punch and dodge and scream. All to no avail. They’re too tough. That stranger really must have hated Masquie. And you’re the victim. Bah: you got THIS close to night 4. Want to try again? Reincarnate by pressing a. Or press q to quit."

c10AttackMsg :: T.Text
c10AttackMsg = "You shouldn’t have done that, {name}. Those animatronics got messed up. That stranger rewired them. Turned cute stuffed animals into murder machines. With ice cream scoops, forks, and grabby claws. "

c10AttackMsg2 :: T.Text
c10AttackMsg2 = "Run! Run away! Press s to run to safety! Or would you rather fight these murderous machines? If so, press w. You can also quit by pressing q. Choose!"

c8Msg :: T.Text
c8Msg = "Ack! You just got sick to your stomach. And there’s that blue smoke again! You duck instinctively. The smoke quickly clears up. You’re back in the hallway of the ice cream parlor. You turn around to see the door… but all it shows is the end of the hallway. Where is the obstacle course? Did you just get teleported? Again? What’s going on in this place? But you have no time to contemplate: the door bell rings. Now what? Press a to turn left into the parlor and check out who entered. Press d to turn right. W to go forward. For backwards, press s. Not sure why you would want to do that."

c10Msg :: T.Text
c10Msg = "“He will pay for that!” the stranger shouts, laughs maniacally, and runs off out of the room. The animatronics surround you. Their music steadily grows louder. But it does not look like they want to play. There’s something ominous about them. Something scary. "

c10Msg2 :: T.Text
c10Msg2 = "To run away, press w. To stand your ground, press s. Or press q to quit now."

c9Msg :: T.Text
c9Msg = "Security duty FTW! You call out to the stranger: \n“Oi! Who are you? What you doing here? It’s the middle of the night. The parlor is closed!” \nSurely that made an impact! The stranger looks up at you. And says: \n“It’s about time you got here, {name}. I’ve been waiting for you. Nice jumping out there. Do you know what your employer did? Jacques Masquie? He fired all of us. Replaced us with these… robots. He took my job. I lost my house. My health insurance. My youngest child… He’s gotta pay for that.” \nThe stranger presses a button on the desk. The cages open. What do you do? Press s to return to safety. Press w to wait and see."

c7Msg :: T.Text
c7Msg = "You are in a lit room. The walls are lined with cages. Each cage houses a stuffed animal animatronic. Their movements seem erratic. Like they’re broken. You see a bunny with square eyes, drenched in blood, a cat that sparks and has an ice-cream cone for 1 ear, an elephant on a wheel spinning in circles, and many more… In the middle of the room there is a desk. With a chair. With someone sitting in it. Do you do you duty as a security guard and ask that person what they’re doing? If yes, press y, {name}. If not, press n to get out of this room. You can also quit by pressing q."

c7Msg2 :: T.Text
c7Msg2 = ""

c6DeathMsg :: T.Text
c6DeathMsg = "There’s animatronics on every side. No way out. Claws ripping at your clothes. Sparks burning your skin. Ice scoops verrrry close to your eyes. "

c6DeathMsg2 :: T.Text
c6DeathMsg2 = "You kick them and punch them and dodge and scream. All to no avail. The animatronics are too tough. Whoever reprogrammed them really must have hated Masquie. And you’re the victim of that hatred. One of the many, considering the bloody lumps all over the place. You did well. You got pretty far. I tip my hat to you in respect. May you reincarnate and try again by pressing a. Or press q to quit."

c6SurviveMsg :: T.Text
c6SurviveMsg = "So you run. Faster! Them claws rip your clothes. Sparks burn your skin. Ice scoops get verrrrry close to your eyes. But there’s the door! Jump! Jump now! Press w to jump!"

c6AttackMsg :: T.Text
c6AttackMsg = "Ready to fight, are you? Have you met these animatronics before? They aren’t right in the head. Someone’s done messed them up. Someone who was very angry with Jacques Masquie. And turned cute stuffed animals into murder machines. With ice cream scoops, forks, and grabby claws. "

c6AttackMsg2 :: T.Text
c6AttackMsg2 = "For a strategic retreat and see if that door leads to safety, press y. To stay and fight, press n. Or press q to quit now."

c6fMsg :: T.Text
c6fMsg = "What are you doing? You had the exit right in front of you? But you had to go and press a different button, didn’t you? Instant karma, {name}: children’s music closes in on you from all sides, as well as mechanical whirring. Differing songs from each side, growing louder. Animatronic stuffed animals slide into the light. Surrounding you. Blocking your escape. "

c6fMsg2 :: T.Text
c6fMsg2 = "Do you stand your ground and fight? If yes, press y. If not, press n to flee and run to the door."

c6Msg :: T.Text
c6Msg = "You land safely on what feels like rubber mats. "
c6Msg2 :: T.Text
c6Msg2 = "Ahead you see a rectangle of light. A door? Would that be the end of this bloody obstacle course? It’s about time! Press w to go forward to that door. You could press different buttons. Like a, s, d, n, or y. Go ahead and try…"

c5Msg :: T.Text
c5Msg = "Interesting choice. You arrived in front of a room. There’s light there. And music. Quite a lot of it and from the sound of it, multiple songs at the same time. Want to enter that room and check it out, {name}? If so, press w. Instead you may want to turn back and take the down ramp. If so, press s. Or… quit, by pressing q."

c4Msg :: T.Text
c4Msg = "GG, {name}, gg! This platform splits into a ramp up and a slide down. There’s barely enough light to make that out… you can’t see what’s beyond either end. If only you had had a map. So… pick a way. Press a for up, or d for down. You could press s to try and jump back. But I wouldn’t. To give up and quit, press q."

c4wMsg :: T.Text
c4wMsg = "GG, {name}, gg! This platform splits into a ramp up and a slide down. There’s barely enough light to make that out… you can’t see what’s beyond either end. Luckily you have a map. Which is smudged. It might direct you down. Or not. Press a for up, or d for down. You could press s to try and jump back. But I wouldn’t. To give up and quit, press q."

c3fDeathAboveMsg :: T.Text
c3fDeathAboveMsg = "Metal claws shoot up from below. You scramble to your feet and dodge. And bump into something furry you didn’t see before. It moves. And sings. A children’s song. That’s where the music came from! You back away. The animatronic follows you. It’s sparking, as if a wire is short-circuiting. The sparks show a blue smoke bellowing out of the cat’s mouth. You back away further. And fall into the soggy lumps. With the metal claws. That hold you in place. The blue smoke suffocates you. The cat sparks electrocute you. So this is game over, {name}. Want to try again? Press a to reincarnate and try again. Or press q to quit."

c3fMsg :: T.Text
c3fMsg = "Eep! That platform is too far away! You crash into the wall, and reach out to grab the ledge… but you’re not used to pulling up your own weight, are you, {name}? And that ledge is slippery. So you slip and slide down the slick stone, faster and faster as you fear the fall… into soft and fluffy bean bags? But soggy. Maybe not bean bags. Maybe something way more disgusting. And it’s moving? What’s under there? Press w to press on and find out. Press s to scramble back up on your feet."

c2fSurviveMsg :: T.Text
c2fSurviveMsg = "Good thinking! Now let’s get back up and try and jump better. You hold your breath, jump up above the blue smoke. You see a ladder. Press a to ascend it. But maybe you’ve had enough? To quit, press q."

c2fwDeathMsg :: T.Text
c2fwDeathMsg = "You should have rolled on the floor. Searching for an extinguisher in this darkness takes way too long. And wrapping your map around you would’ve just caused it to burn, too. The smoke fills your lungs and you faint. And that’s a good thing, {name}: now you won’t feel the pain from burning alive. Press a to reincarnate and try again. Or give up and quit: press q."

c2fDeathMsg :: T.Text
c2fDeathMsg = "You should have rolled on the floor. Searching for an extinguisher or blanket in this darkness takes way too long. The smoke fills your lungs and you faint. And that’s a good thing, {name}: now you won’t feel the pain from burning alive. Press a to reincarnate and try again. Or give up and quit: press q."

c2fAttackMsg :: T.Text
c2fAttackMsg = "Where’s that ladder? Can’t find it! The darkness lights up from the broken cat’s electricity sparks. Every spark shows bellowing blue smoke. That’s a bit much, isn’t it? A mechanical whir alerts you: cat ahead! You hear it screech and jump. "

c2fMsg :: T.Text
c2fMsg = "Err! Wrong! Down you go, into the darkness! You fall on top of something hard. It moves? It’s one of those stuffed animal animatronics! It’s a cat! Bright sparks emanate from its head, lighting up blue smoke that rises from the cat’s body. Your fall must have broken it, {name}, and shorted a circuit. The sparking smoke engulfs you, making it hard to breathe. "

c1fMsg :: T.Text
c1fMsg = "You missed! You fall! Squishy pillows safely catch you. Around you, small lights appear in pairs, at eye level. The pairs start moving and approach you. A children’s song gets louder as the lights close in. You recognize the whirring of machinery: animatronics. You also recognize the smells: vanilla, chocolate, motor oil, and… blood? Uh-oh. Let’s get out of here, {name}. You reach around and find a ladder. Press s to climb up to the platform."

b4EoSMsg :: T.Text
b4EoSMsg = "The door bell rings. Would that be thieves? You walk to the ice cream parlor. The hallway lights turn on and you get met by a familiar face. It’s your employer, Jacques Masquie. What’s he doing here at this hour? He says: “Hey there, {name}! How’s it going? Shift’s over for today. See you tomorrow night! Same ice cream time, same ice cream channel!” He laughs. Like he made a joke. Probably a quaint reference to some old book or TV show. Anyway: press w to continue into night 3!"

b4pEoSMsg :: T.Text
b4pEoSMsg = "The door bell rings. Would that be thieves? You walk to the ice cream parlor. The hallway lights turn on and you get met by a familiar face. It’s your employer, Jacques Masquie. What’s he doing here at this hour? He says: “Hey there, {name}! How’s it going? Shift’s over for today. Anything problematic to report?” Hmm… You saw that video, didn’t you? Do you feel like telling Masquie? If yes, press y. Or press n to stay silent and pretend you didn’t see nothing, and jump straight into night 3!"

b4RidiculeMsg :: T.Text
b4RidiculeMsg = "So you tell Masquie about the video. About the other security guard. About his concern for the animatronics. And how it looked like he got murdered. Masquie takes it in, mulls it over, and responds: “Murder? That’s a serious allegation, {name}. By my animatronics, though? They’re robots! They’re only programmed to serve ice cream and sing children’s songs! Do you really think that if they’d murder anyone, I’d still be in business? Come on now! Sounds like someone pranked you good! Tomorrow is going to be a busy day! See you at night fall! Same ice cream time, same ice cream channel!” He laughs like he made a good joke. Probably some quaint reference to an old book or TV show. What do you do? Be upset with Masquie and quit? If so, press q. Otherwise, press w to start night 3."

c1Msg :: T.Text
c1Msg = "Welcome back, {name}. This is night 3. Good to have you here. Jacques Masquie, your employer, promised today would be busy. He left you a letter. Maybe that will tell you what to expect? To read it, press h. There’s enough moonlight to read by. You don’t have to read it. Go ahead and ignore it! You know you want to! Press w."


