module RenderHandler where

import Graphics.Gloss
import DungeonCrawler
import Data.Fixed

renderHandler :: World -> World -> Picture
renderHandler World{screenType="start"} world = start (seconds world)
renderHandler World{screenType="charCreation1"} world = renderCharCreation1 (inputText world)
renderHandler World{screenType="charCreation2"} world = renderCharCreation2 (inputText world)
renderHandler World{screenType="charCreation3"} world = renderCharCreation3 (inputText world)
renderHandler World{screenType="charCreation4"} world = renderCharCreation4 (inputText world)
renderHandler World{screenType="charCreation5"} world = renderCharCreation5 (inputText world)
renderHandler World{screenType="beginFight"} world = renderStartFightScene world
renderHandler World{screenType="fight"} world = fight (seconds world) (getHero (internalState world)) (getMonster (internalState world))
renderHandler World{screenType="result"} world = resultScene (getHero (internalState world)) (getMonster (internalState world))
renderHandler World{screenType="levelUp"} world = 
  do
    let hero = getHero (internalState world)
    levelUpScene (getMaxHealth hero) (getAttack hero) (getBleed hero) (getLifeSteal hero) (getPriority hero)
renderHandler World{screenType="end"} world = end (seconds world) (getRound (internalState world))

--------------------------------------------------------
-- ANIMATIONS
--------------------------------------------------------

end :: Float -> Int -> Picture
end seconds rounds =
  do
    if (seconds `mod'` 3) < 1 then
      do
        Pictures [ tombstone rounds, title, endText ]
    else
      do
         Pictures[ tombstone rounds, title, endText, skullBase1, skullBase2, skullEye1, skullEye2, skullJawLine1, skullJawLine2, skullJawLine3 ]

fight :: Float -> Character -> Character -> Picture
fight seconds getHero getMonster = 
  do
    let heroHealth = (getHealth getHero)
    let heroAttack = (getAttack getHero)
    let heroBleed = (getBleed getHero)
    let heroLifeSteal = (getLifeSteal getHero)
    let heroPriority = (getPriority getHero)
    let heroBleedReceived = (getBleedRecieved getHero)

    let monsterHealth = (getHealth getMonster)
    let monsterAttack = (getAttack getMonster)
    let monsterBleed = (getBleed getMonster)
    let monsterBleedReceived = (getBleedRecieved getMonster)
    let monsterLifeSteal = (getLifeSteal getMonster)
    let monsterPriority = (getPriority getMonster)
    let notAnimatedPictures = Pictures[fightTitle, title, heroHPBar (fromIntegral heroHealth), heroStatAttack heroAttack,
                                heroStatBleed heroBleed, heroStatLifeSteal heroLifeSteal, heroStatPriority heroPriority,
                                heroStatBleedReceived heroBleedReceived,monsterHPBar (fromIntegral monsterHealth),
                                monsterStatAttack monsterAttack, monsterStatBleed monsterBleed,
                                monsterStatBleedReceived monsterBleedReceived, monsterStatLifeSteal monsterLifeSteal,
                                monsterStatPriority monsterPriority, Color blue heroBox, Color red monsterBox]
    if (seconds `mod'` 2) < 1 then
      do
        Pictures [notAnimatedPictures, hero (-120) 0, monster (120) 0]
    else
      do
         Pictures [notAnimatedPictures, hero (-110) 0, monster (110) 0]

start:: Float -> Picture
start seconds = 
  do
    let scale = 1
    if (seconds `mod'` 2) < 1 then
      do
        Pictures [ title, Scale 1.1 1.1 redFlame, Scale 1.05 1.05 orangeFlame, yellowFlame, torchStick, mainText]
    else
      do
         Pictures [ title, redFlame, orangeFlame, yellowFlame, torchStick, mainText]

--------------------------------------------------------
-- RENDER CHAR CREATION SCENES
--------------------------------------------------------

renderCharCreation1 :: String -> Picture
renderCharCreation1 s = Pictures [
                          title,
                          characterCreationText,
                          characterHealthText,
                          (renderInputBox (InputBox s))]

renderCharCreation2 :: String -> Picture
renderCharCreation2 s = Pictures [
                          title,
                          characterCreationText,
                          characterAttackText,
                          (renderInputBox (InputBox s))]

renderCharCreation3 :: String -> Picture
renderCharCreation3 s = Pictures [
                          title,
                          characterCreationText,
                          characterBleedText,
                          (renderInputBox (InputBox s))]

renderCharCreation4 :: String -> Picture
renderCharCreation4 s = Pictures [
                          title,
                          characterCreationText,
                          characterLifeStealText,
                          (renderInputBox (InputBox s))]

renderCharCreation5 :: String -> Picture
renderCharCreation5 s = Pictures [
                          title,
                          characterCreationText,
                          characterPriorityext,
                          (renderInputBox (InputBox s))]

--------------------------------------------------------
-- TITLES
--------------------------------------------------------

fightTitle = Translate (-40) (70)
  $ Scale 0.2 0.2
  $ Color orange (Text "Attack!")

title =  Translate (-200) (160) 
  $ Scale 0.3 0.3 
  $ Color yellow (Text "DUNGEON CRAWLER") 


--------------------------------------------------------
-- SCENES
--------------------------------------------------------

entryScene = Pictures [
  title,
  torch,
  mainText]

fightScene = Pictures [
  title, 
  heroHPBar 80, 
  monsterHPBar 50,
  hero (-120) 0,
  monster (120) 0]

endScene = Pictures [
  tombstone 3,
  title,
  endText]

levelUpScene hpStat attackStat bleedStat lifeStealState priorityStat = Pictures [
  title,
  levelUpText,
  levelUpSubText,
  levelUpHealthButton,
  levelUpHealthButtonText hpStat,
  levelUpAttackButton,
  levelUpAttackButtonText attackStat,
  levelUpBleedButton,
  levelUpBleedButtonText bleedStat,
  levelUpLifeStealButton,
  levelUpLifeStealButtonText lifeStealState,
  levelUpPriorityButton,
  levelUpPriorityButtonText priorityStat]

resultScene resultHero resultMonster =
  do
    let heroHealth = (getHealth resultHero)
    let heroAttack = (getAttack resultHero)
    let heroBleed = (getBleed resultHero)
    let heroLifeSteal = (getLifeSteal resultHero)
    let heroPriority = (getPriority resultHero)
    let monsterHealth = (getHealth resultMonster)
    let monsterAttack = (getAttack resultMonster)
    let monsterBleed = (getBleed resultMonster)
    let monsterLifeSteal = (getLifeSteal resultMonster)
    let monsterPriority = (getPriority resultMonster)
    -- if hero and monster are both dead, calculate priority based win
    if (heroHealth <= 0) && (monsterHealth <= 0) then
      do
        -- hero has priority, hero wins ties
        if (heroPriority >= monsterPriority) then
          do
            Pictures [
              title, 
              resultTextWin, 
              heroHPBar (fromIntegral heroHealth), 
              heroStatAttack heroAttack, 
              heroStatBleed heroBleed, 
              heroStatLifeSteal heroLifeSteal, 
              heroStatPriority heroPriority,
              monsterHPBar (fromIntegral monsterHealth), 
              monsterStatAttack monsterAttack, 
              monsterStatBleed monsterBleed, 
              monsterStatLifeSteal monsterLifeSteal, 
              monsterStatPriority monsterPriority, 
              hero (-120) 0, 
              deadMonster (120) (-80), 
              Color blue heroBox, 
              Color red monsterBox]
        -- monster has priority
        else 
          do
            Pictures [
              title, 
              resultTextLose, 
              heroHPBar (fromIntegral heroHealth), 
              heroStatAttack heroAttack, 
              heroStatBleed heroBleed, 
              heroStatLifeSteal heroLifeSteal, 
              heroStatPriority heroPriority,
              monsterHPBar (fromIntegral monsterHealth), 
              monsterStatAttack monsterAttack, 
              monsterStatBleed monsterBleed, 
              monsterStatLifeSteal monsterLifeSteal, 
              monsterStatPriority monsterPriority, 
              deadHero (-120) (-80), 
              monster (120) 0, 
              Color blue heroBox, 
              Color red monsterBox]
    -- hero dead, monster lives
    else 
      if (heroHealth <= 0) then
        do
          Pictures [
              title, 
              resultTextLose, 
              heroHPBar (fromIntegral heroHealth), 
              heroStatAttack heroAttack, 
              heroStatBleed heroBleed, 
              heroStatLifeSteal heroLifeSteal, 
              heroStatPriority heroPriority,
              monsterHPBar (fromIntegral monsterHealth), 
              monsterStatAttack monsterAttack, 
              monsterStatBleed monsterBleed, 
              monsterStatLifeSteal monsterLifeSteal, 
              monsterStatPriority monsterPriority, 
              deadHero (-120) (-80), 
              monster (120) 0, 
              Color blue heroBox, 
              Color red monsterBox]
      -- monster dead, hero lives
      else 
        do
          Pictures [
              title, 
              resultTextWin, 
              heroHPBar (fromIntegral heroHealth), 
              heroStatAttack heroAttack, 
              heroStatBleed heroBleed, 
              heroStatLifeSteal heroLifeSteal, 
              heroStatPriority heroPriority,
              monsterHPBar (fromIntegral monsterHealth), 
              monsterStatAttack monsterAttack, 
              monsterStatBleed monsterBleed, 
              monsterStatLifeSteal monsterLifeSteal, 
              monsterStatPriority monsterPriority, 
              hero (-120) 0, 
              deadMonster (120) (-80), 
              Color blue heroBox, 
              Color red monsterBox]

--------------------------------------------------------
-- ENTRY SCENE
--------------------------------------------------------

mainText :: Picture
mainText =
  Translate (-150) (-120) 
  $ Scale 0.2 0.2 
  $ Color yellow (Text "Press Enter to Start") 


-- torch model
torch = Pictures [redFlame, orangeFlame, yellowFlame, torchStick]

yellowFlame = Translate 0 10 
  $ Scale 0.2 0.2 
  $ Color yellow (ThickCircle 100 100) 

orangeFlame = Translate 0 20 
  $ Scale 0.2 0.3 
  $ Color orange (ThickCircle 120 120) 

redFlame = Translate 0 40 
  $ Scale 0.2 0.4 
  $ Color red (ThickCircle 140 140) 

torchStick = Translate 0 (-80) 
  $ Color brownColor (rectangleSolid 20 200) 


--------------------------------------------------------
-- FIGHT SCENE
--------------------------------------------------------

-- HP Bars

heroHPBar hp = Pictures [Color hpBarColor containerHeroHPBar, Color green (topHeroHPBar hp), heroHPBarCounter (show hp)]

containerHeroHPBar = Translate (-150) (-170) 
  $ rectangleSolid 100 45 

topHeroHPBar hp = Translate (-150 - ((100 - hp) / 2)) (-170) 
  $ rectangleSolid hp 40 

heroHPBarCounter hp = Translate (-190) (-180) 
  $ Scale 0.2 0.2 
  $ Text hp 

monsterHPBar hp = Pictures [Color hpBarColor containerMonsterHPBar, Color red (topMonsterHPBar hp), monsterHPBarCounter (show hp)]

containerMonsterHPBar = Translate (150) (-170) 
  $ rectangleSolid 100 45

topMonsterHPBar hp = Translate (150 + ((100 - hp) / 2)) (-170) 
  $ rectangleSolid hp 40 

monsterHPBarCounter hp = Translate (140) (-180) 
  $ Scale 0.2 0.2 
  $ Text hp 


-- Hero Stats

heroBox = Translate (-130) (-240)
  $ rectangleWire 150 200

heroStatAttack :: Int -> Picture
heroStatAttack stat=
  Translate (-200) (-220)
  $ Scale 0.2 0.2
  $ Color blue (Text ("Attack:" ++ show stat))

heroStatBleed :: Int -> Picture
heroStatBleed stat=
  Translate (-200) (-245)
  $ Scale 0.2 0.2
  $ Color blue (Text ("Bleed:" ++ show stat))

heroStatBleedReceived :: Int -> Picture
heroStatBleedReceived stat=
  Translate (-200) (-275)
  $ Scale 0.13 0.13
  $ Color blue (Text ("Bleed received:" ++ show stat))

heroStatLifeSteal :: Int -> Picture
heroStatLifeSteal stat=
  Translate (-200) (-300)
  $ Scale 0.2 0.2
  $ Color blue (Text ("LifeSteal:" ++ show stat))

heroStatPriority :: Int -> Picture
heroStatPriority stat=
  Translate (-200) (-325)
  $ Scale 0.2 0.2
  $ Color blue (Text ("Priority:" ++ show stat))

-- Monster Stats

monsterBox = Translate (170) (-240)
  $ rectangleWire 150 200

monsterStatAttack :: Int -> Picture
monsterStatAttack stat=
  Translate (100) (-220)
  $ Scale 0.2 0.2
  $ Color red (Text ("Attack:" ++ show stat))

monsterStatBleed :: Int -> Picture
monsterStatBleed stat=
  Translate (100) (-245)
  $ Scale 0.2 0.2
  $ Color red (Text ("Bleed:" ++ show stat))

monsterStatBleedReceived :: Int -> Picture
monsterStatBleedReceived stat=
  Translate (100) (-275)
  $ Scale 0.13 0.13
  $ Color red (Text ("Bleed received:" ++ show stat))

monsterStatLifeSteal :: Int -> Picture
monsterStatLifeSteal stat=
  Translate (100) (-300)
  $ Scale 0.2 0.2
  $ Color red (Text ("LifeSteal:" ++ show stat))

monsterStatPriority :: Int -> Picture
monsterStatPriority stat=
  Translate (100) (-325)
  $ Scale 0.2 0.2
  $ Color red (Text ("Priority:" ++ show stat))

-- Hero Model

hero x y = Pictures [heroBody x y, heroHead x y, heroRightLeg x y, heroLeftLeg x y, heroSword x y, heroRightArm x y, heroLeftArm x y]

heroBody x y = Translate x y
  $ Color blue (rectangleSolid 50 50)

heroHead x y = Translate x (y+25)
  $ Color blue (rectangleSolid 20 50)

heroRightLeg x y = Translate (x+15) (y-50)
  $ Color blue (rectangleSolid 15 70)

heroLeftLeg x y = Translate (x-15) (y-50)
  $ Color blue (rectangleSolid 15 70)

heroRightArm x y = Translate (x+40) y
  $ Rotate (-45)
  $ Color blue (rectangleSolid 15 50)

heroLeftArm x y = Translate (x-40) y
  $ Rotate 45
  $ Color blue (rectangleSolid 15 50)

heroSword x y = Translate (x+70) (y+10)
  $ Rotate 45
  $ Color greyColor (rectangleSolid 10 80)

-- Monster Model

monster x y = Pictures [monsterBody x y, monsterHead x y, monsterRightLeg x y, monsterLeftLeg x y, monsterSword x y, monsterRightArm x y, monsterLeftArm x y]

monsterBody x y = Translate x y
  $ Color red (rectangleSolid 50 50)

monsterHead x y = Translate x (y+25)
  $ Color red (rectangleSolid 20 50)

monsterRightLeg x y = Translate (x+15) (y-50)
  $ Color red (rectangleSolid 15 70)

monsterLeftLeg x y = Translate (x-15) (y-50)
  $ Color red (rectangleSolid 15 70)

monsterRightArm x y = Translate (x+40) y
  $ Rotate (-45)
  $ Color red (rectangleSolid 15 50)

monsterLeftArm x y = Translate (x-40) y
  $ Rotate 45
  $ Color red (rectangleSolid 15 50)

monsterSword x y = Translate (x-70) (y+10)
  $ Rotate (-45)
  $ Color greyColor (rectangleSolid 10 80)



startFightText :: Picture
startFightText =
  Translate (-180) (-120)
  $ Scale 0.2 0.2
  $ Color yellow (Text "Press Enter to Start the FIGHT!")

roundText :: Int -> Picture
roundText r=
  Translate (-60) (110)
  $ Scale 0.25 0.25
  $ Color red (Text ("ROUND " ++ (show r)))

renderStartFightScene :: World -> Picture
renderStartFightScene world = Pictures [
                                (fight (seconds world) (getHero (internalState world)) (getMonster (internalState world))),
                                startFightText,
                                (roundText (getRound (internalState world)))]

--------------------------------------------------------
-- RESULT SCENE
--------------------------------------------------------

resultTextWin :: Picture
resultTextWin =
  Translate (-150) (100) 
  $ Scale 0.2 0.2 
  $ Color yellow (Text "You Win!")

resultTextLose :: Picture
resultTextLose =
  Translate (-150) (100) 
  $ Scale 0.2 0.2 
  $ Color yellow (Text "You Lose!")

-- dead hero model

deadHero x y = Pictures [deadHeroBody x y, deadHeroHead x y, deadHeroRightLeg x y, deadHeroLeftLeg x y, deadHeroSword x y, deadHeroRightArm x y, deadHeroLeftArm x y]

deadHeroBody x y = Translate x y
  $ Rotate 90
  $ Color blue (rectangleSolid 50 50)

deadHeroHead x y = Translate (x-50) y
  $ Rotate 120
  $ Color blue (rectangleSolid 20 20)

deadHeroRightLeg x y = Translate (x+50) (y+15)
  $ Rotate 90
  $ Color blue (rectangleSolid 15 70)

deadHeroLeftLeg x y = Translate (x+50) (y-15)
  $ Rotate 90
  $ Color blue (rectangleSolid 15 70)

deadHeroRightArm x y = Translate (x-25) (y+40)
  $ Rotate (-30)
  $ Color blue (rectangleSolid 15 50)

deadHeroLeftArm x y = Translate (x-25) (y-40)
  $ Rotate 30
  $ Color blue (rectangleSolid 15 50)

deadHeroSword x y = Translate (x-70) (y+10)
  $ Rotate (-45)
  $ Color greyColor (rectangleSolid 10 80)

-- dead monster model

deadMonster x y = Pictures [deadMonsterBody x y, deadMonsterHead x y, deadMonsterRightLeg x y, deadMonsterLeftLeg x y , deadMonsterSword x y, deadMonsterRightArm x y, deadMonsterLeftArm x y]

deadMonsterBody x y = Translate x y
  $ Rotate 90
  $ Color red (rectangleSolid 50 50)

deadMonsterHead x y = Translate (x-50) y
  $ Rotate 120
  $ Color red (rectangleSolid 20 20)

deadMonsterRightLeg x y = Translate (x+50) (y+15)
  $ Rotate 90
  $ Color red (rectangleSolid 15 70)

deadMonsterLeftLeg x y = Translate (x+50) (y-15)
  $ Rotate 90
  $ Color red (rectangleSolid 15 70)

deadMonsterRightArm x y = Translate (x-25) (y+40)
  $ Rotate (-30)
  $ Color red (rectangleSolid 15 50)

deadMonsterLeftArm x y = Translate (x-25) (y-40)
  $ Rotate (-30 )
  $ Color red (rectangleSolid 15 50)

deadMonsterSword x y = Translate (x-70) (y-40)
  $ Rotate 135
  $ Color greyColor (rectangleSolid 10 80)

--------------------------------------------------------
-- LEVEL UP SCENE
--------------------------------------------------------

levelUpText :: Picture
levelUpText =
  Translate (-150) (100) 
  $ Scale 0.2 0.2 
  $ Color yellow (Text "Level Up!") 

levelUpSubText :: Picture
levelUpSubText =
  Translate (-250) (75) 
  $ Scale 0.2 0.2 
  $ Color yellow (Text "You feel stronger upon vanquishing your foe!") 

levelUpHealthButton :: Picture
levelUpHealthButton =
  Translate (-250) (0) 
  $ Color yellow (rectangleSolid 200 50) 

levelUpHealthButtonText :: Int -> Picture
levelUpHealthButtonText currentMaxHp =
  Translate (-335) (0) 
  $ Scale 0.1 0.1 
  $ Color black (Text ("Increase Health: " ++ show currentMaxHp ++ "->" ++ show (currentMaxHp + amountToLevelUpBy))) 

levelUpAttackButton :: Picture
levelUpAttackButton =
  Translate (0) (0) 
  $ Color yellow (rectangleSolid 200 50) 

levelUpAttackButtonText :: Int -> Picture
levelUpAttackButtonText currentAttack =
  Translate (-85) (0) 
  $ Scale 0.1 0.1 
  $ Color black (Text ("Increase Attack: " ++ show currentAttack ++ "->" ++ show (currentAttack + amountToLevelUpBy))) 

levelUpBleedButton :: Picture
levelUpBleedButton =
  Translate (250) (0) 
  $ Color yellow (rectangleSolid 200 50) 

levelUpBleedButtonText :: Int -> Picture
levelUpBleedButtonText currentBleed =
  Translate (165) (0) 
  $ Scale 0.1 0.1 
  $ Color black (Text ("Increase Bleed: " ++ show currentBleed ++ "->" ++ show (currentBleed + amountToLevelUpBy))) 

levelUpLifeStealButton :: Picture
levelUpLifeStealButton =
  Translate (-125) (-100) 
  $ Color yellow (rectangleSolid 200 50) 

levelUpLifeStealButtonText :: Int -> Picture
levelUpLifeStealButtonText currentLifeSteal =
  Translate (-210) (-100) 
  $ Scale 0.1 0.1 
  $ Color black (Text ("Increase Life Steal: " ++ show currentLifeSteal ++ "->" ++ show (currentLifeSteal + amountToLevelUpBy))) 

levelUpPriorityButton :: Picture
levelUpPriorityButton =
  Translate (125) (-100) 
  $ Color yellow (rectangleSolid 200 50) 

levelUpPriorityButtonText :: Int -> Picture
levelUpPriorityButtonText currentPriority =
  Translate (40) (-100) 
  $ Scale 0.1 0.1 
  $ Color black (Text ("Increase Priority: " ++ show currentPriority ++ "->" ++ show (currentPriority + amountToLevelUpBy))) 

--------------------------------------------------------
-- CHARACTER CREATION SCENE
--------------------------------------------------------

characterCreationText :: Picture
characterCreationText =
  Translate (-150) (100) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Color yellow (Text "Build your character!") -- text to display

characterHealthText :: Picture
characterHealthText =
  Translate (-200) (60) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Color yellow (Text "What is your character's health?") -- text to display

characterAttackText :: Picture
characterAttackText =
    Translate (-200) (60) -- shift the text to the middle of the window
    $ Scale 0.2 0.2 -- display it half the original size
    $ Color yellow (Text "What is your character's attack?") -- text to display

characterBleedText :: Picture
characterBleedText =
  Translate (-200) (60) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Color yellow (Text "What is your character's bleed?") -- text to display

characterLifeStealText :: Picture
characterLifeStealText =
    Translate (-200) (60) -- shift the text to the middle of the window
    $ Scale 0.2 0.2 -- display it half the original size
    $ Color yellow (Text "What is your character's life steal?") -- text to display

characterPriorityext :: Picture
characterPriorityext =
  Translate (-200) (60) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Color yellow (Text "What is your character's priority?") -- text to display

data InputBox = InputBox String
renderInputBox :: InputBox -> Picture
renderInputBox (InputBox s) = pictures [inputField, inputText]
  where
    inputField = Color yellow (rectangleWire 380 80)
    inputText = translate (-170) 0
        $ scale 0.25 0.25
        $ Color yellow (text s)

--------------------------------------------------------
-- END SCENE
--------------------------------------------------------

endText :: Picture
endText =
  Translate (-150) (-120) 
  $ Scale 0.4 0.4 
  $ Color red (Text "GAME OVER") 

tombstone round = Pictures [tombstoneBody, tombstoneHead, tombstoneEngraving round]

tombstoneBody = Translate 0 (-50)
  $ Color greyColor (rectangleSolid 175 300)

tombstoneHead = Translate (-30) 30
  $ Scale 0.3 0.3 
  $ Text "R.I.P"

tombstoneEngraving round = Translate (-50) (-20)
  $ Scale 0.2 0.2 
  $ Text ("Round: " ++ show round)

skullBase1 = Translate (-250) 10 
  $ Scale 0.2 0.2
  $ Color greyColorDark (ThickCircle 100 200)

skullBase2 = Translate (-250) 5
  $ Scale 0.3 0.3
  $ Color greyColorDark (ThickCircle 120 220)

skullEye1 = Translate (-220) 15
  $ Scale 0.05 0.05
  $ Color red (ThickCircle 100 200)

skullEye2 = Translate (-280) 15
  $ Scale 0.05 0.05
  $ Color red (ThickCircle 100 200)

skullJawLine1 = Translate (-220) (-50)
  $ Color black (rectangleSolid 10 40)

skullJawLine2 = Translate (-250) (-50)
  $ Color black (rectangleSolid 10 40)

skullJawLine3 = Translate (-280) (-50)
  $ Color black (rectangleSolid 10 40)
  
--------------------------------------------------------
-- CUSTOM COLORS
--------------------------------------------------------

greyColor :: Color
greyColor = makeColor 0.7 0.7 0.7 1

greyColorDark :: Color
greyColorDark = makeColor 0.8 0.8 0.8 1

hpBarColor :: Color
hpBarColor = makeColor 0.5 0.5 0.5 1

lightColor :: Color
lightColor = makeColor 0.3 0.3 0.3 0.5

brownColor :: Color
brownColor = makeColor 0.5 0.3 0.1 1