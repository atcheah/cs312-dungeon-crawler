module RenderHandler where

import Graphics.Gloss
import DungeonCrawler
import Data.Fixed

renderHandler :: World -> World -> Picture
renderHandler World{screenType="start"} world = start (seconds world)
renderHandler World{screenType="charCreation"} world = fightScene-- PLACEHOLDER
renderHandler World{screenType="fight"} world = fight (seconds world) (getHealth (getHero (internalState world))) (getHealth (getMonster (internalState world)))
renderHandler World{screenType="levelUp"} world = 
  do
    let hero = getHero (internalState world)
    levelUpScene (getHealth hero) (getAttack hero) (getBleed hero) (getLifeSteal hero) (getPriority hero)
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

fight :: Float -> Int -> Int -> Picture
fight seconds heroHP monsterHP = 
  do
    if (seconds `mod'` 2) < 1 then
      do
        Pictures [ title, heroHPBar (fromIntegral heroHP), monsterHPBar (fromIntegral monsterHP), hero (-120) 0, monster (120) 0]
    else
      do
         Pictures [ fightTitle, title, heroHPBar (fromIntegral heroHP), monsterHPBar (fromIntegral monsterHP), hero (-110) 0, monster (110) 0 ]

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

-- Hero

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

-- Monster

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