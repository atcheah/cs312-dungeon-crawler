-- To run
-- ghci -fno-ghci-sandbox
-- :load Main
-- main

import Graphics.Gloss
main = display (InWindow "Dungeon Crawler" (400, 400) (0, 0)) black fightScene

--------------------------------------------------------
-- TITLE
--------------------------------------------------------

title =  Translate (-200) (160) -- shift the text to the middle of the window
  $ Scale 0.3 0.3 -- display it half the original size
  $ Color yellow (Text "DUNGEON CRAWLER") -- text to display

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

--------------------------------------------------------
-- ENTRY SCENE
--------------------------------------------------------

mainText :: Picture
mainText =
  Translate (-150) (-120) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Color yellow (Text "Press Enter to Start") -- text to display


-- torch model
torch = Pictures [redFlame, orangeFlame, yellowFlame, torchStick]

yellowFlame = Translate 0 10 -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Color yellow (ThickCircle 100 100) -- text to display

orangeFlame = Translate 0 20 -- shift the text to the middle of the window
  $ Scale 0.2 0.3 -- display it half the original size
  $ Color orange (ThickCircle 120 120) -- text to display

redFlame = Translate 0 40 -- shift the text to the middle of the window
  $ Scale 0.2 0.4 -- display it half the original size
  $ Color red (ThickCircle 140 140) -- text to display

torchStick = Translate 0 (-80) -- shift the text to the middle of the window
  $ Color brownColor (rectangleSolid 20 200) -- text to display


--------------------------------------------------------
-- FIGHT SCENE
--------------------------------------------------------

-- HP Bars

heroHPBar hp = Pictures [Color hpBarColor containerHeroHPBar, Color green (topHeroHPBar hp), heroHPBarCounter (show hp)]

containerHeroHPBar = Translate (-150) (-170) 
  $ rectangleSolid 100 45 

topHeroHPBar hp = Translate (-150 - ((100 - hp) / 2)) (-170) -- shift the text to the middle of the window
  $ rectangleSolid hp 40 -- display it half the original size

heroHPBarCounter hp = Translate (-190) (-180) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Text hp -- display it half the original size

monsterHPBar hp = Pictures [Color hpBarColor containerMonsterHPBar, Color red (topMonsterHPBar hp), monsterHPBarCounter (show hp)]

containerMonsterHPBar = Translate (150) (-170) 
  $ rectangleSolid 100 45

topMonsterHPBar hp = Translate (150 + ((100 - hp) / 2)) (-170) -- shift the text to the middle of the window
  $ rectangleSolid hp 40 -- display it half the original size

monsterHPBarCounter hp = Translate (140) (-180) -- shift the text to the middle of the window
  $ Scale 0.2 0.2 -- display it half the original size
  $ Text hp -- display it half the original size

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
-- END SCENE
--------------------------------------------------------

endText :: Picture
endText =
  Translate (-150) (-120) -- shift the text to the middle of the window
  $ Scale 0.4 0.4 -- display it half the original size
  $ Color red (Text "GAME OVER") -- text to display

tombstone round = Pictures [tombstoneBody, tombstoneHead, tombstoneEngraving round]

tombstoneBody = Translate 0 (-50)
  $ Color greyColor (rectangleSolid 175 300)

tombstoneHead = Translate (-30) 30
  $ Scale 0.3 0.3 -- display it half the original size
  $ Text "R.I.P"

tombstoneEngraving round = Translate (-50) (-20)
  $ Scale 0.2 0.2 -- display it half the original size
  $ Text ("Round: " ++ show round)

--------------------------------------------------------
-- CUSTOM COLORS
--------------------------------------------------------

greyColor :: Color
greyColor = makeColor 0.7 0.7 0.7 1

hpBarColor :: Color
hpBarColor = makeColor 0.5 0.5 0.5 1

lightColor :: Color
lightColor = makeColor 0.3 0.3 0.3 0.5

brownColor :: Color
brownColor = makeColor 0.5 0.3 0.1 1

--------------------------------------------------------
-- UTILS
--------------------------------------------------------

-- Convert a number to a string
showNumber :: Int -> String
showNumber = show