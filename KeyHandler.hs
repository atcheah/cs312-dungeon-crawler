module KeyHandler where

import DungeonCrawler

--------------------------------------------------------
-- TOP LEVEL KEY HANDLERS
--------------------------------------------------------

handleEnterKey :: World -> World
handleEnterKey w = 
  if (screenType w) == "start" then
    handleStartEnter w
  else
    w

handleMouseClick :: Float -> Float -> World -> World
handleMouseClick x y w =
  if (screenType w) == "levelUp" then
    handleLevelUpClick x y w
  else
    w

--------------------------------------------------------
-- SCREEN SPECIFIC KEY HANDLERS
--------------------------------------------------------

handleStartEnter :: World -> World
handleStartEnter w = World "charCreation" (seconds w) (internalState w)

handleLevelUpClick :: Float -> Float -> World -> World
handleLevelUpClick x y w =
  do
    let hero = getHero (internalState w)
    let newRound = getRound (internalState w) + 1
    -- health button
    if (x > (-350) && x < (-150) && y > (-25) && y < 25) then
      (World "fight" (seconds w) (IhcnternalState (levelUpCharacter hero (Action 1)) (getMonster (internalState w)) newRound))
    else 
      -- attack button
      if (x > (-100) && x < 100 && y > (-25) && y < 25) then 
        (World "fight" (seconds w) (InternalState (levelUpCharacter hero (Action 2)) (getMonster (internalState w)) newRound))
      else
        -- bleed button
        if (x > 150 && x < 350 && y > (-25) && y < 25) then
          (World "fight" (seconds w) (InternalState (levelUpCharacter hero (Action 3)) (getMonster (internalState w)) newRound))
        else
          -- life steal button
          if (x > (-225) && x < (-25) && y > (-125) && y < (-75)) then
            (World "fight" (seconds w) (InternalState (levelUpCharacter hero (Action 4)) (getMonster (internalState w)) newRound))
          else
            -- priority button
            if (x > 25 && x < 225 && y > (-125) && y < (-75)) then
              (World "fight" (seconds w) (InternalState (levelUpCharacter hero (Action 5)) (getMonster (internalState w)) newRound))
            else
              w