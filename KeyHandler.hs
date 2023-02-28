module KeyHandler where

import Data.Char (isDigit)
import DungeonCrawler

--------------------------------------------------------
-- TOP LEVEL KEY HANDLERS
--------------------------------------------------------

handleEnterKey :: World -> World
handleEnterKey w = 
  if (screenType w) == "start" then
    handleStartEnter w
  else 
    if (screenType w) == "result" then
      handleResultEnter w
    else
      if ((screenType w) == "charCreation1") && ((inputText w) /= "") then
          do
              let InternalState player monster level = (internalState w)
              let Character health max_health attack bleed bleed_recieved life_steal priority = player
              World "charCreation2" (seconds w) (InternalState (Character (read (inputText w)) (read (inputText w)) attack bleed bleed_recieved life_steal priority) monster level) ""
      else
          if ((screenType w) == "charCreation2") && ((inputText w) /= "") then
              do
                  let InternalState player monster level = (internalState w)
                  let Character health max_health attack bleed bleed_recieved life_steal priority = player
                  World "charCreation3" (seconds w) (InternalState (Character health max_health (read (inputText w)) bleed bleed_recieved life_steal priority) monster level) ""
          else
              if ((screenType w) == "charCreation3") && ((inputText w) /= "") then
                  do
                      let InternalState player monster level = (internalState w)
                      let Character health max_health attack bleed bleed_recieved life_steal priority = player
                      World "charCreation4" (seconds w) (InternalState (Character health max_health attack (read (inputText w)) bleed_recieved life_steal priority) monster level) ""
              else
                  if ((screenType w) == "charCreation4") && ((inputText w) /= "") then
                      do
                          let InternalState player monster level = (internalState w)
                          let Character health max_health attack bleed bleed_recieved life_steal priority = player
                          World "charCreation5" (seconds w) (InternalState (Character health max_health attack bleed bleed_recieved (read (inputText w)) priority) monster level) ""
                  else
                    if ((screenType w) == "charCreation5") && ((inputText w) /= "") then
                        do
                            let InternalState player monster level = (internalState w)
                            let Character health max_health attack bleed bleed_recieved life_steal priority = player
                            World "beginFight" (seconds w) (InternalState (Character health max_health attack bleed bleed_recieved life_steal (read (inputText w))) monster level) ""
                    else
                        if ((screenType w) == "beginFight") then
                            do
                                World "fight" (seconds w) (internalState w) ""
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
handleStartEnter w = World "charCreation1" (seconds w) (internalState w) (inputText w)

handleResultEnter :: World -> World
handleResultEnter w = 
  do
    let hero = getHero (internalState w)
    let monster = getMonster (internalState w)
    let heroHealth = getHealth hero
    let monsterHealth = getHealth monster
    let heroPriority = getPriority hero
    let monsterPriority = getPriority monster
     -- if hero and monster are both dead, calculate priority based win
    if (heroHealth <= 0) && (monsterHealth <= 0) then
      do
        -- hero has priority, hero wins ties
        if (heroPriority >= monsterPriority) then
          do
            World "levelUp" (seconds w) (internalState w) (inputText w)
        -- monster has priority
        else 
          do
            World "end" (seconds w) (internalState w) (inputText w)
    -- hero dead, monster lives
    else 
      if (heroHealth <= 0) then
        do
          World "end" (seconds w) (internalState w) (inputText w)
      -- monster dead, hero lives
      else 
        do
          World "levelUp" (seconds w) (internalState w) (inputText w)

handleCharKey :: Char -> World -> World
handleCharKey c w =
    if (isDigit c) && (elem (screenType w) ["charCreation1", "charCreation2", "charCreation3", "charCreation4", "charCreation5"]) then
       World (screenType w) (seconds w) (internalState w) ((inputText w) ++ [c])
    else
        w

handleDeleteKey :: World -> World
handleDeleteKey w =
    if (elem (screenType w) ["charCreation1", "charCreation2", "charCreation3", "charCreation4", "charCreation5"]) then
        World (screenType w) (seconds w) (internalState w) (deleteOneChar (inputText w))
    else
        w

deleteOneChar :: [a] -> [a]
deleteOneChar s = if null s then [] else init s

handleLevelUpClick :: Float -> Float -> World -> World
handleLevelUpClick x y w =
  do
    let state = internalState w
    let hero = getHero state
    let healedHero = healCharacter hero
    let newMonster = smartLevelUpMonster healedHero (getMonster state)
    let newRound = getRound state + 1
    -- health button
    if (x > (-350) && x < (-150) && y > (-25) && y < 25) then
      (World "beginFight" (seconds w) (InternalState (levelUpCharacter healedHero (Action 1)) newMonster newRound) "")
    else 
      -- attack button
      if (x > (-100) && x < 100 && y > (-25) && y < 25) then 
        (World "beginFight" (seconds w) (InternalState (levelUpCharacter healedHero (Action 2)) newMonster newRound) "")
      else
        -- bleed button
        if (x > 150 && x < 350 && y > (-25) && y < 25) then
          (World "beginFight" (seconds w) (InternalState (levelUpCharacter healedHero (Action 3)) newMonster newRound) "")
        else
          -- life steal button
          if (x > (-225) && x < (-25) && y > (-125) && y < (-75)) then
            (World "beginFight" (seconds w) (InternalState (levelUpCharacter healedHero (Action 4)) newMonster newRound) "")
          else
            -- priority button
            if (x > 25 && x < 225 && y > (-125) && y < (-75)) then
              (World "beginFight" (seconds w) (InternalState (levelUpCharacter healedHero (Action 5)) newMonster newRound) "")
            else
              w