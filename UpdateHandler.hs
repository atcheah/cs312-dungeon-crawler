module UpdateHandler where

import DungeonCrawler

updateWorld :: Float -> World -> World
updateWorld secondsPassed world 
  | (screenType world) == "fight" = updateFight secondsPassed world
  | otherwise = World (screenType world) (seconds world + secondsPassed) (internalState world) (inputText world)

-- TODO: 
-- should be do essentially what simFight does except, except returning the new fight state
-- instead of recursing until fight is over and navigate to the right result screen (level up or end)
updateFight :: Float -> World -> World
updateFight secondsPassed world = 
  do
    -- bar <- World (screenType world) (seconds world + secondsPassed) (internalState world) (inputText world)
    let state = (internalState world)
    let hero = (getHero state)
    let monster = (getMonster state)

    let heroHealth = (getHealth hero)
    let heroMaxHealth = (getMaxHealth hero)
    let heroAttack = (getAttack hero)
    let heroBleed = (getBleed hero)
    let heroBleedRecieved = (getBleedRecieved hero)
    let heroLifeSteal = (getLifeSteal hero)
    let heroPriority = (getPriority hero)

    let monsterHealth = (getHealth monster)
    let monsterMaxHealth = (getMaxHealth monster)
    let monsterAttack = (getAttack monster)
    let monsterBleed = (getBleed monster)
    let monsterBleedRecieved = (getBleedRecieved monster)
    let monsterLifeSteal = (getLifeSteal monster)
    let monsterPriority = (getPriority monster)

    let newMonster = Character (monsterHealth - heroAttack - monsterBleedRecieved + monsterLifeSteal - heroLifeSteal)
                              (monsterMaxHealth)
                              (monsterAttack)
                              (monsterBleed)
                              (monsterBleedRecieved + heroBleed)
                              (monsterLifeSteal)
                              (monsterPriority)
    let newHero = Character (heroHealth - monsterAttack - heroBleedRecieved + heroLifeSteal - monsterLifeSteal)
                              (heroMaxHealth)
                              (heroAttack)
                              (heroBleed)
                              (heroBleedRecieved + monsterBleed)
                              (heroLifeSteal)
                              (heroPriority)
    -- simulateFightScene newHero newMonster secondsPassed world
    if (getHealth newHero <= 0) || (getHealth newMonster <= 0) then
      do
        World "result" (seconds world + secondsPassed)  (InternalState newHero newMonster (getRound (internalState world))) (inputText world)
    else
      do
        World "fight" (seconds world + secondsPassed) (InternalState newHero newMonster (getRound (internalState world))) (inputText world)



