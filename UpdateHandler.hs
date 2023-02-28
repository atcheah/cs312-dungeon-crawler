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
    let heroHealth = (getHealth (getHero (internalState world)))
    let heroAttack = (getAttack (getHero (internalState world)))
    let heroBleed = (getBleed (getHero (internalState world)))
    let heroBleedRecieved = (getBleedRecieved (getHero (internalState world)))
    let heroLifeSteal = (getLifeSteal (getHero (internalState world)))
    let heroPriority = (getPriority (getHero (internalState world)))

    let monsterHealth = (getHealth (getMonster (internalState world)))
    let monsterAttack = (getAttack (getMonster (internalState world)))
    let monsterBleed = (getBleed (getMonster (internalState world)))
    let monsterBleedRecieved = (getBleedRecieved (getMonster (internalState world)))
    let monsterLifeSteal = (getLifeSteal (getMonster (internalState world)))
    let monsterPriority = (getPriority (getMonster (internalState world)))

    let newMonster = Character (monsterHealth - heroAttack - monsterBleedRecieved + monsterLifeSteal - heroLifeSteal)
                              (monsterAttack)
                              (monsterBleed)
                              (monsterBleedRecieved + heroBleed)
                              (monsterLifeSteal)
                              (monsterPriority)
    let newHero = Character (heroHealth - monsterAttack - heroBleedRecieved + heroLifeSteal - monsterLifeSteal)
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



