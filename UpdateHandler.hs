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
    simulateFightScene newHero newMonster secondsPassed world

-- The loop for the autoBattler system. This loop runs till either the player defeats the monster going to the level up screen
-- or till the player loses going to the end screen.
simulateFightScene :: Character -> Character -> Float -> World -> World
simulateFightScene hero monster secondsPassed world = do
    if (getHealth hero <= 0) && (getHealth monster > 0) then
      do
        World "end" (seconds world + secondsPassed) (internalState world) (inputText world)
    else
      if (getHealth monster <= 0) then
        do
          World "levelUp" (seconds world + secondsPassed) (internalState world) (inputText world)
      else
        do
          let newMonster = Character (getHealth monster - getAttack hero - getBleedRecieved monster + getLifeSteal monster - getLifeSteal hero)
                            (getAttack monster)
                            (getBleed monster)
                            (getBleedRecieved monster + getBleed hero)
                            (getLifeSteal monster)
                            (getPriority monster)
          let newHero = Character (getHealth hero - getAttack newMonster - getBleedRecieved hero + getLifeSteal hero - getLifeSteal monster)
                            (getAttack hero)
                            (getBleed hero)
                            (getBleedRecieved hero + getBleed monster)
                            (getLifeSteal hero)
                            (getPriority hero)
          simulateFightScene newHero newMonster secondsPassed world

