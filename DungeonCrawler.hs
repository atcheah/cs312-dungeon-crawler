module DungeonCrawler where
-- To load it, try:
-- ghci
-- :load DungeonCrawler
-- main

import System.IO

--------------------------------------------------------
-- Data Definitions for Game
--------------------------------------------------------

data World = World {
  screenType :: String,
  seconds :: Float,
  internalState :: InternalState,
  inputText :: String
}

data State = State InternalState [Action]  -- internal_state available_actions
--         deriving (Ord, Eq, Show)

data Result = EndOfGame Int    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
      --   deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

amountToLevelUpBy :: Int
amountToLevelUpBy = 5

--------------------------------------------------------
-- Data Definitions for Dungen Crawler
--------------------------------------------------------
-- Character is a 
--   health value representing the player's health, 
--   max_health value representing the player's max health,
--   damage value representing the player's attack damage, 
--   bleed value representing the player's bleed damage they inflict per attack, 
--   bleed_recieved value representing the player's bleed damage they have recieved,
--   life_steal value representing the player's life steal value (how much they heal per attack),
--   priority (higher priority means they attack first), 
data Character = Character Int Int Int Int Int Int Int

data InternalState = InternalState Character Character Int -- hero, monster, round

data Action = Action Int                   -- a move for a player is just an Int that specifies what item they chose
         deriving (Ord,Eq)

------ The Dungeon Crawler Game -------

--------------------------------------------------------
-- GAME FUNCTIONS
--------------------------------------------------------

-- simulates a fight between a player and a monster, returns the player and monster after the fight
-- used for smartLevelUpMonster in KeyHandler.hs to determine the amount of turns a monster can last in battle
simulateFight :: Character -> Character -> Int -> (Character, Character, Int)
simulateFight monster player turns = do
    let newMonster = Character (getHealth monster - getAttack player - getBleedRecieved monster + getLifeSteal monster - getLifeSteal player)
                            (getMaxHealth monster)
                            (getAttack monster)
                            (getBleed monster)
                            (getBleedRecieved monster + getBleed player)
                            (getLifeSteal monster)
                            (getPriority monster)
    let newPlayer = Character (getHealth player - getAttack newMonster - getBleedRecieved player + getLifeSteal player - getLifeSteal monster)
                            (getMaxHealth player)
                            (getAttack player)
                            (getBleed player)
                            (getBleedRecieved player + getBleed monster)
                            (getLifeSteal player)
                            (getPriority player)
    if (getHealth newPlayer <= 0) && (getHealth newMonster > 0) then -- monster wins this is the best option so weight it highest
      do
        (newMonster, newPlayer, turns + 100)
    else
      if
        (getHealth newMonster <= 0) then -- If the monster doesn't win it is safe to assume it lost. Return the number of turns.
        do
          (newMonster, newPlayer, turns + 1)
      else
        do
          simulateFight newMonster newPlayer (turns + 1)

-- levels up a character based on the given action
levelUpCharacter :: Character -> Action -> Character
levelUpCharacter (Character health max_health attack bleed bleed_recieved life_steal priority) (Action chosenStat) = 
  do 
    if (chosenStat == 1) then 
      do
        (Character (health + amountToLevelUpBy) (max_health + amountToLevelUpBy) attack bleed bleed_recieved life_steal priority)
    else 
      if (chosenStat == 2) then 
        do
          (Character health max_health (attack + amountToLevelUpBy) bleed bleed_recieved life_steal priority)
      else 
        if (chosenStat == 3) then 
          do
            (Character health max_health attack (bleed + amountToLevelUpBy) bleed_recieved life_steal priority)
        else
          if (chosenStat == 4) then
            do
              (Character health max_health attack bleed bleed_recieved (life_steal + amountToLevelUpBy) priority)
           else
             if (chosenStat == 5) then
               do
                 (Character health max_health attack bleed bleed_recieved life_steal (priority + amountToLevelUpBy))
             else
               do -- should't get to this other than first level
                 (Character health max_health attack bleed bleed_recieved life_steal priority)

-- heals a character, and resets bleed recieved
healCharacter :: Character -> Character
healCharacter (Character health max_health attack bleed bleed_recieved life_steal priority) = Character max_health max_health attack bleed 0 life_steal priority

-- levels up a character, choosing the stat to level up by seeing which stat will give the character the most turns in battle
smartLevelUpMonster :: Character -> Character -> Character
smartLevelUpMonster hero monster =
  do
    let maxHealthMonster = getMaxHealth monster
    let attackMonster = getAttack monster
    let bleedMonster = getBleed monster
    let bleedRecievedMonster = getBleedRecieved monster
    let lifeStealMonster = getLifeSteal monster
    let priorityMonster = getPriority monster

    let healthMonster = Character (maxHealthMonster + amountToLevelUpBy)
                              (maxHealthMonster + amountToLevelUpBy)
                              attackMonster
                              bleedMonster
                              bleedRecievedMonster
                              lifeStealMonster
                              priorityMonster
    let attackMonster = Character maxHealthMonster
                              maxHealthMonster
                              (attackMonster + amountToLevelUpBy)
                              bleedMonster
                              bleedRecievedMonster
                              lifeStealMonster
                              priorityMonster
    let bleedMonster = Character maxHealthMonster
                              maxHealthMonster
                              attackMonster
                              (bleedMonster + amountToLevelUpBy)
                              bleedRecievedMonster
                              lifeStealMonster
                              priorityMonster
    let lifeStealMonster = Character maxHealthMonster
                              maxHealthMonster
                              attackMonster
                              bleedMonster
                              bleedRecievedMonster
                              (lifeStealMonster + amountToLevelUpBy)
                              priorityMonster  
    let priorityMonster = Character maxHealthMonster
                              maxHealthMonster
                              attackMonster
                              bleedMonster
                              bleedRecievedMonster
                              lifeStealMonster
                              (priorityMonster + amountToLevelUpBy)

    let health = numberOfTurns healthMonster hero 0
    let attack = numberOfTurns attackMonster hero 0
    let bleed = numberOfTurns bleedMonster hero 0
    let ls = numberOfTurns lifeStealMonster hero 0
    let prio = numberOfTurns priorityMonster hero 0

    -- maybe we could sort here?? 
    if (health > attack) && (health > bleed) && (health > ls) && (health > prio) then healthMonster
    else
      if (attack > health) && (attack > bleed) && (attack > ls) && (attack > prio) then attackMonster
      else
        if (bleed > health) && (bleed > attack)  && (bleed > ls) && (bleed > prio) then bleedMonster
        else
          if (ls > health) && (ls > attack)  && (ls > bleed) && (ls > prio) then lifeStealMonster
          else
            if (prio > health) && (prio > attack)  && (prio > bleed) && (prio > ls) then priorityMonster
            else healthMonster -- they are all equal so just get more health.

-- returns the number of turns it takes for the fight to end, and if the monster wins, adds 1000 to ensure it is considered the best option
numberOfTurns :: Character -> Character -> Int -> Int
numberOfTurns monster hero turns = 
  do
    let attackHero = getAttack hero
    let bleedHero = getBleed hero
    let bleedRecievedHero = getBleedRecieved hero
    let lifeStealHero = getLifeSteal hero
    let attackMonster = getAttack monster
    let bleedMonster = getBleed monster
    let bleedRecievedMonster = getBleedRecieved monster
    let lifeStealMonster = getLifeSteal monster
    let heroPriority = getPriority hero
    let monsterPriority = getPriority monster

    let newMonster = Character (getHealth monster - attackHero - bleedRecievedMonster + lifeStealMonster)
                            (getMaxHealth monster)
                            attackMonster
                            bleedMonster
                            (bleedRecievedMonster + bleedHero)
                            lifeStealMonster
                            monsterPriority
    let newHero = Character (getHealth hero - attackMonster - bleedRecievedHero + lifeStealHero)
                            (getMaxHealth hero)
                            attackHero
                            bleedHero
                            (bleedRecievedHero + bleedMonster)
                            lifeStealHero
                            heroPriority

    let heroHealth = getHealth newHero
    let monsterHealth = getHealth newMonster
    -- is fight over?
    if (monsterHealth <= 0) || (heroHealth <= 0) then
      do
        --  check priority to solve tie
        if (heroHealth <= 0) && (monsterHealth <= 0) then
          do
            -- hero has priority, hero wins ties
            if (heroPriority >= monsterPriority) then
              do
                (turns + 1)
            -- monster has priority
            else 
              do
                (turns + 1000)
        -- hero dead, monster lives
        else 
          if (heroHealth <= 0) then
            do
              (turns + 1000)
          -- monster dead, hero lives
          else 
            do
              (turns + 1)
    -- continue fight
    else
      do
        numberOfTurns newMonster newHero (turns + 1)

--------------------------------------------------------
-- GETTERS
--------------------------------------------------------

-- gets the health of a player
getHealth :: Character -> Int
getHealth (Character health _ _ _ _ _ _) = health

getMaxHealth :: Character -> Int
getMaxHealth (Character _ max_health _ _ _ _ _) = max_health

-- gets the attack of a player
getAttack :: Character -> Int
getAttack (Character _ _ attack _ _ _ _) = attack

-- gets the bleed of a player
getBleed :: Character -> Int
getBleed (Character _ _ _ bleed _ _ _) = bleed

-- gets the bleed recieved of a player
getBleedRecieved :: Character -> Int
getBleedRecieved (Character _ _ _ _ bleed_recieved _ _) = bleed_recieved

-- gets the life steal of a player
getLifeSteal :: Character -> Int
getLifeSteal (Character _ _ _ _ _ life_steal _) = life_steal

-- gets the priority of a player
getPriority :: Character -> Int
getPriority (Character _ _ _ _ _ _ priority) = priority

-- gets the hero from the InternalState
getHero :: InternalState -> Character
getHero (InternalState hero _ _) = hero

-- gets the monster from the InternalState
getMonster :: InternalState -> Character
getMonster (InternalState _ monster _) = monster

-- gets the round from the InternalState
getRound :: InternalState -> Int
getRound (InternalState _ _ round) = round