module DungeonCrawler where
-- To load it, try:
-- ghci
-- :load DungeonCrawler2
-- main

import System.IO

--------------------------------------------------------
-- Data Definitions for Game
--------------------------------------------------------

data World = World {
  screenType :: String,
  seconds :: Float,
  internalState :: InternalState
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
--   damage value representing the player's attack damage, 
--   bleed value representing the player's bleed damage they inflict per attack, 
--   bleed_recieved value representing the player's bleed damage they have recieved,
--   life_steal value representing the player's life steal value (how much they heal per attack),
--   priority (higher priority means they attack first), 
data Character = Character Int Int Int Int Int Int
data InternalState = InternalState Character Character Int -- self, monster, round
data Action = Action Int                   -- a move for a player is just an Int that specifies what item they chose
         deriving (Ord,Eq)

------ The Dungen Crawler Game -------

dungeoncrawler :: Game

dungeoncrawler move (State (InternalState self monster round) available) = do
    let player = levelUpCharacter self move
    let (newPlayer, newMonster, turns) = simulateFight player monster 0
    if ((getHealth newMonster) <= 0) then 
      do
        ContinueGame (State (InternalState player (autoLevelUpCharacter monster) (round + 1)) available)
    else 
      do
        EndOfGame round


autoLevelUpCharacter :: Character -> Character 
autoLevelUpCharacter (Character health attack bleed bleed_recieved life_steal priority) =
  do
    let newHealth = health + 5
    let newAttack = attack + 1
    let newBleed = bleed + 1
    let newBleedRecieved = bleed_recieved + 1
    let newLifeSteal = life_steal + 1
    let newPriority = priority + 1
    Character newHealth newAttack newBleed newBleedRecieved newLifeSteal newPriority

-- simulates a fight between a player and a monster, returns the player and monster after the fight
simulateFight :: Character -> Character -> Int -> (Character, Character, Int)
simulateFight player monster turns = do
    let newMonster = Character (getHealth monster - getAttack player - getBleedRecieved monster + getLifeSteal monster - getLifeSteal player)
                            (getAttack monster)
                            (getBleed monster)
                            (getBleedRecieved monster + getBleed player)
                            (getLifeSteal monster)
                            (getPriority monster)
    let newPlayer = Character (getHealth player - getAttack newMonster - getBleedRecieved player + getLifeSteal player - getLifeSteal monster)
                            (getAttack player)
                            (getBleed player)
                            (getBleedRecieved player + getBleed monster)
                            (getLifeSteal player)
                            (getPriority player)
    if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
      do
        (newPlayer, newMonster, turns + 1)
    else
      do
        simulateFight newPlayer newMonster (turns + 1)

playGame :: Game -> Result -> IO ()

playGame game (ContinueGame state) =
   do
      let State internal avail = state
      let InternalState player monster level = internal
      putStrLn "You defeated the monster!"
      putStrLn "You feel stronger with this new experience!"
      statChosen <- chooseStatToUpgrade player monster
      putStrLn "You venture deeper into the dungeon..."
      putStrLn "And you find a stronger monster!"
      putStrLn "You encounter a monster!"
      let newPlayer = levelUpCharacter player statChosen -- need to do it here for in order to out put the stats before he game
      putStrLn ("Your health: " ++ show (getHealth newPlayer))
      putStrLn ("Your attack: " ++ show (getAttack newPlayer))
      putStrLn ("Your bleed: " ++ show (getBleed newPlayer))
      putStrLn ("Your life steal: " ++ show (getLifeSteal newPlayer))
      putStrLn ("Your priority: " ++ show (getPriority newPlayer))
      putStrLn ("Monster health: " ++ show (getHealth monster))
      putStrLn ("Monster attack: " ++ show (getAttack monster))
      putStrLn ("Monster bleed: " ++ show (getBleed monster))
      putStrLn ("Monster life steal: " ++ show (getLifeSteal monster))
      putStrLn ("Monster priority: " ++ show (getPriority monster))
      putStrLn "The monster attacks you! The fight begins!" 
      playGame game (game statChosen state)

playGame game (EndOfGame round) =
  do
    putStrLn "You were killed by the monster..."
    putStrLn ("You made it to round " ++ show round)
    putStrLn "Game Over! Thanks for playing!"


playFirstRound game (ContinueGame state) =
   do
      let State internal avail = state
      let InternalState player monster level = internal
      putStrLn "You encounter a monster!"
      putStrLn ("Your health: " ++ show (getHealth player))
      putStrLn ("Your attack: " ++ show (getAttack player))
      putStrLn ("Your bleed: " ++ show (getBleed player))
      putStrLn ("Your life steal: " ++ show (getLifeSteal player))
      putStrLn ("Your priority: " ++ show (getPriority player))
      putStrLn ("Monster health: " ++ show (getHealth monster))
      putStrLn ("Monster attack: " ++ show (getAttack monster))
      putStrLn ("Monster bleed: " ++ show (getBleed monster))
      putStrLn ("Monster life steal: " ++ show (getLifeSteal monster))
      putStrLn ("Monster priority: " ++ show (getPriority monster))
      putStrLn "The monster attacks you! The fight begins!"
      let result = game (Action 0) state -- no action 
      playGame game result

-- lets the player choose a stat to upgrade
chooseStatToUpgrade :: Character -> Character -> IO Action
chooseStatToUpgrade (Character health attack bleed bleed_recieved life_steal priority) monster = 
  do 
    putStrLn "What Stat would you like to level up?"
    putStrLn "1. Health"
    putStrLn "2. Attack"
    putStrLn "3. Bleed"
    putStrLn "4. Life Steal"
    putStrLn "5. Priority"
    let player = Character health attack bleed bleed_recieved life_steal priority
    reccomend player monster
    choice <- getLineFixed
    if choice `elem` ["1", "health", "Health", "h", "H"] then
      do
        return (Action 1)
    else
      if choice `elem` ["2", "attack", "Attack", "a", "A"] then
        do
          return (Action 2)
      else
        if choice `elem` ["3", "bleed", "Bleed", "b", "B"] then
          do
            return (Action 3)
        else
          if choice `elem` ["4", "life steal", "Life Steal", "l", "L"] then
            do
              return (Action 4)
          else
            if choice `elem` ["5", "priority", "Priority", "p", "P"] then
              do
                return (Action 5)
            else
              do
                putStrLn "Invalid Choice"
                chooseStatToUpgrade (Character health attack bleed bleed_recieved life_steal priority) monster
          

-- levels up a player based on their choice
levelUpCharacter :: Character -> Action -> Character
levelUpCharacter (Character health attack bleed bleed_recieved life_steal priority) (Action chosenStat) = 
  do 
    if (chosenStat == 1) then 
      do
        (Character (health + amountToLevelUpBy) attack bleed bleed_recieved life_steal priority)
    else 
      if (chosenStat == 2) then 
        do
          (Character health (attack + amountToLevelUpBy) bleed bleed_recieved life_steal priority)
      else 
        if (chosenStat == 3) then 
          do
            (Character health attack (bleed + amountToLevelUpBy) bleed_recieved life_steal priority)
        else
          if (chosenStat == 4) then
            do
              (Character health attack bleed bleed_recieved (life_steal + amountToLevelUpBy) priority)
           else
             if (chosenStat == 5) then
               do
                 (Character health attack bleed bleed_recieved life_steal (priority + amountToLevelUpBy))
             else
               do -- should't get to this other than first level
                 (Character health attack bleed bleed_recieved life_steal priority)

-- handles starting the game
mainGame :: IO ()
mainGame = do
    putStrLn "Welcome to Dungeon Crawler!"
    -- allow char creation for some kind of variability? Otherwise, optimal path is same each time
    -- as if monster levels up randomly, solver function will not be able to return a true optimal path
    -- because it was based on a different monster
    putStrLn "Create your character!"
    putStrLn "What is your character's attack?"
    attack <- getLineFixed
    putStrLn "What is your charaacter's health?"
    health <- getLineFixed
    putStrLn "What is your character's bleed?"
    bleed <- getLineFixed
    putStrLn "What is your character's life steal?"
    life_steal <- getLineFixed
    putStrLn "What is your character's priority?"
    priority <- getLineFixed
    let dungencrawler_start = State (InternalState (Character (read health) (read attack) (read bleed) 0 (read life_steal) (read priority)) (Character 10 5 0 0 0 1) 1) [Action n | n <- [1..5]]
    playFirstRound dungeoncrawler (ContinueGame dungencrawler_start)

------ Recommender System ---------
reccomend :: Character -> Character -> IO()
reccomend player monster =
  do
    let healthPlayer = Character (getHealth player + 5)
                              (getAttack player)
                              (getBleed player)
                              (getBleedRecieved player)
                              (getLifeSteal player)
                              (getPriority player)

    let attackPlayer = Character (getHealth player)
                              (getAttack player + 5)
                              (getBleed player)
                              (getBleedRecieved player)
                              (getLifeSteal player)
                              (getPriority player)

    let bleedPlayer = Character (getHealth player)
                              (getAttack player)
                              (getBleed player + 5)
                              (getBleedRecieved player)
                              (getLifeSteal player)
                              (getPriority player)

    let lifeStealPlayer = Character (getHealth player)
                                (getAttack player)
                                (getBleed player)
                                (getBleedRecieved player)
                                (getLifeSteal player + 5)
                                (getPriority player)
    
    let priorityPlayer = Character (getHealth player)
                                (getAttack player)
                                (getBleed player)
                                (getBleedRecieved player)
                                (getLifeSteal player)
                                (getPriority player + 5)

    let simulatedMonster = autoLevelUpCharacter monster

    let (newHealthPlayer, newMonster, healthTurns) = simulateFight healthPlayer simulatedMonster 0
    let health_rec = getHealth newHealthPlayer

    let (newAttackPlayer, newMonster, attackTurns) = simulateFight attackPlayer simulatedMonster 0
    let attack_rec = getHealth newAttackPlayer

    let (newBleedPlayer, newMonster, bleedTurns) = simulateFight bleedPlayer simulatedMonster 0
    let bleed_rec = getHealth newBleedPlayer

    let (newLSPlayer, newMonster, lsTurns) = simulateFight lifeStealPlayer simulatedMonster 0
    let ls_rec = getHealth newLSPlayer

    let (newPrioPlayer, newMonster, prioTurns) = simulateFight priorityPlayer simulatedMonster 0
    let prio_rec = getHealth newPrioPlayer

    putStrLn " "
    putStrLn "Defensive Reccomendation: "
    if getAttack newMonster > getHealth newLSPlayer then
      do
        chooseBest (>) (health_rec) (attack_rec) (bleed_rec) (-1) (prio_rec)
    else
      do
        chooseBest (<) healthTurns attackTurns bleedTurns lsTurns prioTurns
    putStrLn " "
    putStrLn "Offensive Reccomendation: "
    chooseBest (<) healthTurns attackTurns bleedTurns lsTurns prioTurns

chooseBest :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int -> Int -> IO()
chooseBest op health attack bleed ls prio =
  do
    if (health `op` attack) && (health `op` bleed) && (health `op` ls) && (health `op` prio) then
        do
          putStrLn "Choose Health"
    else
      if (attack `op` health) && (attack `op` bleed) && (attack `op` ls) && (attack `op` prio) then
        do
          putStrLn "Choose Attack"
      else
        if (bleed `op` health) && (bleed `op` attack)  && (bleed `op` ls) && (bleed `op` prio) then
          do
            putStrLn "Choose Bleed"
        else
          if (ls `op` health) && (ls `op` attack)  && (ls `op` bleed) && (ls `op` prio) then
            do
              putStrLn "Choose Life Steal"
          else
            if (prio `op` health) && (prio `op` attack)  && (prio `op` bleed) && (prio `op` ls) then
              do
                putStrLn "Choose Priority"
            else
              do
                putStrLn "Choose Any (preferably Health)" -- they are all equal so just get more health.

--------------------------------------------------------
-- GETTERS
--------------------------------------------------------

-- gets the health of a player
getHealth :: Character -> Int
getHealth (Character health _ _ _ _ _) = health

-- gets the attack of a player
getAttack :: Character -> Int
getAttack (Character _ attack _ _ _ _) = attack

-- gets the bleed of a player
getBleed :: Character -> Int
getBleed (Character _ _ bleed _ _ _) = bleed

-- gets the bleed recieved of a player
getBleedRecieved :: Character -> Int
getBleedRecieved (Character _ _ _ bleed_recieved _ _) = bleed_recieved

-- gets the life steal of a player
getLifeSteal :: Character -> Int
getLifeSteal (Character _ _ _ _ life_steal _) = life_steal

-- gets the priority of a player
getPriority :: Character -> Int
getPriority (Character _ _ _ _ _ priority) = priority

-- gets the hero from the InternalState
getHero :: InternalState -> Character
getHero (InternalState hero _ _) = hero

-- gets the monster from the InternalState
getMonster :: InternalState -> Character
getMonster (InternalState _ monster _) = monster

-- gets the round from the InternalState
getRound :: InternalState -> Int
getRound (InternalState _ _ round) = round


--------------------------------------------------------
-- stolen code from a3 solutions to fix backspace
--------------------------------------------------------

getLineFixed =
   do
     line <- getLine
     return (fixdel line)

-- fixdel removes deleted elements from string
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r