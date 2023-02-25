module DungeonCrawler where
-- A game where you explore a dungeon and fight monsters. Players are represented by a health value and 
-- an attack value. Monsters are just players. The game consists of repeated fights between the player
-- and a monster. The player can level up after each fight, and fights a new monster stronger than before.
-- The game ends when the player loses a fight. 

-- To run it, try:
-- ghci
-- :load DungeonCrawler
-- main

import System.IO
import System.Random

--------------------------------------------------------
-- Data Definitions
--------------------------------------------------------
-- Player is a
--   health value representing the player's health, 
--   damage value representing the player's attack damage, 
--   bleed value representing the player's bleed damage they inflict per attack, 
--   bleed_recieved value representing the player's bleed damage they have recieved,
--   life_steal value representing the player's life steal value (how much they heal per attack),
--   priority (higher priority means they attack first), 
--   crit_chance (chance to crit, 0-100)
--   dodge_chance (chance to dodge, 0-100)
data Player = Player Int Int Int Int Int Int Int Int
data Game = Game Player Player Int

--------------------------------------------------------
-- Game Functions
--------------------------------------------------------

-- handles starting the game
main :: IO ()
main = do
    putStrLn "Welcome to Dungeon Crawler!"
    -- allow char creation for some kind of variability? Otherwise, optimal path is same each time
    -- as if monster levels up randomly, solver function will not be able to return a true optimal path
    -- because it was based on a different monster
    putStrLn "Create your character!"
    putStrLn "What is your character's health?"
    health <- getLineFixed
    putStrLn "What is your character's attack?"
    attack <- getLineFixed
    putStrLn "What is your character's bleed?"
    bleed <- getLineFixed
    putStrLn "What is your character's life steal?"
    life_steal <- getLineFixed
    putStrLn "What is your character's priority?"
    priority <- getLineFixed
    putStrLn "What is your character's crit chance?"
    crit_chance <- getLineFixed
    putStrLn "What is your character's dodge chance?"
    dodge_chance <- getLineFixed
    putStrLn "You venture into the dungeon..."
    play (Game (Player (read health) (read attack) (read bleed) 0 (read life_steal) (read priority) (read crit_chance) (read dodge_chance)) (Player 10 5 0 0 0 1 0 0) 1)

-- handles playing the game loop
play :: Game -> IO ()
play (Game player monster round) =
  do
    putStrLn "You encounter a monster!"
    putStrLn ("Your health: " ++ show (getHealth player))
    putStrLn ("Your attack: " ++ show (getAttack player))
    putStrLn ("Your bleed: " ++ show (getBleed player))
    putStrLn ("Your life steal: " ++ show (getLifeSteal player))
    putStrLn ("Your priority: " ++ show (getPriority player))
    putStrLn ("Your crit chance: " ++ show (getCritChance player))
    putStrLn ("Your dodge chance: " ++ show (getDodgeChance player))
    putStrLn ("Monster health: " ++ show (getHealth monster))
    putStrLn ("Monster attack: " ++ show (getAttack monster))
    putStrLn ("Monster bleed: " ++ show (getBleed monster))
    putStrLn ("Monster life steal: " ++ show (getLifeSteal monster))
    putStrLn ("Monster priority: " ++ show (getPriority monster))
    putStrLn ("Monster crit chance: " ++ show (getCritChance monster))
    putStrLn ("Monster dodge chance: " ++ show (getDodgeChance monster))
    putStrLn "The monster attacks you! The fight begins!"
    let (newPlayer, newMonster) = simulateFight player monster
    if (getHealth newPlayer > 0) && (getHealth newMonster <= 0) then
      do
        putStrLn "You defeated the monster!"
        putStrLn "You feel stronger with this new experience!"
        newPlayer <- levelUpPlayer player
        putStrLn "You venture deeper into the dungeon..."
        putStrLn "And you find a stronger monster!"
        play (Game newPlayer (autoLevelUpPlayer monster) (round + 1))
    else
      if (getHealth newMonster > 0) && (getHealth newPlayer <= 0) then
        do
          putStrLn "You were killed by the monster..."
          putStrLn ("You made it to round " ++ show round)
          putStrLn "Game Over! Thanks for playing!"
      else
        -- players wins ties on priority
        if getPriority newPlayer >= getPriority newMonster then
          do
            putStrLn "You defeated the monster!"
            putStrLn "You feel stronger with this new experience!"
            newPlayer <- levelUpPlayer player
            putStrLn "You venture deeper into the dungeon..."
            putStrLn "And you find a stronger monster!"
            play (Game newPlayer (autoLevelUpPlayer monster) (round + 1))
        else
          do
            putStrLn "You were killed by the monster..."
            putStrLn ("You made it to round " ++ show round)
            putStrLn "Game Over! Thanks for playing!"

-- simulates a fight between a player and a monster, returns the player and monster after the fight
simulateFight :: Player -> Player -> (Player, Player)
simulateFight player monster = 
  do
    -- did the player dodge? if number is less than dodge chance, they dodge as dodge chance is 0-100
    let didPlayerDodge = randomNumber (getHealth player) <= getDodgeChance player
    -- did the monster dodge?
    let didMonsterDodge = randomNumber (getHealth monster) <= getDodgeChance monster
    -- did the player crit?
    let didPlayerCrit = randomNumber (getHealth player) <= getCritChance player
    -- did the monster crit?
    let didMonsterCrit = randomNumber (getHealth monster) <= getCritChance monster
    -- if the player dodged, they take no damage
    if didPlayerDodge && didMonsterDodge then
      do
        -- if both dodge, they both take no damage besides bleed recieved already
        let newPlayer = Player (getHealth player - getBleedRecieved player)
                                (getAttack player)
                                (getBleed player)
                                (getBleedRecieved player)
                                (getLifeSteal player)
                                (getPriority player)
                                (getCritChance player)
                                (getDodgeChance player)
        let newMonster = Player (getHealth monster - getBleedRecieved monster)
                                (getAttack monster)
                                (getBleed monster)
                                (getBleedRecieved monster)
                                (getLifeSteal monster)
                                (getPriority monster)
                                (getCritChance monster)
                                (getDodgeChance monster)
        if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
          do
            (newPlayer, newMonster)
        else
          do
            simulateFight newPlayer newMonster
    else
      do
        if didPlayerDodge then
          do
            -- if the player dodged, they take no damage, and they must have hit the monster
            let newPlayer = Player  (getHealth player - getBleedRecieved player + getLifeSteal player)
                                    (getAttack player)
                                    (getBleed player)
                                    (getBleedRecieved player)
                                    (getLifeSteal player)
                                    (getPriority player)
                                    (getCritChance player)
                                    (getDodgeChance player)
            if didPlayerCrit then
              do
                -- if the player crit, they deal double damage
                let newMonster = Player (getHealth monster - (getAttack player * 2) - getBleedRecieved monster)
                                        (getAttack monster)
                                        (getBleed monster)
                                        (getBleedRecieved monster + getBleed player)
                                        (getLifeSteal monster)
                                        (getPriority monster)
                                        (getCritChance monster)
                                        (getDodgeChance monster)
                if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                  do
                    (newPlayer, newMonster)
                else
                  do
                    simulateFight newPlayer newMonster
            else
              do
                -- if the player didn't crit, they deal normal damage
                let newMonster = Player (getHealth monster - getAttack player - getBleedRecieved monster)
                                        (getAttack monster)
                                        (getBleed monster)
                                        (getBleedRecieved monster + getBleed player)
                                        (getLifeSteal monster)
                                        (getPriority monster)
                                        (getCritChance monster)
                                        (getDodgeChance monster)
                if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                  do
                    (newPlayer, newMonster)
                else
                  do
                    simulateFight newPlayer newMonster
        else
          do
            if didMonsterDodge then
              do
                -- if the monster dodged, they take no damage, and they must have hit the player
                let newMonster = Player (getHealth monster - getBleedRecieved monster + getLifeSteal monster)
                                        (getAttack monster)
                                        (getBleed monster)
                                        (getBleedRecieved monster)
                                        (getLifeSteal monster)
                                        (getPriority monster)
                                        (getCritChance monster)
                                        (getDodgeChance monster)
                if didMonsterCrit then
                  do
                    -- if the monster crit, they deal double damage
                    let newPlayer = Player (getHealth player - (getAttack monster * 2) - getBleedRecieved player)
                                            (getAttack player)
                                            (getBleed player)
                                            (getBleedRecieved player + getBleed monster)
                                            (getLifeSteal player)
                                            (getPriority player)
                                            (getCritChance player)
                                            (getDodgeChance player)
                    if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                      do
                        (newPlayer, newMonster)
                    else
                      do
                        simulateFight newPlayer newMonster
                else
                  do
                    -- if the monster didn't crit, they deal normal damage
                  let newPlayer = Player (getHealth player - getAttack monster - getBleedRecieved player)
                                          (getAttack player)
                                          (getBleed player)
                                          (getBleedRecieved player + getBleed monster)
                                          (getLifeSteal player)
                                          (getPriority player)
                                          (getCritChance player)
                                          (getDodgeChance player)
                  if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                    do
                      (newPlayer, newMonster)
                  else
                    do
                      simulateFight newPlayer newMonster
            else
              do
                -- if neither dodged, they both take damage
                if didPlayerCrit then
                  do
                    -- if the player crit, they deal double damage
                    let newMonster = Player (getHealth monster - (getAttack player * 2) - getBleedRecieved monster + getLifeSteal monster)
                                            (getAttack monster)
                                            (getBleed monster)
                                            (getBleedRecieved monster + getBleed player)
                                            (getLifeSteal monster)
                                            (getPriority monster)
                                            (getCritChance monster)
                                            (getDodgeChance monster)
                    if didMonsterCrit then
                      do
                        -- if the monster crit, they deal double damage
                        let newPlayer = Player (getHealth player - (getAttack monster * 2) - getBleedRecieved player + getLifeSteal player)
                                                (getAttack player)
                                                (getBleed player)
                                                (getBleedRecieved player + getBleed monster)
                                                (getLifeSteal player)
                                                (getPriority player)
                                                (getCritChance player)
                                                (getDodgeChance player)
                        if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                          do
                            (newPlayer, newMonster)
                        else
                          do
                            simulateFight newPlayer newMonster
                    else
                      do
                        -- if the monster didn't crit, they deal normal damage
                        let newPlayer = Player (getHealth player - getAttack monster - getBleedRecieved player + getLifeSteal player)
                                                (getAttack player)
                                                (getBleed player)
                                                (getBleedRecieved player + getBleed monster)
                                                (getLifeSteal player)
                                                (getPriority player)
                                                (getCritChance player)
                                            (getDodgeChance player)
                        if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                          do
                            (newPlayer, newMonster)
                        else
                          do
                            simulateFight newPlayer newMonster
                else
                  do
                    -- if the player didn't crit, they deal normal damage
                    let newMonster = Player (getHealth monster - getAttack player - getBleedRecieved monster + getLifeSteal monster)
                                            (getAttack monster)
                                            (getBleed monster)
                                            (getBleedRecieved monster + getBleed player)
                                            (getLifeSteal monster)
                                            (getPriority monster)
                                            (getCritChance monster)
                                            (getDodgeChance monster)
                    if didMonsterCrit then
                      do
                        -- if the monster crit, they deal double damage
                        let newPlayer = Player (getHealth player - (getAttack monster * 2) - getBleedRecieved player + getLifeSteal player)
                                                (getAttack player)
                                                (getBleed player)
                                                (getBleedRecieved player + getBleed monster)
                                                (getLifeSteal player)
                                                (getPriority player)
                                                (getCritChance player)
                                                (getDodgeChance player)
                        if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                          do
                            (newPlayer, newMonster)
                        else
                          do
                            simulateFight newPlayer newMonster
                    else
                      do
                        -- if the monster didn't crit, they deal normal damage
                        let newPlayer = Player (getHealth player - getAttack monster - getBleedRecieved player + getLifeSteal player)
                                                (getAttack player)
                                                (getBleed player)
                                                (getBleedRecieved player + getBleed monster)
                                                (getLifeSteal player)
                                                (getPriority player)
                                                (getCritChance player)
                                                (getDodgeChance player)
                        if (getHealth newPlayer <= 0) || (getHealth newMonster <= 0) then
                          do
                            (newPlayer, newMonster)
                        else
                          do
                            simulateFight newPlayer newMonster

-- levels up a player based on their choice
levelUpPlayer :: Player -> IO Player
levelUpPlayer (Player health attack bleed bleed_recieved life_steal priority crit_chance dodge_chance) =
  do
    let amntToLevelUpBy = 5
    putStrLn "What Stat would you like to level up?"
    putStrLn "1. Health"
    putStrLn "2. Attack"
    putStrLn "3. Bleed"
    putStrLn "4. Life Steal"
    putStrLn "5. Priority"
    putStrLn "6. Crit Chance"
    putStrLn "7. Dodge Chance"
    choice <- getLineFixed
    if choice `elem` ["1", "health", "Health", "h", "H"] then
      do
        return (Player (health + amntToLevelUpBy) attack bleed bleed_recieved life_steal priority crit_chance dodge_chance)
    else
      if choice `elem` ["2", "attack", "Attack", "a", "A"] then
        do
          return (Player health (attack + amntToLevelUpBy) bleed bleed_recieved life_steal priority crit_chance dodge_chance)
      else
        if choice `elem` ["3", "bleed", "Bleed", "b", "B"] then
          do
            return (Player health attack (bleed + amntToLevelUpBy) bleed_recieved life_steal priority crit_chance dodge_chance)
        else
          if choice `elem` ["4", "life steal", "Life Steal", "l", "L"] then
            do
              return (Player health attack bleed bleed_recieved (life_steal + amntToLevelUpBy) priority crit_chance dodge_chance)
          else
            if choice `elem` ["5", "priority", "Priority", "p", "P"] then
              do
                return (Player health attack bleed bleed_recieved life_steal (priority + amntToLevelUpBy) crit_chance dodge_chance)
            else
              if choice `elem` ["6", "crit chance", "Crit Chance", "c", "C"] then
                do
                  return (Player health attack bleed bleed_recieved life_steal priority (crit_chance + amntToLevelUpBy) dodge_chance)
              else
                if choice `elem` ["7", "dodge chance", "Dodge Chance", "d", "D"] then
                  do
                    return (Player health attack bleed bleed_recieved life_steal priority crit_chance (dodge_chance + amntToLevelUpBy))
                else
                  do
                    putStrLn "Invalid Choice"
                    levelUpPlayer (Player health attack bleed bleed_recieved life_steal priority crit_chance dodge_chance)

-- automatically levels up a player, meant for monster usage
autoLevelUpPlayer :: Player -> Player
autoLevelUpPlayer (Player health attack bleed bleed_recieved life_steal priority crit_chance dodge_chance) =
  do
    let newHealth = health + 1
    let newAttack = attack + 1
    let newBleed = bleed + 1
    let newBleedRecieved = bleed_recieved + 1
    let newLifeSteal = life_steal + 1
    let newPriority = priority + 1
    let newCritChance = crit_chance + 1
    let newDodgeChance = dodge_chance + 1
    Player newHealth newAttack newBleed newBleedRecieved newLifeSteal newPriority newCritChance newDodgeChance

-- gets the health of a player
getHealth :: Player -> Int
getHealth (Player health _ _ _ _ _ _ _) = health


-- gets the attack of a player
getAttack :: Player -> Int
getAttack (Player _ attack _ _ _ _ _ _) = attack

-- gets the bleed of a player
getBleed :: Player -> Int
getBleed (Player _ _ bleed _ _ _ _ _) = bleed

-- gets the bleed recieved of a player
getBleedRecieved :: Player -> Int
getBleedRecieved (Player _ _ _ bleed_recieved _ _ _ _) = bleed_recieved

-- gets the life steal of a player
getLifeSteal :: Player -> Int
getLifeSteal (Player _ _ _ _ life_steal _ _ _) = life_steal

-- gets the priority of a player
getPriority :: Player -> Int
getPriority (Player _ _ _ _ _ priority _ _) = priority

-- gets the crit chance of a player
getCritChance :: Player -> Int
getCritChance (Player _ _ _ _ _ _ crit_chance _) = crit_chance

-- gets the dodge chance of a player
getDodgeChance :: Player -> Int
getDodgeChance (Player _ _ _ _ _ _ _ dodge_chance) = dodge_chance

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

-- checks if an IO Int is larger than an Int and returns a Bool
isLargerThan :: IO Int -> Int -> IO Bool
isLargerThan ioInt int =
  do
    int2 <- ioInt
    if int2 > int then
      do
        return True
    else
      do
        return False


randomNumber :: Int -> Int
randomNumber x = fst $ randomR (1, 100) (mkStdGen x)