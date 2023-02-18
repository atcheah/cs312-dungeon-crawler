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

--------------------------------------------------------
-- Data Definitions
--------------------------------------------------------

data Player = Player Int Int
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
    putStrLn "What is your character's attack?"
    attack <- getLineFixed
    putStrLn "What is your charaacter's health?"
    health <- getLineFixed
    play (Game (Player (read health) (read attack)) (Player 10 5) 1)

-- handles playing the game loop
play :: Game -> IO ()
play (Game player monster round) = do
    putStrLn "You encounter a monster!"
    putStrLn ("Your health: " ++ show (getHealth player))
    putStrLn ("Your attack: " ++ show (getAttack player))
    putStrLn ("Monster health: " ++ show (getHealth monster))
    putStrLn ("Monster attack: " ++ show (getAttack monster))
    putStrLn "The monster attacks you! The fight begins!"
    let (newPlayer, newMonster) = simulateFight player monster
    if ((getHealth newMonster) <= 0) then 
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
simulateFight player monster = do
    let newMonster = Player ((getHealth monster) - (getAttack player)) (getAttack monster)
    let newPlayer = Player ((getHealth player) - (getAttack newMonster)) (getAttack player)
    -- assumes player's attack has priority
    if ((getHealth newMonster) <= 0) then 
      do
        (newPlayer, newMonster)
    else 
      if ((getHealth newPlayer) <= 0) then 
        do
          (newPlayer, newMonster)
      else 
        do
          simulateFight newPlayer newMonster

-- levels up a player based on their choice
levelUpPlayer :: Player -> IO Player
levelUpPlayer (Player health attack) = 
  do 
    putStrLn "What Stat would you like to level up?"
    putStrLn "1. Health"
    putStrLn "2. Attack"
    choice <- getLineFixed
    if (choice `elem` ["1", "health", "Health", "h", "H"]) then 
      do
        return (Player (health + 5) attack)
    else 
      if (choice `elem` ["2", "attack", "Attack", "a", "A"]) then 
        do
          return (Player health (attack + 5))
      else 
        do
          putStrLn "Invalid Choice"
          levelUpPlayer (Player health attack)

-- automatically levels up a player, meant for monster usage
autoLevelUpPlayer :: Player -> Player 
autoLevelUpPlayer (Player health attack)=
  Player (health + 3) (attack + 3)

-- gets the health of a player
getHealth :: Player -> Int
getHealth (Player health _) = health

-- gets the attack of a player
getAttack :: Player -> Int
getAttack (Player _ attack) = attack

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
