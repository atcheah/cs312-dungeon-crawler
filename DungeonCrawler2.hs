module Games
  (State(..), Result(..), Game, Player) where
-- To load it, try:
-- ghci
-- :load DungeonCrawler2
-- main

import System.IO

--------------------------------------------------------
-- Data Definitions for Game
--------------------------------------------------------

data State = State InternalState [Action]  -- internal_state available_actions
--         deriving (Ord, Eq, Show)

data Result = EndOfGame Int    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
      --   deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action



--------------------------------------------------------
-- Data Definitions for Dungen Crawler
--------------------------------------------------------
data Character = Character Int Int
data InternalState = InternalState Character Character Int -- self, monster , level
data Action = Action Int                   -- a move for a player is just an Int that specifies what item they chose
         deriving (Ord,Eq)

------ The Dungen Crawler Game -------

dungeoncrawler :: Game

dungeoncrawler move (State (InternalState self monster round) available) = do
    let player = levelUpCharacter self move
    let (newPlayer, newMonster) = simulateFight player monster
    if ((getHealth newMonster) <= 0) then 
      do
        ContinueGame (State (InternalState player (autoLevelUpCharacter monster) (round + 1)) available)
    else 
      do
        EndOfGame round


autoLevelUpCharacter :: Character -> Character 
autoLevelUpCharacter (Character health attack)=
  Character (health + 3) (attack + 3)

-- simulates a fight between a player and a monster, returns the player and monster after the fight
simulateFight :: Character -> Character -> (Character, Character)
simulateFight player monster = do
    let newMonster = Character ((getHealth monster) - (getAttack player)) (getAttack monster)
    let newPlayer = Character ((getHealth player) - (getAttack newMonster)) (getAttack player)
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


play :: Game -> Result -> IO ()

play game (ContinueGame state) =
   do
      let State internal avail = state
      let InternalState player monster level = internal
      putStrLn "You defeated the monster!"
      putStrLn "You feel stronger with this new experience!"
      statChosen <- chooseStatToUpgrade player
      putStrLn "You venture deeper into the dungeon..."
      putStrLn "And you find a stronger monster!"
      putStrLn "You encounter a monster!"
      let newPlayer = levelUpCharacter player statChosen -- need to do it here for in order to out put the stats before he game
      putStrLn ("Your health: " ++ show (getHealth newPlayer))
      putStrLn ("Your attack: " ++ show (getAttack newPlayer))
      putStrLn ("Monster health: " ++ show (getHealth monster))
      putStrLn ("Monster attack: " ++ show (getAttack monster))
      putStrLn "The monster attacks you! The fight begins!" 
      play game (game statChosen state)

play game (EndOfGame round) =
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
      putStrLn ("Monster health: " ++ show (getHealth monster))
      putStrLn ("Monster attack: " ++ show (getAttack monster))
      putStrLn "The monster attacks you! The fight begins!"
      let result = game (Action 0) state -- no action 
      play game result

-- lets the player choose a stat to upgrade
chooseStatToUpgrade :: Character -> IO Action
chooseStatToUpgrade (Character health attack) = 
  do 
    putStrLn "What Stat would you like to level up?"
    putStrLn "1. Health"
    putStrLn "2. Attack"
    choice <- getLineFixed
    if (choice `elem` ["1", "health", "Health", "h", "H"]) then 
      do
        return (Action 1)
    else 
      if (choice `elem` ["2", "attack", "Attack", "a", "A"]) then 
        do
          return (Action 2)
      else 
        do
          putStrLn "Invalid Choice"
          chooseStatToUpgrade (Character health attack)

-- levels up a player based on their choice
levelUpCharacter :: Character -> Action -> Character
levelUpCharacter (Character health attack) (Action chosenStat) = 
  do 
    if (chosenStat == 1) then 
      do
        (Character (health + 5) attack)
    else 
      if (chosenStat == 2) then 
        do
          (Character health (attack + 5))
      else 
        -- should't get to this other than first level
        do
          (Character health attack)


-- gets the health of a player
getHealth :: Character -> Int
getHealth (Character health _) = health



-- gets the attack of a player
getAttack :: Character -> Int
getAttack (Character _ attack) = attack

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
    let dungencrawler_start = State (InternalState (Character (read health) (read attack)) (Character 10 5) 1) [Action n | n <- [1..2]]
    playFirstRound dungeoncrawler (ContinueGame dungencrawler_start)


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