module UpdateHandler where

import DungeonCrawler

updateWorld :: Float -> World -> World
updateWorld secondsPassed world 
  | (screenType world) == "fight" = updateFight secondsPassed world
  | otherwise = World (screenType world) (seconds world + secondsPassed) (internalState world)

-- TODO: 
-- should be do essentially what simFight does except, except returning the new fight state
-- instead of recursing until fight is over and navigate to the right result screen (level up or end)
updateFight :: Float -> World -> World
updateFight secondsPassed world = world 