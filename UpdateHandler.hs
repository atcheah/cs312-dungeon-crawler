module UpdateHandler where

import DungeonCrawler

updateWorld :: Float -> World -> World
updateWorld secondsPassed world 
  | screenType world == "fight" = world { seconds = ((seconds world) + secondsPassed) }
  | otherwise = world