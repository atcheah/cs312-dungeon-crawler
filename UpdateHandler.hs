module UpdateHandler where

import DungeonCrawler

updateWorld :: Float -> World -> World
updateWorld secondsPassed world = World (screenType world) (seconds world + secondsPassed) (internalState world) (inputText world)