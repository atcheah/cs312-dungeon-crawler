module UpdateHandler where

updateWorld :: Float -> World -> World
updateWorld secondsPassed world 
  | screenType world == "fight" = world { seconds = (seconds world) + secondsPassed }
  | otherwise = world