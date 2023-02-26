module KeyHandler where

handleEnterKey :: World -> World
handleEnterKey w = let screen = screenType w 
                    newWorld
                      | screen == "start" = w { screenType = "fight" }
                      | otherwise = w
						      in newWorld