handleEnterKey :: World -> World
handleEnterKey w = let screen = screenType w 
                    newWorld
                      | screen == "start" = handleStartEnter w
                      | otherwise = w
						      in newWorld