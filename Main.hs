import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed
import DungeonCrawler
import KeyHandler
import UpdateHandler
import RenderHandler

-- To run
-- ghci -fno-ghci-sandbox
-- :load Main
-- main

main :: IO ()
main = play (InWindow "Dungeon Crawler" (800, 600) (10, 10)) black 30 (World "levelUp" 0.0 (InternalState (Character 0 0 0 0 0 0) (Character 10 5 0 0 0 1) 1)) render handleKeys update

-- needs to take a float representing time for start
render :: World -> Picture
render world = renderHandler world world

handleKeys :: Event -> World -> World
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) world = handleEnterKey world
handleKeys (EventKey (Char '1') Down _ _) world = handleOneKey world
handleKeys (EventKey (Char '2') Down _ _) world = handleTwoKey world
handleKeys (EventKey (Char '3') Down _ _) world = handleThreeKey world
handleKeys (EventKey (Char '4') Down _ _) world = handleFourKey world
handleKeys (EventKey (Char '5') Down _ _) world = handleFiveKey world
handleKeys (EventKey (Char '6') Down _ _) world = handleSixKey world
handleKeys _ world = world

update :: Float -> World -> World 
update secondsPassed world
  | screenType world == "fight" = updateWorld secondsPassed world
  | otherwise = world