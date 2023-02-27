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
main = play (InWindow "Dungeon Crawler" (800, 600) (10, 10)) black 1 (World "start" 0.0 (InternalState (Character 0 0 0 0 0 0) (Character 10 5 0 0 0 1) 1) "") render handleKeys update

-- needs to take a float representing time for start
render :: World -> Picture
render world = renderHandler world world

handleKeys :: Event -> World -> World
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) world = handleEnterKey world
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) world = handleMouseClick x y world
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) world = handleDeleteKey world
handleKeys (EventKey (Char c) Down _ _) world = handleCharKey c world
handleKeys _ world = world

update :: Float -> World -> World 
update secondsPassed world = updateWorld secondsPassed world