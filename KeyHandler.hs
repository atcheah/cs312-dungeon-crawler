module KeyHandler where

import DungeonCrawler

handleEnterKey :: World -> World
handleEnterKey w = 
  if (screenType w) == "start" then
    handleStartEnter w
  else
    w

handleStartEnter :: World -> World
handleStartEnter w = World "charCreation" (seconds w) (internalState w) (inputText w)

handleCharKey :: Char -> World -> World
handleCharKey c w = World "charCreation1" (seconds w) (internalState w) ((inputText w) ++ [c])

KeyDelete

handleOneKey :: World -> World
handleOneKey w = w

handleTwoKey :: World -> World
handleTwoKey w = w

handleThreeKey :: World -> World
handleThreeKey w = w

handleFourKey :: World -> World
handleFourKey w = w

handleFiveKey :: World -> World
handleFiveKey w = w

handleSixKey :: World -> World
handleSixKey w = w
