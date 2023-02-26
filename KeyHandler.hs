module KeyHandler where

import DungeonCrawler

--------------------------------------------------------
-- TOP LEVEL KEY HANDLERS
--------------------------------------------------------

handleEnterKey :: World -> World
handleEnterKey w = 
  if (screenType w) == "start" then
    handleStartEnter w
  else
    w

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

--------------------------------------------------------
-- SCREEN SPECIFIC KEY HANDLERS
--------------------------------------------------------

handleStartEnter :: World -> World
handleStartEnter w = World "charCreation" (seconds w) (internalState w)
