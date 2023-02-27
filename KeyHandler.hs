module KeyHandler where

import Data.Char (isDigit)
import DungeonCrawler

handleEnterKey :: World -> World
handleEnterKey w = 
  if (screenType w) == "start" then
    handleStartEnter w
  else
    if ((screenType w) == "charCreation1") && ((inputText w) /= "") then
        do
            let InternalState player monster level = (internalState w)
            let Character health attack bleed bleed_recieved life_steal priority = player
            World "charCreation2" (seconds w) (InternalState (Character (read (inputText w)) attack bleed bleed_recieved life_steal priority) monster level) ""
    else
        if ((screenType w) == "charCreation2") && ((inputText w) /= "") then
            do
                let InternalState player monster level = (internalState w)
                let Character health attack bleed bleed_recieved life_steal priority = player
                World "charCreation3" (seconds w) (InternalState (Character health (read (inputText w)) bleed bleed_recieved life_steal priority) monster level) ""
        else
            if ((screenType w) == "charCreation3") && ((inputText w) /= "") then
                do
                    let InternalState player monster level = (internalState w)
                    let Character health attack bleed bleed_recieved life_steal priority = player
                    World "charCreation4" (seconds w) (InternalState (Character health attack (read (inputText w)) bleed_recieved life_steal priority) monster level) ""
            else
                if ((screenType w) == "charCreation4") && ((inputText w) /= "") then
                    do
                        let InternalState player monster level = (internalState w)
                        let Character health attack bleed bleed_recieved life_steal priority = player
                        World "charCreation5" (seconds w) (InternalState (Character health attack bleed bleed_recieved (read (inputText w)) priority) monster level) ""
                else
                    if ((screenType w) == "charCreation5") && ((inputText w) /= "") then
                        do
                            let InternalState player monster level = (internalState w)
                            let Character health attack bleed bleed_recieved life_steal priority = player
                            World "fight" (seconds w) (InternalState (Character health attack bleed bleed_recieved life_steal (read (inputText w))) monster level) ""
                    else
                        w

handleStartEnter :: World -> World
handleStartEnter w = World "charCreation1" (seconds w) (internalState w) (inputText w)

handleCharKey :: Char -> World -> World
handleCharKey c w =
    if (isDigit c) && (elem (screenType w) ["charCreation1", "charCreation2", "charCreation3", "charCreation4", "charCreation5"]) then
       World (screenType w) (seconds w) (internalState w) ((inputText w) ++ [c])
    else
        w

handleDeleteKey :: World -> World
handleDeleteKey w =
    if (elem (screenType w) ["charCreation1", "charCreation2", "charCreation3", "charCreation4", "charCreation5"]) then
        World (screenType w) (seconds w) (internalState w) (deleteOneChar (inputText w))
    else
        w

deleteOneChar :: [a] -> [a]
deleteOneChar s = if null s then [] else init s


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
