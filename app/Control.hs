module Control (handleEvent) where

import Graphics.Vty (Event (EvKey), Key (KEsc), Modifier)
import Brick.Types (BrickEvent (VtyEvent, AppEvent), EventM, modify)
import Brick (halt)

import Types

handleEvent :: (ApplicationState -> ApplicationState) -> BrickEvent Resource Tick -> EventM Resource ApplicationState ()
handleEvent tickHandler ev
  | VtyEvent ev' <- ev = handleVtyEvent ev'
  | AppEvent ()  <- ev = modify tickHandler
  | otherwise = pure ()

handleVtyEvent :: Event -> EventM Resource ApplicationState ()
handleVtyEvent (EvKey key mods) = handleKey key mods
handleVtyEvent _ = pure ()

handleKey :: Key -> [Modifier] -> EventM Resource ApplicationState ()
handleKey KEsc _ = halt
handleKey _ _    = pure ()
