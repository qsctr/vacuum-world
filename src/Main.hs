{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Process
import Text.Show.Pretty

data Vacuum = L | R deriving (Eq, Show)
data Action = MoveL | MoveR | Suck | NoOp deriving (Eq, Show)
data State = State
    { vacuum :: Vacuum
    , dirtL :: Bool
    , dirtR :: Bool
    , action :: Action
    , playing :: Bool
    , cost :: Int
    , vacuumPicture :: Picture
    , dirtPicture :: Picture
    } deriving (Show)

main :: IO ()
main = do
    vacuumPictureM <- loadJuicyPNG "vacuum.png"
    dirtPictureM <- loadJuicyPNG "dirt.png"
    case sequence [vacuumPictureM, dirtPictureM] of
        Just [vacuumPicture, dirtPicture] -> do
            let initial = State
                { vacuum = L
                , dirtL = True
                , dirtR = True
                , action = NoOp
                , playing = False
                , cost = 0
                , .. }
            _ <- printState initial
            playIO (InWindow "Vacuum world" (820, 420) (500, 200)) white 1 initial (return . draw) eventHandler $ const update
        Nothing -> putStrLn "Failed to read pictures, press enter to exit" >> getLine >> return ()

update :: State -> IO State
update state@State {..}
    | playing = printState $ act { cost = cost + (if decide == NoOp then 0 else 1), action = decide }
    | otherwise = return state
    where   sense = vacuum == L && dirtL || vacuum == R && dirtR
            decide
                | sense = Suck
                | vacuum == L = if dirtR then MoveR else NoOp
                | vacuum == R = if dirtL then MoveL else NoOp
            act = case decide of
                MoveR -> state { vacuum = R }
                MoveL -> state { vacuum = L }
                Suck -> if vacuum == L then state { dirtL = False } else state { dirtR = False }
                NoOp -> state

draw :: State -> Picture
draw State {..} = pictures [vacuumPicture', dirtPictureL, dirtPictureR, cellL, cellR]
    where   vacuumPicture' = translate (if vacuum == L then -200 else 200) 75 vacuumPicture
            dirtPictureL = if dirtL then translate (-200) (-75) dirtPicture else blank
            dirtPictureR = if dirtR then translate 200 (-75) dirtPicture else blank
            cellL = translate (-200) 0 cell
            cellR = translate 200 0 cell
            cell = rectangleWire 400 400

eventHandler :: Event -> State -> IO State
eventHandler (EventKey (MouseButton LeftButton) Down _ (x, y)) state@State {..}
    | x < 0 && y > 0 = printState $ state { vacuum = L }
    | x > 0 && y > 0 = printState $ state { vacuum = R }
    | x < 0 && y < 0 = printState $ state { dirtL = not dirtL }
    | x > 0 && y < 0 = printState $ state { dirtR = not dirtR }
eventHandler (EventKey (SpecialKey KeySpace) Down _ _) state@State {..}
    | playing = printState $ state { playing = False }
    | otherwise = printState $ state { playing = True, cost = 0 }
eventHandler _ state = return state

printState :: State -> IO State
printState state = do
    callCommand "cls" -- only works on Windows
    putStrLn "Click to change stuff, space bar to play/pause"
    putStrLn "Picture credit: the AIMA slides pdf"
    pPrint state
    return state