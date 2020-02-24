module AIGame where

import Data.List (intercalate)
import System.Random.Shuffle (shuffleM)

{- Retrieves the element that results in the minimum element once
evaluated by the given function -}
argmin :: Ord b => (a -> b) -> [a] -> a
argmin f xs = let
    v = minimum (map f xs)
    in head $ filter (\x -> f x == v) xs

{- Retrieves the element that results in the maximum element once
evaluated by the given function -}
argmax :: Ord b => (a -> b) -> [a] -> a
argmax f xs = let
    v = maximum (map f xs)
    in head $ filter (\x -> f x == v) xs

{- Retrieves the value of an (evaluation,depth) pair, so
that if the evaluation is positive, the depth is counted negativelly.
-}
evalValue :: (Float,Int) -> (Float,Int)
evalValue (v,d) = (v,if v>=0 then -d else d)

{- Checks the decision tree up to the given deepness level,
retriving an estimation of the quality of the current state
choice: a function gives all the states thay may raise from the current decision.
eval: an evaluation function for the game states.
dp: deepness of the decision tree
The first player will try to maximize the evaluation function while the adversary minimizes.
-}
deepEval :: (s -> [s]) -> (s -> Float) -> Int -> s -> (Float,Int)
deepEval choice eval 0 st  = (eval st,1)
deepEval choice eval dp st = let
    -- choices from this state
    chs = choice st
    -- | negative evaluation function
    negeval = (*(-1)) . eval
    -- | evaluation for a given choice assuming smart adversary
    result ch = (\(p,d) -> (-p,d)) $ deepEval choice negeval (dp-1) ch
    --
    in if null chs then
        -- evaluate the current state, can't go deeper.
        (eval st,0)
    else
        -- pick what maximizes the pair (eval,time)
        argmax evalValue (map result chs)

{- Artificial intelligence that checks the decision tree up to the given deepness level.
cchoice: a function gives all the available decisions with their codenames.
         Should not retrieve an empty list for the current state.
eval: an evaluation function for the game states.
dp: deepness of the decision tree
st: current game state
The AI will try to maximize the evaluation function while the adversary minimizes.
The return value is the best computed choice.
-}
ai :: (s -> [(String,s)]) -> (s -> Float) -> Int -> s -> IO String
ai cchoice eval dp st = do
    -- | available choices, shuffled
    chs <- shuffleM (cchoice st)
    -- | negative evaluation function
    let negeval = (*(-1)) . eval
    -- | evaluation for a given choice asuming smart adversary
    let result ch = (\(p,d) -> (-p,d)) $ deepEval ((map snd) . cchoice) negeval dp ch
    -- | best possible choice, that maximizes evaluation assuming smart adversary
    -- | (on tie use the deepness)
    let (chname,_) = argmax (evalValue . result . snd) chs
    return chname -- retrieve code of the best choice.

{- Asks the user to pick a choice from the available ones.
If the player inserst an invalid choice (s)he is asked again. -}
getChoice :: [String] -> IO String
getChoice options = do
    putStrLn $ "Insert choice ["++(intercalate "," options)++"]:"
    ans <- getLine
    if elem ans options then
        return ans
    else do
        putStrLn " -- INVALID INPUT --"
        getChoice options


{- Play a game aganist and AI that tries to win.
ons with their codenames.
         Should not retrieve an empty list.
cchoice : a function gives all the available decisions with their codenames.
eval    : an evaluation function for the game states (should be +1 if 1st player wins, -1 if 2nd player wins)
st      : the initial state.
-}
playAganistAI :: Show s => (s -> [(String,s)]) -> (s -> Float) -> s -> IO ()
playAganistAI cchoice eval st = do
    putStrLn "Difficulty level?"
    level_s <- getChoice (map show [1..8])
    let level = read level_s
    putStrLn "Player 1 or 2?"
    player_s <- getChoice ["1","2"]
    let playerIni = if player_s=="1" then True else False
    gameLoop cchoice eval level st playerIni (not playerIni)

gameLoop :: Show s => (s -> [(String,s)]) -> (s -> Float) -> Int -> s -> Bool -> Bool -> IO ()
gameLoop cchoice eval level st playerTurn aimaxes = do
    -- print board
    print st
    -- get choices
    let chs = cchoice st -- player choices
    --
    if null chs then do
        -- the game has ended
        putStrLn "Game over."
        putStrLn $ "Result " ++ show (eval st)
    else do
        Just st2 <- if playerTurn then do
                putStrLn "Player's turn:"
                move <- getChoice (map fst chs)
                return $ lookup move chs
            else do
                let aieval = if aimaxes then eval else (*(-1)) . eval
                aimove <- ai cchoice aieval level st
                putStrLn $ "AI's move: " ++ aimove
                return $ lookup aimove chs
        gameLoop cchoice eval level st2 (not playerTurn) aimaxes



