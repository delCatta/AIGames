{-# LANGUAGE ViewPatterns #-}

import AIGame
import qualified Data.Matrix as Mx
import qualified Data.List
import Debug.Trace

import AIGame


-- A Board has the state of the board and the next player
data Board = CHB (Mx.Matrix Int) Int

instance Show Board where
    show (CHB mx _) = show mx

-- Initial state of the Board
chini :: Int -> Int-> Board
chini rows cols = CHB (Mx.fromList rows cols [1..]) (1)

-- Evaluates a given state
cheval :: Board -> Float
cheval (CHB mx next) = do
    if(Mx.toList mx == (replicate pieces 0))
        then fromIntegral $ next
        else 0.0
    where 
        pieces = (Mx.nrows mx)*(Mx.ncols mx)

-- Gives the next possible states from a given choice, with their codes.
chchoice :: Board -> [(String,Board)]
chchoice st@(CHB cells next)
    | ev ==  1.0  = []
    | ev == -1.0  = []
    | otherwise   = moves
    where
        ev = cheval st
        rows =Mx.nrows cells
        cols=Mx.ncols cells
        pieces= (rows)*(cols)
        emplace p cs = [if c==p then 0 else c | c <- cs]
        moves = [(show k, (eat k st) ) | k <- [1..pieces], elem k (Mx.toList cells)]

eat:: Int -> Board -> Board
eat k bd@(CHB mx next) = (CHB eated next)
    where
        rows=Mx.nrows mx
        cols=Mx.ncols mx
        next2 =  (-1) *next
        row x =  (div (x-1) cols)+1
        col x = if (mod x cols)==0 then cols else (mod x cols)
        eated = Mx.fromList rows cols ([if (and [(row x)>=(row k) , (col x)>=(col k)]) then 0 else x| x<-(Mx.toList mx)])


getValidNumber :: String -> IO String
getValidNumber message = do
    putStrLn $ message
    ans <- getLine
    if elem ans [show x|x<-[1..]] then
        return ans
        else do
            putStrLn " -- INVALID INPUT --"
            getValidNumber message

main :: IO ()
main = do

    -- play aganist an AI at the given level

    putStrLn "Insert Board size:"
    getRows<- getValidNumber "Rows:"
    getCols<- getValidNumber "Cols:"
    let rows=(read getRows :: Int)
    let cols=(read getCols :: Int)
    playAganistAI chchoice cheval (chini rows cols)
    return ()