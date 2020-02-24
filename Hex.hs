{-# LANGUAGE ViewPatterns #-}

import AIGame
import qualified Data.Matrix as Mx
import qualified Data.List as Dl
import Debug.Trace



-- A HexBoard has the state of the board and the next player
data HexBoard = HB (Mx.Matrix Int) Int

instance Show HexBoard where
    show (HB mx _) = show mx

-- Initial state of the HexBoard:  (6 high, 7 wide grid or 7x6 Matrix)
hbini :: HexBoard
hbini = HB (Mx.fromList 4 4 [1..]) (-1)

findPath:: [(Int,Int)] -> [(Int,Int)] -> HexBoard-> [(Int,Int)]->Bool
findPath [] goals hx@(HB mx next) v =False
findPath adjPos goals hx@(HB mx next) visited= or [if elem x goals then True else (findPath (validAdj x hx) goals hx (visited++[x]))|x<-adjPos,not $ elem x visited]

inAll:: Int -> Mx.Matrix Int->Bool
inAll next mx
    | next == (-1) = allContains next mx
    | otherwise = allContains next (Mx.transpose mx)
    where 
        allContains n m = and [elem next list | list<- (Mx.toLists m)]

isPath:: HexBoard-> Bool
isPath hb@(HB mx next)
    | inAll next mx = or [findPath (validAdj x hb)  end hb [] |x<-positions]
    | otherwise = False
    where
        first = next== (-1)
        cols =Mx.ncols mx
        rows =Mx.nrows mx
        total=rows * cols
        start = if first then head (Mx.toLists mx) else head (Mx.toLists (Mx.transpose mx))
        end = if first then [getMatrixPos x | x<-([(total - cols+1)..total])] else [getMatrixPos x | x<-([cols,(cols*2)..total])]
        indices = (Dl.elemIndices next start)
        positions= if first then [getMatrixPos (x+1)|x<-indices] else [getMatrixPos ((11*x)+1)|x<-indices]
        -- TODO: CHECK THIS !
        

validAdj:: (Int,Int)->HexBoard->[(Int,Int)]
validAdj pos hx@(HB mx next) = [x| x<- adjWalk ,inRange x , Mx.getElem (fst x) (snd x) mx==next] 
    where
        row = fst pos
        col = snd pos
        inRange x= and [0< fst x, fst x <=(Mx.nrows mx) , 0<snd x, snd x <=(Mx.ncols mx) ]
        adjWalk=[(row+1,col),(row+1,col-1),(row,col+1),(row-1,col+1),(row-1,col),(row,col-1)]

getMatrixPos:: Int->(Int,Int)
getMatrixPos x=(row x,col x)
    where
        rows = 4
        cols = 4
        row x =(div (x-1) cols)+1
        col x =if (mod x cols)==0 then cols else (mod x cols)


hbeval:: HexBoard-> Float
hbeval hb@(HB mx next)
    | isPath (HB mx next) = result
    | otherwise = (0.0)
    where
        next2 = if next==(-1) then 0 else (-1)
        result = if next == (-1) then 1 else (-1)

-- walk::[(Int,Int)]
-- walk=[(1,1),(2,1),(1,2),(1,3),(2,2),(3,1),(4,1),(3,2),(2,3),(1,4),(1,5),(2,4),(3,3),(4,2),(5,1),(6,1),(5,2),(4,3),(3,4),(2,5),(1,6),(1,7),(2,6),(3,5),(4,4),(5,3),(6,2),(7,1),(8,1),(7,2),(6,3),(5,4),(4,5),(3,6),(2,7),(1,8),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,1),(9,2),(8,3),(7,4),(6,5),(5,6),(4,7),(3,8),(2,9),(1,10),(1,11),(2,10),(3,9),(4,8),(5,7),(6,6),(7,5),(8,4),(9,3),(10,2),(11,1),(11,2),(10,3),(9,4),(8,5),(7,6),(6,7),(5,8),(4,9),(3,10),(2,11),(3,11),(4,10),(5,9),(6,8),(7,7),(8,6),(9,5),(10,4),(11,3),(11,4),(10,5),(9,6),(8,7),(7,8),(6,9),(5,10),(4,11),(5,11),(6,10),(7,9),(8,8),(9,7),(10,6),(11,5),(11,6),(10,7),(9,8),(8,9),(7,10),(6,11),(7,11),(8,10),(9,9),(10,8),(11,7),(11,8),(10,9),(9,10),(8,11),(9,11),(10,10),(11,9),(11,10),(10,11),(11,11)]

-- Gives the next possible states from a given choice, with their codes.
hbchoice :: HexBoard -> [(String,HexBoard)]
hbchoice st@(HB mx next)
    | ev ==  1.0  = []
    | ev == -1.0  = []
    | otherwise   = moves
    where
        ev = hbeval st
        next2 = if next==(-1) then 0 else (-1)
        rows =Mx.nrows mx
        cols=Mx.ncols mx
        pieces= (rows)*(cols)
        emplace p cs = [if c==p then next else c | c <- cs]
        moves = [(show k,HB (Mx.fromList rows cols (emplace k (Mx.toList mx))) next2) | k <- [1..pieces], elem k mx]

main :: IO ()
main = do
    -- play aganist an AI at the given level
    putStrLn "--- Reminder ---"
    putStrLn "Player 1's (-1) goal: Connect TOP-BOTTOM."
    putStrLn "Player 2's  (0) goal: Connect LEFT-RIGHT."
    putStrLn "Only one Diagonal Direction Checked (45 Degrees East to North)."
    putStrLn "Then Adjacent to X are (*):"
    putStrLn "  * *"
    putStrLn "* X *"
    putStrLn "* *  "

    putStrLn "--- End of Reminder --- \n\n"
    -- let m = Mx.fromLists [[0,(-1),3 ,4   ],
    --                       [5,(-1)   ,0 ,8],
    --                       [9,0   ,0,(-1)],
    --                       [13,(-1)  ,15,0]]
    -- print $ hbeval (HB m (-1))
    playAganistAI hbchoice hbeval hbini
    return ()