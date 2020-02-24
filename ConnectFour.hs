{-# LANGUAGE ViewPatterns #-}

import AIGame
import qualified Data.Matrix as Mx
import qualified Data.List
import Debug.Trace



-- A ConnectFour has the state of the board and the next player
data ConnectFour = CF (Mx.Matrix Int) Int

instance Show ConnectFour where
    show (CF mx _) = show mx

-- Initial state of the ConnectFour:  (6 high, 7 wide grid or 7x6 Matrix)
cfini :: ConnectFour
cfini = CF (Mx.fromList 6 7 [1..]) (-1)

--Check if there is a winner on any Row.
checkRows:: ConnectFour -> Bool
checkRows (CF (Mx.toLists->(last:[])) k)= Data.List.isInfixOf [k,k,k,k] last
checkRows (CF (Mx.toLists->(r:rs)) k)= do
    if((Data.List.isInfixOf [k,k,k,k] r))
        then True
        else checkRows (CF (Mx.fromLists rs) k)
checkCols:: ConnectFour->Bool
checkCols (CF (Mx.transpose->trans) k) = checkRows (CF trans k)

getDiagonal::ConnectFour->[(Int,Int)]->Int->Bool
getDiagonal (CF mx k) (walk) adds= Data.List.isInfixOf [k,k,k,k] [(Mx.getElem a b mx)|(a,b)<-walk,(a+b)==adds]

checkDiagonal::ConnectFour->[(Int,Int)]->Bool
checkDiagonal cf walk = Data.List.length [x | x<-[5..10], getDiagonal cf walk x] >0

transformar:: ConnectFour->ConnectFour
transformar (CF mx k) = (CF (Mx.fromLists [Data.List.reverse x| x<-Mx.toLists mx]) k)

checkOtherDiagonals::ConnectFour->[(Int,Int)]->Bool
checkOtherDiagonals cf walk = checkDiagonal (transformar cf) walk

check:: ConnectFour-> Float
check cf
    | or [checkRows cf ,checkCols cf ,checkDiagonal cf  walk,checkOtherDiagonals cf  walk ] = (1.0)
    | otherwise = (0.0)
-- Evaluates a given state (-1 =='X', 0 == 'O')
cfeval :: ConnectFour -> Float 
cfeval st@(CF mx (-1)) = do
    if(and [check st==(1.0), check (CF mx 0)==(0.0)])
        then 1.0
        else if(and [check st==(0.0), check (CF mx 0)==(1.0)])
            then (-1.0)
            else 0.0
cfeval st@(CF mx 0) = 
    if(and [check st==(1.0), check (CF mx (-1))==(0.0)])
        then (-1.0)
        else if(and [check st==(0.0), check (CF mx (-1))==(1.0)])
            then (1.0)
            else 0.0
cfeval _ = 0.0

walk::[(Int,Int)]
walk=[(1,1),(2,1),(1,2),(1,3),(2,2),(3,1),(4,1),(3,2),(2,3),(1,4),(1,5),(2,4),(3,3),(4,2),(5,1),(6,1),(5,2),(4,3),(3,4),(2,5),(1,6),(1,7),(2,6),(3,5),(4,4),(5,3),(6,2),(6,3),(5,4),(4,5),(3,6),(2,7),(3,7),(4,6),(5,5),(6,4),(6,5),(5,6),(4,7),(5,7),(6,6),(6,7)]

-- Gives the next possible states from a given choice, with their codes.
cfchoice :: ConnectFour -> [(String,ConnectFour)]
cfchoice st@(CF mx next)
    | ev ==  1.0  = []
    | ev == -1.0  = []
    | otherwise   = moves
    where
        ev = cfeval st
        next2 = if next==(-1) then 0 else (-1)
        emplace p cs = [if c==p then next else c | c <- cs]
        moves = [(show k,CF (dropp 1 k st) next2) | k <- [1..7], elem k mx]

dropp:: Int -> Int -> ConnectFour -> Mx.Matrix Int
dropp 6 i st@(CF mx next) = Mx.setElem next (6,i) mx
dropp j i st@(CF mx next) = do
    let this  = Mx.getElem j i mx
    if(and [this /= 0, this /= (-1), j==6]) 
        then Mx.setElem next (j,i) mx
        else do
            let bottom  = Mx.getElem (j+1) i mx
            if(or [bottom == (-1), bottom == 0])
                then Mx.setElem next (j,i) mx
                else dropp (j+1) i st

main :: IO ()
main = do
    -- play aganist an AI at the given level
    playAganistAI cfchoice cfeval cfini