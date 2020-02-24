import Text.Printf (printf)

import AIGame


-- A TicTacToe has the state of the board and the next player
data TicTacToe = TTT [Char] Char

instance Show TicTacToe where
    show (TTT [p1,p2,p3,p4,p5,p6,p7,p8,p9] _) =
        printf "\n %c %c %c\n %c %c %c\n %c %c %c\n" p1 p2 p3 p4 p5 p6 p7 p8 p9

-- Initial state of the TicTacToe
tttini :: TicTacToe
tttini = TTT ['0','1','2','3','4','5','6','7','8'] 'X'

-- Evaluates a given state
ttteval :: TicTacToe -> Float
ttteval (TTT ['X','X','X', _ , _ , _ , _ , _ , _ ] _) = 1.0
ttteval (TTT [ _ , _ , _ ,'X','X','X', _ , _ , _ ] _) = 1.0
ttteval (TTT [ _ , _ , _ , _ , _ , _ ,'X','X','X'] _) = 1.0
ttteval (TTT ['X', _ , _ ,'X', _ , _ ,'X', _ , _ ] _) = 1.0
ttteval (TTT [ _ ,'X', _ , _ ,'X', _ , _ ,'X', _ ] _) = 1.0
ttteval (TTT [ _ , _ ,'X', _ , _ ,'X', _ , _ ,'X'] _) = 1.0
ttteval (TTT ['X', _ , _ , _ ,'X', _ , _ , _ ,'X'] _) = 1.0
ttteval (TTT [ _ , _ ,'X', _ ,'X', _ ,'X', _ , _ ] _) = 1.0
ttteval (TTT ['O','O','O', _ , _ , _ , _ , _ , _ ] _) = -1.0
ttteval (TTT [ _ , _ , _ ,'O','O','O', _ , _ , _ ] _) = -1.0
ttteval (TTT [ _ , _ , _ , _ , _ , _ ,'O','O','O'] _) = -1.0
ttteval (TTT ['O', _ , _ ,'O', _ , _ ,'O', _ , _ ] _) = -1.0
ttteval (TTT [ _ ,'O', _ , _ ,'O', _ , _ ,'O', _ ] _) = -1.0
ttteval (TTT [ _ , _ ,'O', _ , _ ,'O', _ , _ ,'O'] _) = -1.0
ttteval (TTT ['O', _ , _ , _ ,'O', _ , _ , _ ,'O'] _) = -1.0
ttteval (TTT [ _ , _ ,'O', _ ,'O', _ ,'O', _ , _ ] _) = -1.0
ttteval _ = 0.0

-- Gives the next possible states from a given choice, with their codes.
tttchoice :: TicTacToe -> [(String,TicTacToe)]
tttchoice st@(TTT cells next)
    | ev ==  1.0  = []
    | ev == -1.0  = []
    | otherwise   = moves
    where
        ev = ttteval st
        next2 = if next=='X' then 'O' else 'X'
        emplace p cs = [if c==p then next else c | c <- cs]
        moves = [([k],TTT (emplace k cells) next2) | k <- ['0'..'8'], elem k cells]

main :: IO ()
main = do
    -- play aganist an AI at the given level
    -- print $ tttchoice tttini
    playAganistAI tttchoice ttteval tttini