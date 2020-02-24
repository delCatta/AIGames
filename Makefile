.DEFAULT_GOAL := default
makedir:
	mkdir Compiled
tictactoe: makedir
	ghc AIGame.hs TicTacToe.hs -o Compiled/tictactoe -package random-shuffle  -package matrix
connectfour: makedir
	ghc AIGame.hs ConnectFour.hs -o Compiled/connectfour -package random-shuffle  -package matrix
chomp: makedir
	ghc AIGame.hs Chomp.hs -o Compiled/chomp -package random-shuffle  -package matrix
hex: makedir
	ghc AIGame.hs Hex.hs -o Compiled/hex -package random-shuffle  -package matrix
default: hex chomp connectfour tictactoe 

clean:
	rm -f *.hi
	rm -f *.o
	rm -Rf Compiled

# Colocar aquí sus targets, uno por cada juego.