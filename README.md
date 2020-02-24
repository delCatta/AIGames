  # AIGames
  
  Some games that you can play against AI that uses Min Max Optimization Algorithm. 
  
  Coded as homework requested by my Programing Lang Teacher.
  
  ## What did I learn? 
  - Haskell Programming.
  - Data.Matrix functions.
  - New board games like Chomp and Hex.
  - Evaluation/decision functions for the algorithm.
  - Makefile implementation.
  

# Dependencias

  

  

Para compilar `AIGame.hs` se requiere el instalar el siguiente paquete de `cabal`:

  

```bash

  

cabal install random-shuffle

  

```

  

  

Para realizar la tarea también se recomienda:

  

```bash

  

cabal install matrix

  

```

  

  

# Juegos

  

  

Los juegos elegidos para esta tarea son TicTacToe, [ConnectFour](https://en.wikipedia.org/wiki/Connect_Four), [Chomp](https://en.wikipedia.org/wiki/Chomp) y [Hex (BoardGame)](https://en.wikipedia.org/wiki/Hex_(board_game)).

  

  

  

# Targets

  

  

Por defecto, `make` compila todos los juegos en el directorio `Compiled`.

  

  

Para ejecutar un Juego se debe correr con el comando `./Compiled/<juego en minúsculas>`. Ex. (`TicTacToe.hs`) `./Compiled/tictactoe`.

  

  

El comando `make <juego en minúsculas>` , crea el directorio `Compiled` y compila `<Juego en Mayúsculas>.hs`.

  

# Consideraciones

  

  
  
  

En los juegos [ConnectFour](https://en.wikipedia.org/wiki/Connect_Four), [Chomp](https://en.wikipedia.org/wiki/Chomp) y [Hex (BoardGame)](https://en.wikipedia.org/wiki/Hex_(board_game)), al tener tableros representados por matrices, la dificultad de la AI se demorará más al tener mayor dimensiónes. Es por esto que se recomienda mantener una proporción entre dimensión y dificultad que sea computable en un tiempo aceptable.

  
  

Para poder mantener la estructura de `Data.Matrix Int` es que se ocupa `-1` para el input del primer jugador, y `0` para el del segundo para los juegos que ocupan esta estructura.

  

  

Restringir AIGame a menores dificultades, restringe el gameplay de TicTacToe.

  

Según Wikipedia, Hex se juega en un tablero de 11 x 11. Por los mismos motivos anteriores, queda a libre disposición del usuario el definir las dimensiones del tablero. Se mantiene la esencia de Hex al tener una matriz cuadrada que no beneficia a ningún jugador.

  

En ConnectFour, los inputs son las columnas y por efecto de 'gravedad', la ficha caerá hasta la última posición disponible para la ficha.

  

  

En Hex, según el link proporcionado:

  

  

>Rectangular grids and paper and pencil

  

>The game may be played on a rectangular grid like a chess, checker or go board, by considering that spaces (intersections in the case of go) are connected in one diagonal direction but not the other. The game may be played with paper and pencil on a rectangular array of dots or graph paper in the same way by using two different colored pencils.

  

  

Es por esto que dada la posición `X`, sus posiciones adyacentes son para efectos de este juego:

<pre>

				  * *

				* X *

				* *

</pre>
