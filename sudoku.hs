
-- SUDOKU
{-
	Nosotros habíamos pensado la práctica para que la función MAIN fuera jugar (linea 360). 
	
	Creamos un entorno lo mas inmersivo posible para el usuario, de manera que lo único que tuviera que hacer es
	pulsar las teclas que el programa le pide a lo largo del juego y no tuviera que pensar en nada más que en el
	sudoku.
	
	Sin embargo, según avanzabamos en la práctica nos dimos cuenta de que la pila de haskell llegaba a su límite 
	con algunas funciones; en algunas porque le exige muchas operaciones y en otras porque le exige imprimir mucho 
	texto.
	
	Si se ejecutan las funciones por separado se ve que funcionan, el problema es que creemos que haskell no es capaz
	de ejecutarlas de la manera que habíamos pensado.
	
	Lo que queremos decir es que se puede jugar igualmente sin la función JUGAR, cargando un sudoku desde un fichero y
	usando las funciones 
	
	- leerTablero (142)				IO ()										para leer un tablero en un fichero
	- showTablero (160)				[[Int]] -> String							para mostrar un tablero de forma bonita
	- readTablero (163)				String -> [[Int]]							a partir de la representación String del tablero, lo convierte en matriz
	- deleteRandomTablero (337) 	Sem -> Int -> Tablero -> Tablero			para poner un número x de casillas al azar en blanco
	- colocarTablero (360)			Int -> Int -> Int -> Tablero -> Tablero		para colocar un elemento en la posicion (i, j) del tablero
	- ayuda (507)					String -> Tablero -> Int -> Int -> IO ()	por si el jugador no sabe que hacer en un momento dado, hace falta ingresar la BASE DE DATOS como entrada string
	- checkMatrix (117)				[[Int]] -> Bool								para checkear que el tablero está bien
	
-}

{-
	ÍNDICE
	línea				nombre
	50					descripción de tipos, data y funciones auxiliares
	117					función que comprueba si un sudoku está bien
	142					show para tableros, IO sudoku..
	187					funciones para generar los distintos objetos aleatorios (numeros, vectores, matrices, tableros...)
	300					donde empieza realmente la implementación del juego SUDOKU. Explicamos los niveles de dificultad.
	374					función MAIN, JUGAR
-}	

import System.Random
import Control.Monad

import Char
import Data.Maybe
import System.Environment
import System.Exit
import Random

type Tablero = [[Int]]
type Sem = Int

data Solucion = Correcto | Incorrecto
	deriving(Show, Read)

-- funcion que comprueba si un elemento esta (True) o no esta (False) en una lista

isIn :: Int -> [Int] -> Bool
isIn x [] = False
isIn x (y:ys) 
	| x == y = True
	| otherwise = isIn x ys

--Funcion que elimina un elemento de una lista, se supone que no hay repetidos
deleteElementList :: Int -> [Int] -> [Int]
deleteElementList x [] = []
deleteElementList x (y:ys)
	| x == y = ys
	| otherwise = y:(deleteElementList x ys)
 
--Funcion que elimina una lista de elementos de una lista, se supone que no hay repetidos
deleteInList :: [Int] -> [Int] -> [Int]
deleteInList xs ys = foldl (flip(deleteElementList)) ys xs

--Elimina un elemento en la posicion (n, m) de un tablero (matriz de enteros) poniendo un 0 en su lugar, (0,0) es primera fila, primera columna.
deleteElementTablero :: Int -> Int -> Tablero -> Tablero
deleteElementTablero n m (xs:tablero)
	| n > 0 = xs:(deleteElementTablero (n-1) m tablero)
	| n == 0 = (deleteElementVector m xs):tablero
	
--Funcion auxiliar de deleteElementTablero que elimina un elemento en la posicion m de un vector poniendo un 0 en su lugar
deleteElementVector :: Int -> [Int] -> [Int]
deleteElementVector m (x:vector)
	| m > 0 = x:(deleteElementVector (m-1) vector)
	| m == 0 = 0:vector

--Funcion que calcula la interseccion entre dos listas

intersectionList :: [Int] -> [Int] -> [Int]
intersectionList xs ys = [z | z<-xs, isIn z ys]

-- funcion que comprueba si hay elementos repetidos (True) o no (False) en una fila
checkRow :: [Int] -> Bool
checkRow [] = False
checkRow (x:xs)
	| isIn x xs = True
	| otherwise = checkRow xs

-- funcion que comprueba si hay elementos repetidos (True) o no (False) en una columna

checkColumn :: [Int] -> Bool
checkColumn xs = checkRow xs

{- funcion que comprueba si hay elementos repetidos en la fila y columna de una posicion dada en una matriz
devuelve False si no hay ninguno repetido y True si hay alguno repetido
                          
						  checkMatrixPosition (numero_columna) (numero_fila) matrix
-}
checkMatrixPosition :: Int -> Int -> [[Int]] -> Bool
checkMatrixPosition x y zss = (checkRow row) || (checkColumn column)
	where 
		column = [t!!(y-1) | t <- zss]
		row = zss!!(x-1)
	
-- Funcion que comprueba que en cada posicion toda fila y columna tiene (True) o no (False) elementos repetidos
	
checkMatrix :: [[Int]] -> Bool
checkMatrix zss = foldl (||) False ([checkMatrixPosition q p zss | p <- [1..n], q <- [1..m]])
	where 
		n = (length (zss!!0)) 
		m = (length zss) 
		
	

--Ahora ya estamos listos para hacer una funcion que compruebe un tablero de SUDOKU 


{- 
Los tableros tambien puede leerse a partir de un fichero escrito de la forma

						1 2 3 4 5 6 7 8 9\n
						2 4 5 3 7 8 9 6 1\n
						.     . . .      \n 
						.
						.

que representa un tablero de 9x9.

Sabiendo esto creamos una funcion que lea un tablero de un fichero dado. 
-}

leerTablero :: IO ()
leerTablero = do
	putStr "Digame el nombre del fichero: "
	name <- getLine
	content <- readFile name
	let tablero =  readTablero content
	putStr (showTablero tablero)

-- Fución que imprime una matriz
{-
	Estandarizamos la impresión de matrices en esta práctica de esta manera:
								1 2 3 4 5 6 
								4 5 6 3 1 2
								.  ...
								.
								.
	Tanto al mostrar un tablero con showTablero como al meterlo a un fichero con writeTableroFile se mostrará de esta manera.
-}
showTablero :: [[Int]] -> String
showTablero tablero = unlines (map unwords (map (map show) tablero))

readTablero :: String -> [[Int]]
readTablero tablero = map (map read) (map words (lines tablero)) :: [[Int]]
-- Función para escribir un tablero en un fichero

writeTableroFile :: Tablero -> IO ()
writeTableroFile tableroSudoku = do
	putStr "Indique el fichero donde quiere guardar el tablero: "
	name <- getLine
	let 
		content = showTablero tableroSudoku
	writeFile name content
	putStr ("Tablero guardado en " ++ name)



{-Para crear los tableros, defino una funcion que genere numeros aleatorios del 1 al x,
siendo x numero menor o igual que 9.
 
El unico inconveniente es que necesitamos una semilla para generar tableros distintos por
ello lo solucionaremos pidiendo al usuario un numero entero cuando quiera empezar el juego.

Con cada numero entero se generara un tablero distinto
-}

randomNumber0x :: Sem -> Float -> Int
randomNumber0x g x = floor(10*z)
	where 
		(z, r) = randomR (0.0001, u) (mkStdGen g) :: (Float, StdGen)
		u = ((x+1)/10) - 0.0001


--Ahora el objetivo es crear la funcion que haga un tablero aleatorio

{- Primero creo una funcion que modifique un vector dado aleatoriamente, con el fin de 
alternar el orden del vector 
						[1,2,3,4,5,6,7,8,9]
y que dependiendo de la semilla tome valores distintos. 
La entrada de la funcion randomizeVector es una semilla (la digitada por el usuario) y
un vector que vamos modificar aleatoriamente.

Para simular el comportamiento aleatorio, para cada componente del vector tomamos como
valor de la semilla la componente anterior.
-}

randomizeVector :: Sem -> [Int] -> [Int]
randomizeVector sem [] = []
randomizeVector sem vector = r:(randomizeVector r newVector) 
		where 
			r = vector!!n --Sera la semilla para la siguiente componente
			n = randomNumber0x sem lengthVector
			newVector = deleteElementList r vector
			lengthVector = fromIntegral ((length vector) - 1) :: Float
	
--Toma un elemento al azar de una lista dada (con una semilla)	
randomItemInList :: Int -> [Int] -> Int
randomItemInList sem vector = vector!!n
	where 
		n = randomNumber0x sem lengthVector
		lengthVector = fromIntegral ((length vector) - 1) :: Float

--Creamos el tablero aleatoriamente con createTablero, el resto son funciones auxiliares
{-
	Para crear el tablero creamos una matrices 9x9 válida para el sudoku.
	Para ello lo que hacemos es controlar los elementos que ya hemos colocado, para ello partimos de una matriz:
										1 2 3 4 5 6 7 8 9
										1 2 3 4 5 6 7 8 9
										.
										.
										.
										1 2 3 4 5 6 7 8 9
	Que nos dice los elementos que hay disponibles para colocar en la fila i.
	
	Como comentario decir que la función parece que funciona, pero creemos que no termina de generar la matriz porque la
	pila de Haskell llega al tope.
-}
										
createTablero :: Sem -> [[Int]]
createTablero sem = create sem [[1..9] | n<-[1..9]]

create :: Int -> [[Int]] -> [[Int]]
create sem [] = []
create sem matrix = filai:(create (sem + 5323) [deleteElementList (filai!!n) (matrix!!n) | n<-[0..8]])
	where
		filai = chooseAux sem matrix
				
chooseAux :: Sem -> Tablero -> [Int]
chooseAux sem matrix 
	| vector1 == [] = chooseAux (sem + 1) matrix
	| vector2 == [] = chooseAux (sem + 3) matrix
	| vector3 == [] = chooseAux (sem + 2) matrix
	| vector4 == [] = chooseAux (sem + 3) matrix
	| vector5 == [] = chooseAux (sem + 1) matrix
	| vector6 == [] = chooseAux (sem + 2) matrix
	| vector7 == [] = chooseAux (sem + 3) matrix
	| vector8 == [] = chooseAux (sem + 1) matrix
	| otherwise = [v1, v2, v3, v4, v5, v6, v7, v8, v9]
	where
		vector = [1..9]	
		v1 = randomItemInList sem (intersectionList (matrix!!0) vector)
		vector1 = (intersectionList (matrix!!1) (deleteInList [v1] vector))
		v2 
			| vector1 /= [] = randomItemInList sem vector1
			|otherwise = 1
		vector2 = (intersectionList (matrix!!2) (deleteInList [v1, v2] vector))
		v3 
			| vector2 /= [] = randomItemInList sem vector2
			|otherwise = 1
		vector3 = (intersectionList (matrix!!3) (deleteInList [v1, v2, v3] vector))
		v4 
			| vector3 /= [] = randomItemInList sem vector3
			|otherwise = 1
		vector4 = (intersectionList (matrix!!4) (deleteInList [v1, v2, v3, v4] vector))
		v5 
			| vector4 /= [] = randomItemInList sem vector4
			|otherwise = 1
		vector5 = (intersectionList (matrix!!5) (deleteInList [v1, v2, v3, v4, v5] vector))
		v6 
			| vector5 /= [] = randomItemInList sem vector5
			|otherwise = 1
		vector6 = (intersectionList (matrix!!6) (deleteInList [v1, v2, v3, v4, v5, v6] vector))
		v7 
			| vector6 /= [] = randomItemInList sem vector6
			|otherwise = 1
		vector7 = (intersectionList (matrix!!7) (deleteInList [v1, v2, v3, v4, v5, v6, v7] vector))
		v8 
			| vector7 /= [] = randomItemInList sem vector7
			|otherwise = 1
		vector8 = (intersectionList (matrix!!8) (deleteInList [v1, v2, v3, v4, v5, v6, v7, v8] vector))
		v9
			| vector8 /= [] = randomItemInList sem vector8
			|otherwise = 1
		
		
		



-- A partir de aqui es donde realmente empieza la impelmentacion del juego:

{- Habra 4 niveles de dificultad: Facil, Medio, Dificil y Muy Dificil
La dificultad va en funcion del numero de casillas en blanco en el tablero

	(1) Facil : 16-22 casillas en blanco
	(2) Medio : 28-34 casillas en blanco
	(3) Dificil : 42-47 casillas en blanco
	(4) Muy dificil : 58-63 casillas en blanco
	
En cada uno de los niveles el numero de casillas en blanco sera un numero aleatorio.
-}

--generateSudoku, dado un entero que representa el nivel de dificultad genera un tablero con un numero x de casillas en blanco
{-

Para generar un tablero de sudoku partimos de un tablero correcto (resuelto) generado por la funcion 
createTablero con una semilla dada y quitamos elementos del tablero.

-}

generateSudoku :: Sem -> Int -> Tablero
generateSudoku sem x = deleteRandomTablero sem x tablero
	where tablero = createTablero sem 

--Elimina x elementos al azar del tablero, cuando elimina un elemento pone un 0 en su lugar
{-

Ejemplo:
						1 0 3 4 5 0 7 8 9
						0 8 7 6 0 5 3 2 1
						.   ...
						.
						.

-}

deleteRandomTablero :: Sem -> Int -> Tablero -> Tablero
deleteRandomTablero sem 1 tablero 
	| (tablero!!n)!!m == 0 = deleteRandomTablero (sem+1) 1 tablero
	| otherwise = newTablero
	where 
		n = randomNumber0x sem lengthColumn --Elige una fila al azar 
		m = randomNumber0x (sem+1) lengthRow --Elige un columna al azar
		newTablero = deleteElementTablero n m tablero
		lengthColumn = fromIntegral (length tablero) - 1 :: Float
		lengthRow = fromIntegral (length (tablero!!0)) - 1 :: Float
deleteRandomTablero sem x tablero 
	| (tablero!!n)!!m == 0 = deleteRandomTablero (sem+1) x tablero
	| otherwise = deleteRandomTablero (sem+1) (x-1) newTablero
	where 
		n = randomNumber0x sem lengthColumn --Elige una fila al azar 
		m = randomNumber0x (sem+1) lengthRow --Elige un columna al azar
		newTablero = deleteElementTablero n m tablero
		lengthColumn = fromIntegral (length tablero) - 1 :: Float
		lengthRow = fromIntegral (length (tablero!!0)) - 1 :: Float


-- Introducir un numero en una casilla (n, m) con un cero, (1,1) es la primera fila, primera columna. Si no hay un cero la deja como estaba

colocarTablero :: Int -> Int -> Int -> Tablero -> Tablero
colocarTablero n m p (xs:tablero)
	| n>1 = xs:(colocarTablero (n-1) m p tablero)
	| n == 1 = (colocarVector m p xs):tablero
	|otherwise = xs:tablero

colocarVector :: Int -> Int -> [Int] -> [Int]
colocarVector m p (x:vector) 
	| m > 1 = x:(colocarVector (m-1) p vector)
	| m == 1 && x==0 = p:vector
	| otherwise = x:vector

--Por fin, con todas las funciones necesarias definidas, JUGAMOS

jugar :: IO()
jugar = do
	putStr "Introduce un numero entero cualquiera para empezar el juego: " -- Hacemos lo que hemos mencionado al principio, el usuario introduce la semilla
	semilla <- getLine 
	let 
		sem = read(semilla) :: Sem
	putStr "¡Bienvenido a SUDOKU! Para empezar seleccione nivel de dificultad: \n(1) Facil \n(2) Medio \n(3) Dificil \n(4) Muy dificil \n"
	dificultad_temp <- getLine
	putStr "Indique si quiere jugar con un tablero generado aleatoriamente por el programa (1) o uno cargado desde la base de datos (2): "
	modoGeneracionTablero <- getLine
	let
		dificultad = read (dificultad_temp) :: Int
		x 
			| dificultad == 1 = 16 + randomNumber0x sem 7
			| dificultad == 2 = 28 + randomNumber0x sem 6
			| dificultad == 3 = 42 + randomNumber0x sem 5
			| dificultad == 4 = 58 + randomNumber0x sem 5
	if modoGeneracionTablero == "1"
		then do
			jugarGeneradorAleatorio sem x
		else do
			jugarFichero sem x


jugarFichero :: Sem -> Int -> IO ()
jugarFichero sem x = do
	putStr "Escriba el nombre de la base de datos: "
	nameFile <- getLine
	content <- readFile nameFile
	let 
		tableroResuelto = map (map read) (map words (lines content)) :: [[Int]]
		tableroSudoku = deleteRandomTablero sem x tableroResuelto
	putStr "\n\n"
	putStr (showTablero tableroSudoku)
	putStr "!Que comience el juego!"
	putStr "\n\nLas condiciones son las siguientes, debe solucionar el puzzle SUDOKU sin limite de tiempo. Solo puede rellenar las casillas marcadas con un 0, una vez rellenadas ¡¡NO PUEDE CAMBIARLAS!! ¿¿Esta claro??"
	putStr "\n\nEn cada turno se le preguntara si quiere seguir rellenando a lo que contestara (Y) o (N)"
	putStr "\n\n¿Desea rellenar alguna casilla? (Y/N): "
	cond <- getLine
	if cond == "Y" || cond == "y"
		then do
			jugarRecursivo tableroSudoku tableroResuelto
		else do
			putStr "Comprobando solucion..."
			let solucion = checkMatrix tableroSudoku
			if solucion == False
				then do
					print Correcto
					putStr "¡Solucion correcta!, toma una piruleta"
				else do
					print Incorrecto
					putStr "¡Fatal! ¡Horroroso! Tienes que practicar mas"
					putStr "Para volver a jugar escribe JUGAR"
	


jugarGeneradorAleatorio :: Sem -> Int -> IO ()
jugarGeneradorAleatorio sem x = do
	let
		tableroResuelto = createTablero sem 
		tableroSudoku = deleteRandomTablero sem x tableroResuelto -- genera un tablero de forma aleatoria con x casillas con 0.
	putStr "\n\n"
	putStr (showTablero tableroSudoku)
	putStr "!Que comience el juego!"
	putStr "\n\nLas condiciones son las siguientes, debe solucionar el puzzle SUDOKU sin limite de tiempo. Solo puede rellenar las casillas marcadas con un 0, una vez rellenadas ¡¡NO PUEDE CAMBIARLAS!! ¿¿Esta claro??"
	putStr "\n\nEn cada turno se le preguntara si quiere seguir rellenando a lo que contestara (Y) o (N)"
	putStr "\n\n¿Desea rellenar alguna casilla? (Y/N): "
	cond <- getLine
	if cond == "Y" || cond == "y"
		then do
			jugarRecursivo tableroSudoku tableroResuelto
		else do
			putStr "Comprobando solucion..."
			let solucion = checkMatrix tableroSudoku
			if solucion == False
				then do
					print Correcto
					putStr "¡Solucion correcta!, toma una piruleta"
				else do
					print Incorrecto
					putStr "¡Fatal! ¡Horroroso! Tienes que practicar mas"
					putStr "Para volver a jugar escribe JUGAR"

jugarRecursivo :: Tablero -> Tablero -> IO ()
jugarRecursivo tableroSudoku tableroResuelto = do
	putStr "\n\n¿Necesitas ayuda? (Y/N)?"
	condAyuda <- getLine
	putStr "\n\nIndique la fila que desea rellenar: "
	row <- getLine
	putStr "Indique la columna que desea rellenar: "
	column <- getLine
	let
		n = read(row) :: Int
		m = read(column) :: Int
		stringAyuda 
			| condAyuda == "Y" || condAyuda == "y" = ayudaShow tableroResuelto tableroSudoku n m
			| otherwise = ""
	putStr stringAyuda
	putStr "\nIndique el valor que quiere ingresar en la posicion (entre el 1 y el 9): "
	value <- getLine
	let
		p = read(value) :: Int
		tableroSudokuCopy = tableroSudoku
		tableroSudoku = colocarTablero n m p tableroSudokuCopy
	putStr "\n\nSu tablero actual: "
	putStr (showTablero tableroSudoku)
			
	putStr "\n\n\n¿Desea rellenar alguna casilla? (Y/N): "
	cond <- getLine	
	if cond == "Y" || cond == "y"
		then do
			jugarRecursivo tableroSudoku tableroResuelto
		else do
			putStr "Comprobando solucion..."
			let solucion = checkMatrix tableroSudoku
			if solucion == False
				then do
					print Correcto
					putStr "¡Solucion correcta!, toma una piruleta"
				else do
					print Incorrecto
					putStr "¡Fatal! ¡Horroroso! Tienes que practicar mas"
					putStr "Para volver a jugar escribe JUGAR"	
	
	
-- Implementamos una función ayuda. Si el usuario lo requiere le sugiere un número para poner en la posición indicada por el usuario
ayudaShow :: Tablero -> Tablero -> Int -> Int -> String
ayudaShow tableroResuelto tableroSudoku row column = "Sugiero que escribas " ++ show(num) ++ " en la posicion (" ++ show(row) ++ ", " ++ show(column) ++ ")."
	where 
		num = (tableroResuelto!!(row - 1))!!(column - 1)

-- La función ayuda requiere 3 argumentos, el tablero actual [[Int]] y el numero de fila y columna. Además hay que conocer el nombre
-- del fichero de la base de datos.
ayuda :: Tablero -> Int -> Int -> IO ()
ayuda tableroSudoku row column = do
	putStr "Dígame el nombre de la base de datos: "
	baseDatos <- getLine
	content <- readFile baseDatos
	let 
		tableroResuelto =  readTablero content
		num = (tableroResuelto!!(row - 1))!!(column - 1)
	putStr ("\n\nSugiero que escribas " ++ show(num) ++ " en la posicion (" ++ show(row) ++ ", " ++ show(column) ++ ").")
				
{-
	Obs.
	La idea de la función ayuda es que como para crear el SUDOKU partimos de un tablero resuelto, simplemete lo que hacemos
	es recurrir al tablero inicial que le llamamos TABLERORESUELTO y miramos el numero que ocupaba la posición pedida.
-}