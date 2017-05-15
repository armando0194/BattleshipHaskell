module Main where 
	import Board
	import System.IO
	import System.Random

	-- Place the given ships on the given board randomly and return a
    -- new board with all the ships placed.
    -- Input:
	--    ship: list of numbers, each specifying the size 
	--          of a ship to be placed
	--	  board: 2d list representing the board 
	-- Output:
	--    A IO Board containing the the ships placed
	placeShips :: [Int] -> Board -> IO Board
	placeShips [] board = do return (board)
	placeShips (h : t) board = do 
				x <- randomRIO(1,10)
				y <- randomRIO(1,10)
				dir <- randomRIO(True, False)
				if (isShipPlaceable h x y dir board == True) 
					then placeShips t (placeShip h x y dir board) 
					else placeShips (h : t) board where

	-- Reads a coordinate from the user between the 1 and the limit
  	-- Input:
	--    limit: coordinate upper limit
	-- Output:
	--    A IO int containing a coordinate
	getCoordinate :: Int -> IO Int
	getCoordinate limit = do
		line <- getLine
		let parsed = reads line :: [(Int, String)] in
			if length parsed == 0
			then getCoordinate'
			else let (x, _) = head parsed in
				if (x > 0 && x <= limit)
				then return x
				else getCoordinate'
			where
				getCoordinate' = do
					putStrLn "Invalid input!"
					putStrLn "Enter a number between 1- 10 value?"
					getCoordinate limit

	-- Read and return a pair of (x,y) specifying the 1-based column and
  	-- row indices of a board square to hit
  	-- Input:
  	--    board: 2d list representing the board 
	-- Output:
	--    A IO (int, int) tuple that contains the coordinate that will be hit
	getXY :: Board -> IO (Int, Int)
	getXY board = do
		putStr "Enter x coordinate:"
		x <- getCoordinate (length board)
		putStr "Enter y coordinate:"
		y <- getCoordinate (length board)

		if isHit x y board 
		then do
			putStr "Place has been hit \n"
			getXY board
		else return (x,y)

	-- Asks the user for input while it is not game over,
	-- and when all the ships are sunk, the games resets
	play :: (Int -> String) -> Board -> IO()
	play marker board = do
		if isGameOver board 
		then do
			putStr "You Sunk all Ships :D \n"
			board <- (placeShips ([2,2,3,4,5]) (makeBoard 10))
			play marker board
		else do
			putStr (boardToString marker board)
			(x, y) <- getXY board
			play marker (hitBoard x y board)

	-- Entry point for normal game
	main :: IO()
	main = do 
		board <- (placeShips ([2,2,3,4,5]) (makeBoard 10))
		play sqToStr board

	-- Entry point for cheat mode
	mainCheat :: IO()
	mainCheat = do 
		board <- (placeShips ([2,2,3,4,5]) (makeBoard 10))
		play sqToStrCheat board
		
	
