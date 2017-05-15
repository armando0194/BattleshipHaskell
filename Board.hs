module Board where

	type Board = [[Int]]

	-- Return an empty nxn board, where n is a positive number.
	-- Input:
	--    n: number of columns and rows
	-- Output:
	--    2d list containing 0's
	makeBoard :: Int -> Board
	makeBoard n = [ [ 0 | x <- rows ] | y <- columns ]  where
		rows = [1..n] 
		columns = [1..n] 
		getIndex x y = ( (y-1) * n ) + x

	-- Given a board, it determines if the ship have been sunk.
	-- Input:
	--    board: board that will be analyzed
	-- Output:
	--    True if all the ships placed on the given board have been sunk.
	isGameOver :: Board -> Bool
	isGameOver board = length (filter (\x->x>0) (concat board)) == 0 

	-- Checks if a ship can be placed in the board
	-- Input:
	--    n: size of ship
	--	  x: x coordinate
	--	  y: y coordinate
	--	  dir: True for horizontal and False for vertical
	--    board: 2d list representing the board 
	-- Output:
	--    True if the ship can be placed, false otherwise
	isShipPlaceable :: Int -> Int -> Int -> Bool -> Board -> Bool
	isShipPlaceable 0 _ _ _ _ = True
	isShipPlaceable _ _ _ _ [[]] = False
	isShipPlaceable n x y dir board
		| (x < 1 || y < 1) = False
		| (x > (length board) || y > (length board) || n > (length board) ) = False
		| (dir == True && (x-1+n) > 10) = False
		| (dir == False && (y-1+n) > 10) = False
	isShipPlaceable n x y dir board = if getPlace x (getPlace y board) == 0
									  then 
									      if (dir == True) 
									      then isShipPlaceable (n-1) (x+1) y dir board
										  else isShipPlaceable (n-1) x (y+1) dir board
									  else False

	-- Place a ship of size n at the square (x,y) of the given board
	-- Input:
	--    n: size of ship
	--	  x: x coordinate
	--	  y: y coordinate
	--	  dir: True for horizontal and False for vertical
	--    board: 2d list representing the board 
	-- Output:
	--    a new board with the ship placed
	placeShip :: Int -> Int -> Int -> Bool -> Board -> Board
	placeShip n x y dir board = replaceBoard n n x y dir board where 
		replaceBoard _ 0 _ _ _ board = board
		replaceBoard ship n x y dir board = if (dir == True) 
											then replaceBoard ship (n-1) (x+1) y dir newBoard
											else replaceBoard ship (n-1) x (y+1) dir newBoard where 
												newBoard = replace y board (replace x (getPlace y board) ship)

	-- Checks if the Place at the square (x,y) of the given board was hit
	-- Input:
	--	  x: x coordinate
	--	  y: y coordinate
	--    board: 2d list representing the board  
	-- Output:
	--    true if place was hit, false otherwise											
	isHit :: Int -> Int -> Board -> Bool
	isHit x y board = if getPlace x (getPlace y board) < 0 then True else False 

	-- it the square at the position (x,y) of the given board
	-- Input:
	--	  x: x coordinate
	--	  y: y coordinate
	--    board: 2d list representing the board  
	-- Output:
	--    new board	
	hitBoard :: Int -> Int -> Board -> Board
	hitBoard x y board = 
		replace y board (replace x (getPlace y board) hit ) where
		hit = if getPlace x (getPlace y board) == 0
			  then -1
			  else -2

	-- Gets the element in the nth position int the list
	-- Input:
	--	  n: index of the element  
	--	  l: list
	-- Output:
	--    new board	
	getPlace :: Int -> [a] -> a
	getPlace n l = head (drop (n-1) (take n l))

	-- Changes the nth element in a list
	-- Input:
	--	  n: index of the element  
	--	  l: list
	--	  newValue: new value 
	-- Output:
	--    new list with the new value
	replace :: Int -> [a] -> a -> [a]
	replace n l newValue = take (n-1) l ++ [newValue] ++ drop n l

	-- Takes a board and returns its string representation
	-- Input:
	--	  marker: function that converts a place at the board to a string
	--	  board: 2d list representing the board 
	-- Output:
	--    board's string representation
	boardToString :: (Int -> String) -> Board -> String
	boardToString marker board = header ++ rowsToString 1 (map marker (concat board)) where
		header = " x 1 2 3 4 5 6 7 8 9 0 \ny --------------------\n"
		rowsToString _ [] = ""
		rowsToString row l = rowNum ++ rowContent ++ rowsToString (row+1) (drop (length board) l) where
			rowNum = show(row `mod` (length board)) ++ " |" 
			rowContent = unwords (take (length board) l)  ++ " \n"
						
	-- It is a function that takes a square of a board and returns its
 	-- string representation but mantains the ships hidden
 	-- Input:
	--	  sq: value inside the place 
	-- Output:
	--    square's string representation
	sqToStr :: Int -> String 
	sqToStr sq 
			| (sq >= 0) = "."  
			| (sq == -1) = "X"
			| (sq == -2) = "O"

	-- It is a function that takes a square of a board and returns its
 	-- string representation, but the ships are not hidden
 	-- Input:
	--	  sq: value inside the place 
	-- Output:
	--    square's string representation
	sqToStrCheat :: Int -> String 
	sqToStrCheat sq 
			| (sq == 0) = "."
			| (sq > 0) =  show (sq)  
			| (sq == -1) = "X"
			| (sq == -2) = "O"





