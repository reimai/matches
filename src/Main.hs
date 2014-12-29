module Main where

import Control.Monad 
import System.Random
import Control.Exception

main = do
	putStrLn "Choose the initial number of matches: " 
	n <- readPositive   
	humanFirst <- randomIO :: IO Bool
	playIO humanFirst $ return n

--get the human input of any type of Read, keep asking until the value would pass the check
humanInput :: Read a => (a -> Bool) -> String -> IO a 
humanInput check message = do
			n <- handle (ignore retry) readLn
			if' (check n) (return n) $ retry
			where 
				retry = putStrLn message >> humanInput check message
				
ignore :: a -> IOException -> a
ignore f _ = f 

readPositive :: IO Int
readPositive = humanInput (\n -> n > 0) "Positive, please" 

maxTurn = 3

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
 
--returns a string which represents n matches
matches :: Int -> String  
matches n = take n $ repeat '|'

--how many matches should computer take based on the number of matches left
computerTurn :: Int -> IO (Int)
computerTurn t	| t <= 0 = return 0
		| otherwise = 	let turn = if' (magic t == 0) 1 $ magic t
				in putStrLn (matches t) >> putStrLn ("Computer: \n" ++ (show turn)) >> when ((t-turn) <= 0) (putStrLn "You Win!") >> return (t-turn)

--always try to left n*(maxTurn+1) matches
magic :: Int -> Int
magic t	| t == 1 = 1
	| t <= maxTurn = t - 1
	| otherwise = t `rem` (maxTurn + 1)

humanTurn :: Int -> IO (Int)
humanTurn t	| t <= 0  = return 0
		| otherwise = do
			putStrLn $ matches t
			putStrLn "Your turn: "
			n <- getHumanTurn
			let res = t - n
			when (res <= 0) (putStrLn "You Lose!")  
			return res

getHumanTurn :: IO Int
getHumanTurn = humanInput (\n -> n > 0 && n <= maxTurn) $ "Between 0 and " ++ show(maxTurn) ++ ", please" 

playIO :: Bool -> IO (Int) -> IO (Int)
playIO humanFirst tio = tio >>= play humanFirst 

play :: Bool -> (Int) -> IO (Int)
play humanFirst t	| t <= 0 = return 0
			| otherwise = playIO humanFirst $ if' humanFirst (humanTurn t >>= computerTurn) (computerTurn t >>= humanTurn)

