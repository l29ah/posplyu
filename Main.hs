module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.X11
import Graphics.X11.XScreenSaver
import System.Environment
import System.Posix.Files
import System.Process

pollPeriod = 5000
sleepThreshold = 1800000
dbfn = "database"
lastN = 10

ser :: Show a => [[a]] -> String
ser = unlines . map ((intercalate "\t") . (map show))
des :: Read a => String -> [[a]]
des = map ((map read) . (wordsBy (== '\t'))) . lines

readDB :: IO [[UTCTime]]
readDB = do
	f <- readFile dbfn
	return $ des f

writeDB :: [[UTCTime]] -> IO ()
writeDB list = writeFile dbfn $ ser list

toUser :: [[ZonedTime]] -> IO [[ZonedTime]]
toUser list = do
	let tmpfn = "tmp"
	writeFile tmpfn $ ser list
	system $ "vim " ++ tmpfn
	fromUser <- readFile tmpfn
	removeLink tmpfn
	return $ des fromUser

pollIdle :: Display -> StateT Bool IO ()
pollIdle d = do
	t <- liftIO $ getXIdleTime d
	s1 <- get
	put $ t > sleepThreshold
	s2 <- get
	liftIO $ when (s1 /= s2) $ (forkIO $ notify s2) >> return ()
	liftIO $ threadDelay $ pollPeriod * 1000

main :: IO ()
main = do
	setFileCreationMask $ unionFileModes groupModes otherModes
	args <- getArgs
	case args of
		["edit"] -> do
			db <- readDB
			let (left, edit) = splitAt ((length db) - lastN) db
			--leftWithTZ <- mapM (mapM (\t -> (getTimeZone t) >>= (\z -> return (t, z)))) left
			editLocal <- mapM (mapM utcToLocalZonedTime) edit
			edited <- toUser editLocal
			--writeDB $ left ++ edited
			print edited
			--print $ left ++ map (map zonedTimeToUTC) edited
			return ()
		_ -> do
			d <- openDisplay ""
			flip evalStateT False $ forever $ pollIdle d
			return ()

notify :: Bool -> IO ()
notify status = do
	t <- getCurrentTime
	let dbline = (show t) ++ if status then "\t" else "\n"
	appendFile dbfn dbline
	putStr dbline
