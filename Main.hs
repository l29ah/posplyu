module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC3339
import Graphics.X11
import Graphics.X11.XScreenSaver
import System.Console.GetOpt
import System.Environment
import System.Posix.Files
import System.Process

pollPeriod = 5000
sleepThreshold = 1800000
dbfn = "database"

data Options = Options
	{ optEdit :: Int
	, optExpect :: Bool
	} deriving Show

defaultOptions    = Options
	{ optEdit = 0
	, optExpect = False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['e'] ["edit"] (OptArg ((\a o -> o { optEdit = a }) . read . fromMaybe "10") "N") "edit the last N database entries"
	, Option ['x'] ["expect"] (NoArg (\o -> o { optExpect = True })) "print the expected sleepiness time"
	]

parseOpts :: [String] -> IO Options
parseOpts argv =
	case getOpt Permute options argv of
		(o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: posplyu"

main :: IO ()
main = do
	setFileCreationMask $ unionFileModes groupModes otherModes
	args <- getArgs
	opts <- parseOpts args
	when (0 < optEdit opts) $ do
		db <- readDB
		let (left, edit) = splitAt ((length db) - optEdit opts) db
		--leftWithTZ <- mapM (mapM (\t -> (getTimeZone t) >>= (\z -> return (t, z)))) left
		editLocal <- mapM (mapM utcToLocalZonedTime) edit
		edited <- toUser editLocal
		writeDB $ left ++ map (map zonedTimeToUTC) edited
		return ()
	when (optExpect opts) $ do
		db <- readDB
		let sleepOffset = 60 * 60 * 15
		time <- utcToLocalZonedTime $ addUTCTime sleepOffset $ last $ head $ filter (not . isNap) $ reverse db
		putStrLn $ "Sleepiness expected at " ++ (show time)
	when (not $ or [0 < optEdit opts, optExpect opts]) $ do
		d <- openDisplay ""
		flip evalStateT False $ forever $ pollIdle d
		return ()

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
	writeFile tmpfn $ unlines $ map ((intercalate "\t") . (map formatTimeRFC3339)) list
	system $ "vim " ++ tmpfn
	fromUser <- readFile tmpfn
	removeLink tmpfn
	return $ map ((map (fromJust . parseTimeRFC3339)) . (wordsBy (== '\t'))) $ lines $ fromUser

pollIdle :: Display -> StateT Bool IO ()
pollIdle d = do
	t <- liftIO $ getXIdleTime d
	s1 <- get
	put $ t > sleepThreshold
	s2 <- get
	liftIO $ when (s1 /= s2) $ (forkIO $ notify s2) >> return ()
	liftIO $ threadDelay $ pollPeriod * 1000

isNap :: [UTCTime] -> Bool
isNap [a, b] = diffUTCTime b a < 60 * 60 * 3

notify :: Bool -> IO ()
notify status = do
	t <- getCurrentTime
	let dbline = (show t) ++ if status then "\t" else "\n"
	appendFile dbfn dbline
	putStr dbline
