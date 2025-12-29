{-# LANGUAGE StrictData #-}
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
import System.Directory
import System.Environment
import System.Posix.Files
import System.Process

import ShowTDiff

pollPeriod = 5000
sleepThreshold = 1800000

data Options = Options
	{ optEdit :: Int
	, optExpect :: Bool
	, optDatabaseFile :: FilePath
	, optList :: Bool
	} deriving Show

defaultOptions    = Options
	{ optEdit = 0
	, optExpect = False
	, optDatabaseFile = ""
	, optList = False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['e'] ["edit"] (OptArg ((\a o -> o { optEdit = a }) . read . fromMaybe "10") "N") "edit the last N database entries"
	, Option ['x'] ["expect"] (NoArg (\o -> o { optExpect = True })) "print the expected sleepiness time"
	, Option ['d'] ["database-file"] (ReqArg (\a o -> o { optDatabaseFile = a}) "FILE") "use FILE as the sleep data base"
	, Option ['l'] ["list"] (NoArg (\o -> o { optList = True })) "display the database in the current timezone"
	]

parseOpts :: [String] -> IO Options
parseOpts argv =
	case getOpt Permute options argv of
		(o,[],[]  ) -> do
			home <- getAppUserDataDirectory "posplyu"
			return $ foldl' (flip id) (defaultOptions { optDatabaseFile = home ++ "/database" }) o
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: posplyu"

main :: IO ()
main = do
	setFileCreationMask $ unionFileModes groupModes otherModes
	args <- getArgs
	opts <- parseOpts args
	when (0 < optEdit opts) $ do
		db <- readDB $ optDatabaseFile opts
		let (left, edit) = splitAt ((length db) - optEdit opts) db
		editLocal <- mapM (mapM utcToLocalZonedTime) edit
		edited <- toUser editLocal
		writeDB (optDatabaseFile opts) $ left ++ map (map zonedTimeToUTC) edited
	when (optList opts) $ do
		db <- readDB $ optDatabaseFile opts
		times <- mapM (mapM utcToLocalZonedTime) db
		putStr $ formatUser times
	when (optExpect opts) $ do
		db <- readDB $ optDatabaseFile opts
		let sleepOffset = 60 * 60 * 15
		let expectedTime = addUTCTime sleepOffset $ last $ head $ filter (not . isNap) $ reverse db
		expectedLocalTime <- utcToLocalZonedTime expectedTime
		currentTime <- getCurrentTime
		let relTimeS = if expectedTime > currentTime
			then "in " ++ showTDiff (diffUTCTime expectedTime currentTime)
			else showTDiff (diffUTCTime currentTime expectedTime) ++ " ago"
		putStrLn $ "Sleepiness expected at " ++ show expectedLocalTime ++ " (" ++ relTimeS ++ ")"
	when (not $ or [0 < optEdit opts, optExpect opts, optList opts]) $ do
		let dirname = intercalate "/" $ init $ split (dropDelims $ whenElt (== '/')) $ optDatabaseFile opts
		createDirectoryIfMissing True dirname
		d <- openDisplay ""
		flip evalStateT False $ forever $ pollIdle d $ optDatabaseFile opts
		return ()

ser :: Show a => [[a]] -> String
ser = unlines . map ((intercalate "\t") . (map show))
des :: Read a => String -> [[a]]
des = map ((map read) . (wordsBy (== '\t'))) . lines

readDB :: FilePath -> IO [[UTCTime]]
readDB dbfn = do
	f <- readFile dbfn
	return $ des f

writeDB :: FilePath -> [[UTCTime]] -> IO ()
writeDB dbfn list = writeFile dbfn $ ser list

formatUser :: [[ZonedTime]] -> String
formatUser = unlines . map ((intercalate "\t") . (map formatTimeRFC3339))

toUser :: [[ZonedTime]] -> IO [[ZonedTime]]
toUser list = do
	let tmpfn = "tmp"
	writeFile tmpfn $ formatUser list
	system $ "\"$EDITOR\" " ++ tmpfn
	fromUser <- readFile tmpfn
	removeLink tmpfn
	return $ map ((map (fromJust . parseTimeRFC3339)) . (wordsBy (== '\t'))) $ lines $ fromUser

pollIdle :: Display -> FilePath -> StateT Bool IO ()
pollIdle d dbfn = do
	t <- liftIO $ getXIdleTime d
	s1 <- get
	put $ t > sleepThreshold
	s2 <- get
	liftIO $ when (s1 /= s2) $ (forkIO $ notify dbfn s2) >> return ()
	liftIO $ threadDelay $ pollPeriod * 1000

isNap :: [UTCTime] -> Bool
isNap [a, b] = diffUTCTime b a < 60 * 60 * 3
isNap a = error $ "Unexpected nap: " ++ show a

notify :: FilePath -> Bool -> IO ()
notify dbfn status = do
	t <- getCurrentTime
	let dbline = (show t) ++ if status then "\t" else "\n"
	appendFile dbfn dbline
	putStr dbline
