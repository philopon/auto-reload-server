{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import System.Process
import System.Exit (ExitCode(ExitSuccess))
import Control.Exception (bracket)
import Control.Concurrent (threadDelay, killThread, forkIO)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad(forever, forM_, void)
import Data.Monoid
import Data.Maybe(fromMaybe, isJust)
import Options.Applicative
import System.FSNotify (withManagerConf, watchDirChan, WatchConfig(..), Debounce(..))

deriving instance Show Debounce


data Options = Options
    { initial     :: Bool
    , debounce    :: Debounce
    , polling     :: Maybe Int
    , directories :: [String]
    , commands    :: [String]
    } deriving (Show)


optionsP :: Parser Options
optionsP = (<*>) helper $
    Options <$> initial <*> debounce <*> polling <*> directories <*> commands
  where
    initial = switch $ short 'i' <> long "initial" <> help "run command initially"
    debounce = option (Debounce . fromIntegral <$> auto) $ short 'd' <> long "debounce" <> help "debounce" <> value DebounceDefault <> help "debounce (sec)"
    polling = option (Just . (*1000000) <$> auto) $ short 'p' <> long "polling" <> help "use polling" <> value Nothing
    directories = many $ strOption $ short 'w' <> long "watch" <> help "directories to watch"
    commands = many $ strOption $ short 'c' <> long "command" <> help "commands"


myParserInfo :: ParserInfo Options
myParserInfo = info optionsP fullDesc


execCommands [] = return ()
execCommands (command:commands) = bracket (spawnCommand command) (\p -> terminateProcess p >> waitForProcess p) $ \p -> do
    ex <- waitForProcess p
    if ex == ExitSuccess
        then execCommands commands
        else putStrLn $ show command ++ " exit with exit code: " ++ show ex


main :: IO ()
main = do
    opts@Options{..} <- execParser myParserInfo
    let conf = WatchConfig
            { confDebounce = debounce
            , confPollInterval = fromMaybe 1000000 polling
            , confUsePolling = isJust polling
            }
    directories <- return $ if null directories then ["."] else directories
    events <- newChan
    thread <- newMVar =<< if initial then Just <$> forkIO (execCommands commands) else return Nothing
    withManagerConf conf $ \mgr -> do
        forM_ directories $ \d -> watchDirChan mgr d (\_ -> True) events
        forever $ do
            ev <- readChan events
            print ev
            modifyMVar_ thread $ \v -> do
                maybe (return ()) killThread v
                Just <$> forkIO (execCommands commands)
