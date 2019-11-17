{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Function ((&))
import Data.Foldable

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Exit

import qualified Data.Text.IO as TIO

import Text.HTML.TagSoup
import Text.HTML.Scalpel hiding (text)

import NeatInterpolation

import Lib

import Flow

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["-h"]     -> cmdlineHelp >> exitSuccess
    ["--help"] -> cmdlineHelp >> exitSuccess
    [fname]    -> interpreter fname
    _          -> cmdlineHelp >> exitSuccess

cmdlineHelp = putStrLn "usage: htmlTree FILENAME"

interpreter fname = do
  putStrLn ""
  putStrLn "Welcome to HTMLTree!"
  tags <- parseTags <$> readFile fname

  loop tags where

    loop :: [Tag String] -> IO ()
    loop html = do
      putStrLn $ "=========================\n"
              <> "enter a selector command:"
      line <- prompt "> "
      case line of
        ('.':className) -> showClasses className html
        ('#':idName)    -> showIDs idName html
        "help"          -> showHelp
        "exit"          -> exit
        tagName         -> showTags tagName html
      loop html

prompt s = do 
  putStr s
  hFlush stdout
  getLine

exit = putStrLn "Bye!" >> exitSuccess

showTags :: String -> [Tag String] -> IO ()
showTags tagName = showMatching $ tagSelector tagName

showClasses :: String -> [Tag String] -> IO ()
showClasses className = showMatching $ AnyTag @: [hasClass className]

showIDs :: String -> [Tag String] -> IO ()
showIDs idName = showMatching $ AnyTag @: ["id" @= idName]

showHelp :: IO ()
showHelp = TIO.putStrLn [text|
    Commands:

      .<CLASS>
      #<ID>
      <TAGNAME>
|]

showMatching :: Selector -> [Tag String] -> IO ()
showMatching sel html = case matches of
  Just [] -> putStrLn "No matches."
  Just ts -> traverse_ (\s -> putStrLn s *> putStrLn "") ts
  Nothing -> return ()
  where
    matches :: Maybe [String]
    matches = scrape (htmls sel) html

