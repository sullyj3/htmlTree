{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Exit

import Text.HTML.TagSoup
import Text.HTML.Scalpel

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
      putStrLn "enter a selector command"
      line <- prompt "> "
      case line of
        ('.':className) -> showClasses className html
        ('#':idName)    -> showIDs idName html
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

showMatching :: Selector -> [Tag String] -> IO ()
showMatching sel html = case scrape (htmls sel) html of
  Just [] -> putStrLn "No matches."

  -- todo: print the tags prettier
  Just ts -> mapM_ (putStrLn . show) ts
  Nothing -> return ()

