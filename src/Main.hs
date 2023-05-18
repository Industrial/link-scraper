{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (nub)
import Data.Text as T
import System.Environment
import Text.HTML.Scalpel
import Text.HTML.Scalpel.Core (Selector, chroots)

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = listUrlsForSite url
handleArgs _ = putStrLn "usage: list-all-images URL"

listUrlsForSite :: URL -> IO ()
listUrlsForSite url = do
  hrefs <- scrapeURL url (attrs "href" "a")
  maybe printError printImages hrefs
  where
    printError = putStrLn "ERROR: Could not scrape the URL!"
    printImages = mapM_ putStrLn