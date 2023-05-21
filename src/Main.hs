{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.ByteString.Char8 (putStrLn)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text as T (Text)
import Network.URI
  ( URI (uriPath, uriScheme),
    parseURI,
    uriToString,
  )
import System.Environment (getArgs)
import Text.HTML.Scalpel (Scraper, URL, attrs, scrapeURL)

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = listUrlsForSite url
handleArgs _ = putStrLn "usage: list-all-images URL"

createURI :: String -> String -> Maybe URI
createURI page href = do
  pageURI <- parseURI page
  hrefURI <- parseURI href
  let hrefURIPath = uriPath hrefURI
  let updatedURI = pageURI {uriScheme = "https:", uriPath = hrefURIPath}
  return updatedURI

createURIs :: String -> [String] -> [URI]
createURIs page = mapMaybe (createURI page)

createLinksFromURIs :: [URI] -> [String]
createLinksFromURIs = map (\a -> uriToString id a "")

scrapeLinks :: String -> IO (Maybe [Text])
scrapeLinks url = scrapeURL url $ attrs "href" "a"

listUrlsForSite :: URL -> IO ()
listUrlsForSite url = do
  visitPage url []
  where
    visitPage :: URL -> [URL] -> IO ()
    visitPage page visited = do
      hrefs <- scrapeLinks url
      mapM_ putStrLn hrefs

-- uris <- fmap (createURI page) hrefList
-- let uris = createURIs page (nub hrefList)
-- let links = createLinksFromURIs uris
-- let newLinks = filter (`notElem` visited) links
-- mapM_ putStrLn newLinks
-- forM_ newLinks $ \newLink -> visitPage newLink (newLink : visited)