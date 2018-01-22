{-# LANGUAGE DeriveGeneric #-}

module Lib
    (jump, add)
     where

import GHC.Generics

import Data.Serialize
import qualified Data.ByteString as B
import System.Directory
import Data.Vector hiding ((++))
import Data.Vector.Serialize
import qualified Fuzzy as TF
import Fuzzy (original)
import Data.List (sortOn)

data PathInfo = PathInfo {
    fullPath :: String,
    score :: Float,
    lastAccess :: Integer
} deriving (Show, Generic)

type PathData = Vector PathInfo

findPath :: String -> PathData -> Maybe PathInfo
findPath path = find ((== path) . fullPath)

updateScore :: PathInfo -> PathInfo 
updateScore pathInfo =
    let oldScore = score pathInfo
    in
        pathInfo { score = oldScore + 1 / oldScore }

addPath :: String -> PathData -> PathData
addPath path pathData =
    case findIndex ((== path) . fullPath) pathData of
        Just index -> pathData // [(index, updateScore (pathData ! index))]
        Nothing -> cons (PathInfo path 1 1) pathData

search :: String -> PathData -> [PathInfo]
search query pathData = 
    Prelude.reverse . (sortOn score) . (fmap original) $ TF.filter query (toList pathData) "<" ">" fullPath False



instance Serialize PathInfo

-- COMMANDS

jump :: String -> IO (Maybe String)
jump query = do
    Right pathData <- getData
    case search query pathData of
        (x:xs) -> return . Just $ fullPath x
        _ -> return Nothing

add :: String -> IO ()
add path = do
    Right pathData <- getData
    let pathData' = addPath path pathData
    saveData pathData'

-- READING AND WRITING
defaultDataPath :: IO FilePath
defaultDataPath = (++ "/.hz") <$> getHomeDirectory

getData :: IO (Either String PathData)
getData = do
    path <- defaultDataPath
    rawPathData <- B.readFile path
    return $ decode rawPathData

saveData :: PathData -> IO ()
saveData pathData = do
    dataPath <- defaultDataPath
    let encodedData = encode pathData
    B.writeFile dataPath encodedData

