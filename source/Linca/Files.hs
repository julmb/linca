module Linca.Files (extension, FileType (..), fileType, exists, files, copy, remove, move) where

import Data.Char
import Control.Monad
import Text.Printf
import System.FilePath
import System.Directory
import Linca.Error

extension :: FilePath -> String
extension = map toLower . tail . takeExtension

data FileType = File | Directory | None deriving Eq

fileType :: FilePath -> IO FileType
fileType path = do
    fileExists <- doesFileExist path
    directoryExists <- doesDirectoryExist path
    case (fileExists, directoryExists) of
        (True, True) -> error $ errorMessage "fileType" $ printf "path (%s) is both a file and a directory" path
        (True, False) -> return File
        (False, True) -> return Directory
        (False, False) -> return None

exists :: FilePath -> IO Bool
exists path = do
    fileType <- fileType path
    return $ fileType /= None

files :: FilePath -> IO [FilePath]
files path = do
    fileType <- fileType path
    case fileType of
        File -> return [path]
        Directory -> listDirectory path >>= mapM (\entry -> files $ path </> entry) >>= return . concat
        None -> error $ errorMessage "files" $ printf "path (%s) does not exist" path

copy :: FilePath -> FilePath -> IO ()
copy source destination = do
    destinationExists <- exists destination
    when destinationExists $ error $ errorMessage "copy" $ printf "destination (%s) already exists" destination
    sourceType <- fileType source
    case sourceType of
        None -> error $ errorMessage "copy" $ printf "source (%s) does not exist" source
        File -> copyFile source destination
        Directory -> do
            createDirectory destination
            entries <- listDirectory source
            mapM_ (\entry -> copy (source </> entry) (destination </> entry)) entries

remove :: FilePath -> IO ()
remove path = do
    fileType <- fileType path
    case fileType of
        None -> error $ errorMessage "remove" $ printf "path (%s) does not exist" path
        File -> removeFile path
        Directory -> removeDirectoryRecursive path

move :: FilePath -> FilePath -> IO ()
move source destination = do
    destinationExists <- exists destination
    when destinationExists $ error $ errorMessage "move" $ printf "destination (%s) already exists" destination
    sourceType <- fileType source
    case sourceType of
        None -> error $ errorMessage "move" $ printf "source (%s) does not exist" source
        File -> renameFile source destination
        Directory -> renameDirectory source destination
