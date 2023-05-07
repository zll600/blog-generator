-- src/HsBlog.hs

module HsBlog
  ( main,
    process,
  )
where

import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main =
  -- getArgs :: IO [String] Get the program arguments
  getArgs >>= \args ->
    case args of
      -- No program arguments: reading from stdin and writing to stdout
      [] ->
        -- getContents IO String Read all of the content from stdin
        getContents >>= \content ->
          putStrLn (process "Empty title" content)
      -- With input and output file paths as program arguments
      [input, output] ->
        -- readFile :: FilePath -> IO String Read all of the content from a file
        readFile input >>= \content ->
          -- doesFileExist :: FilePath -> IO Bool Checks whether a file exists
          doesFileExist output >>= \exists ->
            -- writeFile :: FilePath -> String -> IO () Write a string into a file
            let writeResult = writeFile output (process input content)
             in if exists
                  then whenIO confirm writeResult
                  else writeResult
      -- Any other kind of program arguments
      _ ->
        putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)"
    -- (*>) :: IO a -> IO b -> IO b
    *> getLine
    >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n"
            *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()
