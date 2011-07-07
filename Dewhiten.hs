import Control.Monad
import DewhitenCore
import FileHelper
import System
import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.FilePath
import System.FilePath.Glob (compile)

main :: IO ()
main = do
    args <- getArgs
    (flags, nonFlags) <- parseOptions args
    when (Version `elem` flags) printVersion
    when (Help    `elem` flags) printHelp
    validateArguments nonFlags
    let pattern = nonFlags !! 0
    let dir     = nonFlags !! 1
    dewhiten (useRecursion flags) (useBlanks flags) pattern dir
    where useRecursion flags = if RecursiveSearch `elem` flags
                                   then Recursive
                                   else NonRecursive
          useBlanks flags    = if BlankLines `elem` flags
                                   then UseBlanks
                                   else CopyIndent

dewhiten :: RecursionOptions -> IndentOptions -> String -> String -> IO ()
dewhiten recurse indent pattern dir = do
    files <- findFiles recurse (compile pattern) dir
    mapM_ (dewhitenFile indent) files

dewhitenFile :: IndentOptions -> FilePath -> IO ()
dewhitenFile indent file = do
    contents <- readFile file
    let newContents = dewhitenString indent contents
    if length contents /= length newContents
        then do putStrLn $ "Dewhitened " ++ file
                writeFile file newContents
        else return ()

printVersion :: IO ()
printVersion = do
    putStrLn $  "dewhiten 1.0\n\n"
             ++ "License: GPLv3 (http://www.gnu.org/licenses/gpl.html)\n"
             ++ "Written in Haskell, June, 2011\n"
             ++ "By Elliot Cameron, http://www.3noch.com/\n"
             ++ "git Repository: https://github.com/3noch/Dewhiten\n"
    exitWith ExitSuccess

printHelp :: IO ()
printHelp = do
    putStrLn help
    exitWith ExitSuccess

data Flag = RecursiveSearch
          | BlankLines
          | KeepOriginals
          | Version
          | Help
          deriving (Show,Eq)

options :: [OptDescr Flag]
options = [ Option ['R','r'] ["recursive"] (NoArg RecursiveSearch) "apply to the given directory recursively"
          , Option ['b'] ["blank"] (NoArg BlankLines) "convert lines with only white-space into blank lines"
          , Option ['o'] ["keep-originals"] (NoArg KeepOriginals) "keep original files by appending .orig to their names"
          , Option ['V'] ["version"] (NoArg Version) "show version information"
          , Option ['h','?'] ["help"] (NoArg Help) "show this help"
          ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions args =
    case getOpt Permute options args of
        (flags, nonFlags, [])   -> return (flags, nonFlags)
        (_,     _,        errs) -> error $ concat errs ++ help

validateArguments :: [String] -> IO ()
validateArguments nonFlags
    | length nonFlags /= 2 = error $ "Unrecognized syntax.\n" ++ help
    | otherwise            = checkDirectory (nonFlags !! 1)
    where checkDirectory dir = do
              dirExists <- doesDirectoryExist dir
              if dirExists
                  then return ()
                  else error $ "Given directory does not exist.\n" ++ help

help = usageInfo header options
       where header =  "Usage: dewhiten [OPTION]... PATTERN DIRECTORY\n"
                    ++ "Search for the glob PATTERN in DIRECTORY and remove extraneous white-space from the files that match the search.\n"
                    ++ "Example: dewhiten -R '*.py' ./\n\n"
                    ++ "Possible values for OPTION are:"
