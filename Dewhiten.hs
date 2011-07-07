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
    when (ShowVersion `elem` flags) printVersion
    when (ShowHelp    `elem` flags) printHelp
    validateArguments nonFlags
    dewhiten (getConfig flags) (nonFlags !! 0) (nonFlags !! 1)

dewhiten :: Config -> String -> String -> IO ()
dewhiten config pattern dir = do
    files <- findFiles (useRecursion config) (compile pattern) dir
    mapM_ (dewhitenFile config) files

dewhitenFile :: Config -> FilePath -> IO ()
dewhitenFile config file = do
    oldContents <- readFile file
    let newContents = dewhitenString (useBlankLines config) oldContents
    if length oldContents /= length newContents
        then do putStrLn $ "Dewhitened " ++ file
                overwriteFile config file oldContents newContents
        else return ()

overwriteFile :: Config -> FilePath -> String -> String -> IO ()
overwriteFile config file oldContents newContents
    | keepOriginals config = do writeFile (file <.> "orig") oldContents
                                writeFile file newContents
    | otherwise            = writeFile file newContents

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

data Config = Config { useRecursion  :: Bool
                     , useBlankLines :: Bool
                     , keepOriginals :: Bool
                     }

getConfig :: [Flag] -> Config
getConfig flags = Config { useRecursion  = SearchRecursively `elem` flags
                         , useBlankLines = UseBlankLines     `elem` flags
                         , keepOriginals = KeepOriginals     `elem` flags
                         }

data Flag = SearchRecursively
          | UseBlankLines
          | KeepOriginals
          | ShowVersion
          | ShowHelp
          deriving (Show,Eq)

options :: [OptDescr Flag]
options = [ Option ['R','r'] ["recursive"] (NoArg SearchRecursively) "apply to the given directory recursively"
          , Option ['b'] ["blank"] (NoArg UseBlankLines) "convert lines with only white-space into blank lines"
          , Option ['o'] ["keep-originals"] (NoArg KeepOriginals) "keep original files by appending .orig to their names"
          , Option ['V'] ["version"] (NoArg ShowVersion) "show version information"
          , Option ['h','?'] ["help"] (NoArg ShowHelp) "show this help"
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
