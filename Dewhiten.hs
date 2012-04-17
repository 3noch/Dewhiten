import Control.Monad
import DewhitenCore
import FileHelper
import System.Directory (doesDirectoryExist)
import System.Console.GetOpt
import System.FilePath
import System.FilePath.Glob (compile)

main :: IO ()
main = do
    args <- getArgs
    (flags, nonFlags) <- parseOptions args
    when (ShowVersion `elem` flags) printVersion
    when (ShowHelp    `elem` flags) printHelp
    validateArguments nonFlags
    dewhiten flags (nonFlags !! 0) (nonFlags !! 1)

dewhiten :: [Flag] -> String -> String -> IO ()
dewhiten cfg pattern dir = do
    files <- findFiles (Recursive `elem` cfg) (compile pattern) dir
    mapM_ (dewhitenFile cfg) files

dewhitenFile :: [Flag] -> FilePath -> IO ()
dewhitenFile cfg file = do
    oldContents <- readFile file
    let lineModifier = if AddBlankIndent  `elem` cfg then addBlankIndent  else noBlankIndent
    let fileModifier = if AddTrailingLine `elem` cfg then addTrailingLine else noTrailingLine
    let newContents = dewhitenString lineModifier fileModifier oldContents
    if length oldContents /= length newContents
        then overwriteFile (KeepOriginals `elem` cfg) file oldContents newContents
        else return ()

overwriteFile :: Bool -> FilePath -> String -> String -> IO ()
overwriteFile keepOrig file oldContents newContents
    | keepOrig  = do writeFile (file <.> "orig") oldContents
                     doWrite
    | otherwise = doWrite
    where doWrite = do putStrLn $ "Dewhitened " ++ file
                       writeFile file newContents

printVersion :: IO ()
printVersion = do
    putStrLn $  "dewhiten 1.0\n\n"
             ++ "License: GPLv3 (http://www.gnu.org/licenses/gpl.html)\n"
             ++ "Written in Haskell, June, 2011\n"
             ++ "By Elliot Cameron, http://www.3noch.com/\n"
             ++ "git Repository: http://github.com/CovenantEyes/Dewhiten\n"
    exitWith ExitSuccess

printHelp :: IO ()
printHelp = do
    putStrLn help
    exitWith ExitSuccess

data Flag = Recursive
          | AddBlankIndent
          | AddTrailingLine
          | KeepOriginals
          | ShowVersion
          | ShowHelp
          deriving (Show,Eq)

options :: [OptDescr Flag]
options = [ Option ['R','r'] ["recursive"] (NoArg Recursive) "search the given directory recursively"
          , Option ['d'] ["indent"] (NoArg AddBlankIndent) "add correct indentation to blank lines and lines with only white-space"
          , Option ['t'] ["trailing"] (NoArg AddTrailingLine) "ensure each file has exactly one trailing blank line"
          , Option ['o'] ["keep-originals"] (NoArg KeepOriginals) "keep original files by appending '.orig` to their names"
          , Option ['V', 'v'] ["version"] (NoArg ShowVersion) "show version information"
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
