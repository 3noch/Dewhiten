import Control.Monad (mapM_)
import DewhitenCore (fixLines)
import FileHelper (findFiles)
import System (getArgs)
import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.FilePath
import System.FilePath.Glob (compile)

main = do
    args <- getArgs
    (flags, pattern, dir) <- parseOptions args
    files <- findFiles (elem Recursive flags) (compile pattern) dir
    mapM_ (dewhiten flags) files


data Flag = Recursive
          | BlankLines
          | KeepOriginals
          | Version
          | Help
          deriving (Show,Eq)

options :: [OptDescr Flag]
options = [ Option ['R','r'] ["recursive"]      (NoArg Recursive)     "apply to the given directory recursively"
          , Option ['b']     ["blank"]          (NoArg BlankLines)    "convert lines with only white-space into blank lines"
          , Option ['o']     ["keep-originals"] (NoArg KeepOriginals) "keep original files by appending .orig to their names"
          , Option ['V']     ["version"]        (NoArg Version)       "show version number"
          , Option ['h','?'] ["help"]           (NoArg Help)          "show this help"
          ]

parseOptions :: [String] -> IO ([Flag], FilePath, FilePath)
parseOptions args =
    case getOpt Permute options args of
        (flags, nonFlags, [])   -> do validateArguments nonFlags
                                      return (flags, nonFlags !! 0, nonFlags !! 1)
        (_,     _,        errs) -> error $ concat errs ++ help

validateArguments :: [String] -> IO ()
validateArguments args 
    | length args /= 2 = error $ "Unrecognized syntax.\n" ++ help
    | otherwise        = do dirExists <- doesDirectoryExist (args !! 1)
                            if dirExists
                                then return ()
                                else error $ "Given directory does not exist.\n" ++ help

help = usageInfo header options
       where header =  "Usage: dewhiten [OPTION]... PATTERN DIRECTORY\n"
                    ++ "Search for the glob PATTERN in DIRECTORY and remove extraneous white-space from the files that match the search.\n"
                    ++ "Example: dewhiten -R *.py ./\n\n"
                    ++ "Possible values for OPTION are:"

dewhiten :: [Flag] -> FilePath -> IO ()
dewhiten flags file = do
    putStrLn $ "Dewhitening " ++ file
    contents <- readFile file
    let fixedLines = fixLines (lines contents)
    writeFile file $! unlines fixedLines
