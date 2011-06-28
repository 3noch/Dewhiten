module FileHelper where
    import Control.Monad (filterM)
    import System.Directory 
    import System.FilePath
    import System.FilePath.Glob 
    
    findFiles :: Bool -> Pattern -> FilePath -> IO [FilePath]
    findFiles recurse pattern dir = do
        items     <- getDirItems dir
        files     <- filterM isGoodFile (globFiles items)
        dirs      <- filterM isGoodDir items
        moreFiles <- if recurse
                         then mapM (findFiles recurse pattern) dirs
                         else return []
        return $ files ++ concat moreFiles
        where globFiles files = filter matchFile files
              matchFile fullPath = match pattern (takeFileName fullPath)
    
    isGoodDir :: FilePath -> IO Bool
    isGoodDir dir = do
        exists <- doesDirectoryExist dir
        p <- if exists
                then getPermissions dir
                else return noPermissions
        return $ exists && readable p && writable p && searchable p
    
    isGoodFile :: FilePath -> IO Bool
    isGoodFile file = do
        exists <- doesFileExist file
        p <- if exists
                 then getPermissions file
                 else return noPermissions 
        return $ exists && readable p && writable p
    
    getDirItems :: FilePath -> IO [FilePath]
    getDirItems dir = do
        items <- globDir1 globStar dir
        return items
    
    noPermissions = Permissions { readable   = False
                                , writable   = False 
                                , executable = False
                                , searchable = False
                                }
    
    globStar = compile "*"
