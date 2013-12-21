import System.Cmd
import System.Console.Readline
import System.Directory
import System.IO
import System.Process
import Data.Maybe
import Data.List.Split
import Data.List
import Data.Conduit.Process
import System.Posix.IO
import System.Posix.User
import System.Exit

cdCommand :: [String] -> IO ()
cdCommand args = setCurrentDirectory (args !! 1)

ailasCommand :: [String] -> IO ()
ailasCommand args = return ()

findKey :: String -> [(String, String)] -> Maybe String
findKey key [] = Nothing  
findKey key ((k,v):xs) = 
    if key == k then Just v  
    else findKey key xs 

indexOf :: String -> Char -> Int -> Int
indexOf [] c i = -1
indexOf (x:xs) c i = 
    if x == c then i
    else do indexOf xs c (i + 1)

runCmd :: String -> Bool -> Maybe Handle -> IO ()
runCmd command background hStdIn
    -- Runs the single command by itself
    | length commands == 1 && isNothing hStdIn = do        
        -- Redirect std out to append to a file
        if length args >= 3 && last (init args) == ">>" then do
            (_, Just std_out, _, processHandle) <- createProcess 
                (proc (args !! 0) (init $ init $ tail args))
                { std_out = CreatePipe }
            code <- waitForProcess processHandle
            content <- hGetContents std_out
            appendFile (last args) content 
            return ()
        -- Redirect std out to write to a file
        else if length args >= 3 && last (init args) == ">" then do
            (_, Just std_out, _, processHandle) <- createProcess 
                (proc (args !! 0) (init $ init $ tail args))
                { std_out = CreatePipe }
            code <- waitForProcess processHandle
            content <- hGetContents std_out
            writeFile (last args) content
            return ()
        -- Regular usage
        else do
            (_, _, _, processHandle) <- createProcess (proc (args !! 0) (tail args))
            if background == False then do
                code <- waitForProcess processHandle
                return ()
            else
                return ()
    -- Runs the last command in the sequence
    | length commands == 1 && isJust hStdIn = do        
        if length args >= 3 && last (init args) == ">>" then do
            (_, Just std_out, _, processHandle) <- createProcess 
                (proc (args !! 0) (init $ init $ tail args))
                { std_out = CreatePipe, std_in = UseHandle (fromJust hStdIn) }
            code <- waitForProcess processHandle
            content <- hGetContents std_out
            appendFile (last args) content
            return ()
        else if length args >= 3 && last (init args) == ">" then do
            (_, Just std_out, _, processHandle) <- createProcess 
                (proc (args !! 0) (init $ init $ tail args))
                { std_out = CreatePipe, std_in = UseHandle (fromJust hStdIn) }
            code <- waitForProcess processHandle
            content <- hGetContents std_out
            writeFile (last args) content
            return ()
        else do
            (_, _, _, processHandle) <- createProcess (proc (args !! 0) (tail args)) 
                { std_in = UseHandle (fromJust hStdIn) }
            if background == False then do
                code <- waitForProcess processHandle
                return ()
            else
                return ()
    -- Start of piping command sequence, and gives the 2nd process the stdin 
    -- of the 1st stdout
    | isNothing hStdIn  = do        
        (p_r, p_w) <- createPipe
        h_r <- fdToHandle p_r
        h_w <- fdToHandle p_w
        (_, _, _, _) <- createProcess (proc ((commands !! 0) !! 0) (tail (commands !! 0))) 
            { std_out = UseHandle h_w }
        runCmd (snd ((splitAt ((indexOf command '|' 0) + 1)) command)) background (Just h_r)
    -- Middle of piping command sequence
    | otherwise = do 
        (p_r, p_w) <- createPipe
        h_r <- fdToHandle p_r
        h_w <- fdToHandle p_w
        (_, _, _, _) <- createProcess (proc ((commands !! 0) !! 0) (tail (commands !! 0))) 
            { std_in = UseHandle (fromJust hStdIn), std_out = UseHandle h_w }
        -- Runs the next commmand with the stdin as the stdout of this program
        runCmd (snd ((splitAt ((indexOf command '|' 0) + 1)) command)) background (Just h_r)
    where 
          args = words command
          commands = map words (splitOn "|" command)

exec :: String -> IO ()
exec command
    | length args == 0 = return ()
    | args !! 0 == "cd" = do
        cdCommand args
    | args !! 0 == "ailas" = do
        ailasCommand args
    | otherwise = do
        runCmd realCommand background Nothing
    where background = last args == "&"
          args = words command
          realCommand = 
            if last args == "&" then 
                unwords $ init args
            else
                unwords args
        
readRc :: FilePath -> IO ()
readRc filePath = do 
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    mapM exec (lines contents)
    hClose handle

-- Makes the IO String prompt: "username" "directory"
getPrompt_ :: IO (String)
getPrompt_ = do
    pwd <- getCurrentDirectory
    username <- getEffectiveUserName
    if pwd == "/" then
        return (username ++ " / > ")
    else
        return (username ++ " " ++ last (splitOn "/" pwd) ++ " > ")

mainLoop :: IO ()
mainLoop = do
    prompt <- getPrompt_
    input <- readline prompt
    case input of
        Nothing -> return ()
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just "" -> mainLoop
        Just line -> do addHistory line
                        exec line
                        mainLoop

main = do
    readRc ".shellrc"
    mainLoop
