import System.IO 
import System.Directory
import Data.List

main = do 
    handle <- openFile "todo.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName , tempHandle) <- openTempFile tempdir "temp"
    contets <- hGetContents handle
    let todoTasks = lines contets
        numberedTasks = zipWith (\n line ->show n ++ " - " ++ line ) [0..] todoTasks
    putStrLn "These are your To do lists:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoTasks = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoTasks
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"