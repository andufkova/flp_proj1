-- FLP: bkg-2-cnf
-- Aneta Dufkova (xdufko02)
-- 2022

module Main (main)  where

import System.IO.Error (catchIOError)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import DataTypes (BKG(..))
import ParseInput (parseBKG)
import Funcs (newBKGwithoutSimpleRules, newBKGinCNF)

-- End with an error
dieM :: String -> IO a
dieM text = hPutStrLn stderr text >>= const exitFailure

-- The file does not exist exception handler
readHandler :: IOError -> IO String
readHandler _ = do putStrLn "The file does not exist" ; return ""

-- Process input arguments
-- if there is just one argument, read the input, otherwise read the file
procInputArgs :: [String] -> IO (BKG -> IO (), String)
procInputArgs [task] = do
    input <- getContents
    case task of
     "-i" -> return (showBKG, input)
     "-1" -> return (showBKGwithoutSimpleRules, input)
     "-2" -> return (showCNF, input)
     _    -> dieM ("Unknown option " ++ task ++ ". Expected: ./flp21-fun -i|-1|-2 [filename]")
procInputArgs [task, filename] = do
    input <- (readFile filename) `catchIOError` readHandler
    case task of
     "-i" -> return (showBKG, input)
     "-1" -> return (showBKGwithoutSimpleRules, input)
     "-2" -> return (showCNF, input)
     _    -> dieM ("Unknown option " ++ task ++ ". Expected: ./flp21-fun -i|-1|-2 [filename]")
procInputArgs (_ : (_ : (_ : _))) = do
    dieM ("No option is not allowed. Expected: ./flp21-fun -i|-1|-2 [filename]")
procInputArgs [] = do
    dieM ("No option is not allowed. Expected: ./flp21-fun -i|-1|-2 [filename]")
    
-- print BKG
showBKG :: BKG -> IO ()
showBKG bkg = do
    putStr (show bkg)

-- transform BKG to BKG without simple rules and print it
showBKGwithoutSimpleRules :: BKG -> IO ()
showBKGwithoutSimpleRules bkg = do
    let bkgNew = newBKGwithoutSimpleRules bkg
    putStr (show bkgNew)

-- transform BKG to BKG without simple rules, then to CNF and print it
showCNF :: BKG -> IO ()
showCNF bkg = do
    let bkgCNF = newBKGinCNF (newBKGwithoutSimpleRules bkg)
    putStr (show bkgCNF)

main :: IO ()
main = do
    (action, input) <- procInputArgs =<< getArgs
    either dieM action (parseBKG input)





