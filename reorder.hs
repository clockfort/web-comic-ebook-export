import System.IO
import Data.List
import Data.List.Split
import Maybe
import System.Directory
import Control.Monad

main = do
	handle <- openFile "requests" ReadMode
	list <- hGetContents handle
	let urls = filter (isInfixOf "http") $ splitOn " " list
	let filenames = map concat $ map (take 2) $ map (split (onSublist ".jpg")) $ concat $ map (filter (isInfixOf ".jpg")) $ map (splitOn "/") urls
	sequence_ [ renameFile filename (show (fromJust $ elemIndex filename filenames)++".jpg") | filename <- filenames ]
	putStrLn "Done."
