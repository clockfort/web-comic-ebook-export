import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import System.IO

main = do
	handle <- openFile "requests" ReadMode
	list <- hGetContents handle
	let urls = filter (isInfixOf "http") $ splitOn " " list
	let fileSegments = concatMap (filter (isInfixOf ".jpg") . splitOn "/") urls
	let filenames = map ((concat . take 2) . split (onSublist ".jpg")) fileSegments
	sequence_ [ renameFile filename (show (fromJust $ elemIndex filename filenames)++".jpg") | filename <- filenames ]
	putStrLn "Done."

lengthMax list = (length . show . last) list
deltaDigit list = map ( (lengthMax list-) . length . show ) list


