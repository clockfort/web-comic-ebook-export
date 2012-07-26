import Codec.Archive.Zip
import Control.Monad
import qualified Data.ByteString.Lazy as B
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
	sequence_ [ renameFile filename ( zeroPrefix filename filenames++".jpg" ) | filename <- filenames ]
	archive <- addFilesToArchive [OptRecursive] emptyArchive [ zeroPrefix filename filenames++".jpg" | filename <- filenames]
	B.writeFile "comicExport.cbz" $ fromArchive archive
	putStrLn "Done."

lengthMax = length . show . length
indexOf filename filenames = show (fromJust $ elemIndex filename filenames)
zeroPrefix filename filenames = replicate ( lengthMax filenames + 2 - ( length . show ) (indexOf filename filenames) ) '0' ++ indexOf filename filenames
