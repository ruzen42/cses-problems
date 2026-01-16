import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.List

parseInput :: B.ByteString -> [Int]
parseInput bs = map (fst . fromJust . B.readInt) (B.words bs)

main :: IO ()
main = do
    input <- B.getContents
    let nums   = tail (parseInput input)
        result = length $ group $ sort nums
    print result
