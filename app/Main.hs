--pune.hs
-- to solve challenge program given here > http://punetech.com/apply/
import System.IO
import Data.List

main :: IO()
main = do 
    -- openFile returns a Handle
    inh <- openFile "pInput.txt" ReadMode
    -- get all the contents from input file
    inputData <- hGetContents inh
    
    -- get file contents split into array
    let pData = lines inputData
    
    -- store all lines in a list of Tweet data type
    let tweets = getTweets pData
    
    -- Remove duplicate entries
    -- nubBy function removes duplicate entries
    -- first argument is a 'comparer function'
    -- second argument is list on which it works
    let nDup = Data.List.nubBy tweetEq tweets
    
    -- Sort in descending order of timestamp
    -- first argument is ordering function
    -- second argument is list to work on
    let sDup = Data.List.sortBy tweetOrd nDup
    
    -- open file for writing. openFile returns a Handle
    outh <- openFile "pOutput.txt" WriteMode
    -- send Handle and data to write to file
    writeToFile outh sDup
    --close both Handles
    hClose inh
    hClose outh

    -- Custom data type to store a tweet
data Tweet = Tweet {
        tid :: Int,
        name :: String,
        time :: Double,
        message :: String
    }deriving(Eq,Show)

-- function that equates two Tweets
-- passed to nubBy
tweetEq :: Tweet -> Tweet -> Bool
tweetEq t1 t2 = (tid t1) == (tid t2)

-- function that checks order
-- passed to sortBy
tweetOrd :: Tweet -> Tweet -> Ordering
tweetOrd t1 t2 = compare (time t2) (time t1)

-- convert Tweet Lines into List of Tweets
getTweets :: [String] -> [Tweet]
getTweets [] = []
getTweets (x:xs) =  let parts =  split x '|' in
             let temp = Tweet{
                    tid = read (parts !! 0),
                    time = read (parts !! 1),
                    name = (parts !! 2),                    
                    message = (parts !! 3)
                }
            in [temp] ++ (getTweets xs)

-- no comments :-P
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

-- no comments
writeToFile :: Handle -> [Tweet] -> IO()
writeToFile outh [] =  hPutStrLn outh ""
writeToFile outh (x:xs) = do
                let line = (show(tid x)) ++ "|" ++ (show(time x)) ++ "|" ++ (name x) ++ "|" ++ (message x)
                hPutStrLn outh line
                writeToFile outh xs
