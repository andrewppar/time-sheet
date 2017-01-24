import System.IO 
import Data.List.Split

main = do 
    contents <- readFile "log.txt"
    let lns   = lines contents 
        weeks = getTotals lns
        nums  = map (\x -> read x :: Int) . map cleanString . map (\x -> last x) $ weeks 
    print $ sum nums 
    print $ fromIntegral (sum nums) / fromIntegral (length nums)

getTotals :: [String] -> [[String]] 
getTotals lns = map (endBy ":") . filter (\x -> head x == 'W') . clean $ lns 

clean :: [String] -> [String] 
clean xs = filter (\x -> x /= "" && x /= " ") xs 

cleanString :: String -> String 
cleanString = filter (\x -> x /=' ')
