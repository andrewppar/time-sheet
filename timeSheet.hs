import Data.Time 
import System.IO 
import System.Directory
import Data.List

main = do 
    now <- getCurrentTime
    let today = utctDay (convertToEST now)
    contents <- readFile "punchCard.txt"
    if contents == "" 
        then do
            putStrLn "How many hours did you work today? "
            answer <- getLine
            appendFile "punchCard.txt" (show(today) ++ "\n")
            appendFile "punchCard.txt" (answer ++ "\n")
    else  do
        let a = lines contents 
            b = filter (\x -> not (x == "")) a 
            dd = read $ (b !! 0) :: Day
            days = mapFold (\x -> fst x) (pairUp b)
            lastEntry = read $ (last days) :: Day 
            daysDates = mapFold (\x -> read x :: Day) days 
        now <- getCurrentTime
        let today = utctDay (convertToEST now)
        
        whatToDo "punchCard.txt" daysDates today
        --putStrLn $ "You've worked "++(calcJustHours contents)++" hours this week."   
    contents2 <- readFile "punchCard.txt"
    putStrLn $ "You've worked "++(calcJustHours contents2)++" hours this week."
    restTime contents2
restTime :: String -> IO()
restTime c = let 
            num = read (calcJustHours c) :: Int 
            in if num < 50 
                then do 
                    putStrLn "Keep at it!"
                else do 
                    putStrLn "Don't you think it's time for a break?"
convertToEST :: UTCTime -> UTCTime 
convertToEST t = addUTCTime (-14400) t 

printDays :: [String] -> [String]
printDays xs = let 
                days = mapFold (\x -> read x :: Day) xs 
               in mapFold (currentDay) days

fillDays :: Day -> [Day] -> [Day]
fillDays d [] = [d]
fillDays d ds = let 
                 l = length ds 
                 diff = diffDays d (last ds)
                 numDiff = [1..diff]
                 pairList = mapFold (\x -> (x,addDays x (last ds))) numDiff
                in mapFold (\(x,y) -> y) pairList 


--enterData :: IO ()
--enterData = if compareLast 
--      then do 
            

whatToDo :: String -> [Day] -> Day -> IO()
whatToDo file ds d2
    | (diffDays (head ds) d2) < -6 = do
        contents <- readFile "punchCard.txt"
        let days = calcHours contents
        appendFile "log.txt" "\n"
        appendFile "log.txt" (contents ++ "\n")
        appendFile "log.txt" (days ++ "\n") 
        (tempName, tempHandle) <- openTempFile "." "temp" 
        removeFile "punchCard.txt"
        renameFile tempName "punchCard.txt"

    | otherwise = do 
        let moreHours = fillDays d2 ds
            actions = mapFold (\x -> do
                putStrLn $ "How many hours did you work on " ++ (show x)
                answer <- getLine
                appendFile file ((show x) ++ "\n")
                appendFile file (answer ++ "\n")) moreHours 
        sequence_ actions

calcHours :: String -> String
calcHours c = let 
                b = lines c
                days = mapFold (\x -> fst x) (pairUp b)
                hours = mapFold (\x -> read (snd x):: Int) (pairUp b) 
                totalhours = sumFold hours 
                beg = show (head days)
                end = show (last days)
              in "Week of "++beg++" - "++end++": "++ (show totalhours)

calcJustHours :: String -> String 
calcJustHours c = let 
                b = lines c
                days = mapFold (\x -> fst x) (pairUp b)
                hours = mapFold (\x -> read (snd x):: Int) (pairUp b) 
                in show $ sumFold hours 
    
sumFold :: [Int] -> Int
sumFold xs = foldl (\acc x -> x + acc) 0 xs 

pairUp :: [a] -> [(a,a)]
pairUp [] = []
pairUp [x] = [(x,x)]
pairUp (x:y:xs) = (x,y): pairUp xs 

dates :: [Day] 
dates = 
        let start = read "2016-08-02 00:22:40.987539 UTC" :: UTCTime 
        in mapFold (\x -> addDays (x+1) (utctDay start)) [(-1)..]


day0 :: Day 
day0 = let day =read "2016-08-02 00:22:40.987539 UTC" :: UTCTime
       in utctDay day  

currentDay :: Day -> String
currentDay d = let n = diffDays d day0 
                in dayDates n

dropFirst :: [a] -> [a]
dropFirst (x:xs) = xs 

dropFrontNum :: Int -> [a] -> [a]
dropFrontNum n xs = applyTimes n dropFirst xs

reverseFold :: [a] -> [a] 
reverseFold xs = foldl (\acc x -> x : acc) [] xs

dropLastNum :: Int -> [a] -> [a]
dropLastNum n xs = reverseFold $ dropFrontNum n (reverseFold xs)

mapFold :: (a -> b) -> [a] -> [b]
mapFold f xs = foldr (\x acc -> f x : acc) [] xs 
 
takeNum :: Int -> [a] -> [a]
takeNum 0 xs = []
takeNum n (x:xs) = x : (takeNum (n-1) xs)

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes 0 f s = s 
applyTimes n f s = f (applyTimes (n-1) f s)

zipEm :: [a] -> [b] -> [(a,b)]
zipEm (x:xs) (y:ys) = (x,y) : zipEm xs ys

numDates :: [(Int,Day)]
numDates = zipEm [1..] dates

dayDates ::Integer ->  String
dayDates n
    | mod n 7 == 6 = "Monday" 
    | mod n 7 == 0 = "Tuesday"
    | mod n 7 == 1 = "Wednesday"
    | mod n 7 == 2 = "Thursday" 
    | mod n 7 == 3 = "Friday"
    | mod n 7 == 4 = "Saturday"
    | mod n 7 == 5 = "Sunday" 
    | otherwise = "Error"

--weeks :: [(String,Day)]
--weeks = mapFold (\(x,y) -> ((dayDates x), y)) numDates

--weekDayDay :: Day -> String
--weekDayDay d = 
