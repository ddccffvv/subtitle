import Text.ParserCombinators.Parsec
import Data.Time.Format
import Data.Time
import System.Locale
import Text.Printf
import System.Environment

data TimeStamp = TimeStamp Time Time
	deriving Show

data Time = Time Integer Integer Integer Integer

data SubtitleBlock = SubtitleBlock Int TimeStamp [String]


instance Show Time where
	show (Time h m s millis) = (printf "%02d" h) ++ ":" ++ (printf "%02d" m) ++ ":" ++ (printf "%02d" s) ++ "," ++ (printf "%03d" millis)

instance Show SubtitleBlock where
	show (SubtitleBlock n (TimeStamp begin end) lines) = (show n) ++ "\n" ++ (show begin) ++ " --> " ++ (show end) ++ "\n" ++ (foldr (\x y -> x ++ "\n" ++ y) "" lines)

addDelayTime :: Integer -> Time -> Time
addDelayTime d (Time h m s ms) = 
		let x = d + ms + (s*1000) + (m*60000) + (h*3600000)
		    millis = mod x 1000
		    secs = mod (quot x 1000) 60
		    mins = mod (quot x 60000) 60
		    hours = (quot x 3600000)
		in
		    Time hours mins secs millis

addDelaySubtitleBlock :: Integer -> SubtitleBlock -> SubtitleBlock
addDelaySubtitleBlock d (SubtitleBlock x y z) = SubtitleBlock x (addDelayTimeStamp d y) z

addDelayTimeStamp :: Integer -> TimeStamp -> TimeStamp
addDelayTimeStamp d (TimeStamp b e) = TimeStamp (addDelayTime d b) (addDelayTime d e)

parseId :: GenParser Char st Int
parseId =
	do number <- many digit
	   eol
	   return (read number)

parseTimeLine :: GenParser Char st TimeStamp
parseTimeLine = 
	do hours <- many (noneOf ":")
	   char ':'
	   mins <- many (noneOf ":")
	   char ':'
	   secs <- many (noneOf ",")
	   char ','
	   millis <- many (noneOf " ") 
	   string " --> "
	   hoursa <- many (noneOf ":")
	   char ':'
	   minsa <- many (noneOf ":")
	   char ':'
	   secsa <- many (noneOf ",")
	   char ','
	   millisa <- many (noneOf "\r\n")
	   eol
	   return $ TimeStamp (Time (read hours :: Integer) (read mins :: Integer) (read secs :: Integer) (read millis :: Integer)) (Time (read hoursa :: Integer) (read minsa :: Integer) (read secsa :: Integer) (read millisa :: Integer))


parseLine :: GenParser Char st String
parseLine =
	do line <- many1 (noneOf "\r\n")
	   eol
	   return line

parseTextBlock :: GenParser Char st [String]
parseTextBlock =
	do lines <- many1 parseLine
	   return lines

parseSubtitleBlock :: GenParser Char st SubtitleBlock
parseSubtitleBlock =
	do id <- parseId
	   time <- parseTimeLine
	   text <- parseTextBlock
	   eol
	   return $ SubtitleBlock id time text

parseSubtitles :: GenParser Char st [SubtitleBlock]
parseSubtitles = 
	do b <- many parseSubtitleBlock
	   return b

addDelaySubtitles :: Integer -> [SubtitleBlock] -> [SubtitleBlock]
addDelaySubtitles d s = map (addDelaySubtitleBlock d) s

eol =   try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
	<|> string "\r"

main =
    do c <- getContents
       args <- getArgs
       case parse parseSubtitles "(stdin)" c of
         Left e -> do putStrLn "Error parsing input:"
		      print e
	 Right r -> mapM_ print (map (addDelaySubtitleBlock (read (head args) :: Integer)) r)
