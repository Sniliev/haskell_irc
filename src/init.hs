import Network
import System.IO
import Text.Printf
import Control.Concurrent
 
server = "46.238.31.230"
port   = 6667
chan   = "#general"
nick   = "NotABot"
 
main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "PASS" "totallysecure"
    write h "USER" (nick++" 0 * :notbot")
    line1<- hGetLine h
    putStrLn ("\n test " ++line1)
    line2<- hGetLine h
    putStrLn ("\n test " ++line2)
    ping <- hGetLine h
    putStrLn ("\n test " ++(parsePing ping))
    write h (parsePing ping) chan
    threadDelay 10000000
    write h "JOIN" chan
    listen h
 
parsePing :: String -> String
parsePing ping ="PONG "++head(tail (words ping))

checkForPing :: String -> Bool
checkForPing input
    |head (words input) == "PING" = True
    |otherwise = False

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t
 
listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    if checkForPing s
        then write h (parsePing s) chan
        else return ()
    putStrLn s
  where
    forever a = do a; forever a
