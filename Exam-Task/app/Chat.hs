module Main where

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import           Control.Monad (liftM, when)
import           Control.Monad.Fix (fix)

import           Network.Socket
import           System.IO

main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 15001 iNADDR_ANY)
    listen sock 5
    chan <- newChan
    chatLoop sock chan 0

type ClientNumber = Int
type MessageText = String
type Message = (MessageText, ClientNumber)

chatLoop :: Socket -> Chan Message -> Int -> IO ()
chatLoop sock chan clientNum = do
    conn <- accept sock
    forkIO $ runConnection conn chan clientNum
    chatLoop sock chan $! clientNum + 1

runConnection :: (Socket, SockAddr) -> Chan Message -> Int -> IO ()
runConnection (sock, _) chan clientNum = do
    hHandle <- socketToHandle sock ReadWriteMode
    hSetBuffering hHandle NoBuffering

    hPutStr hHandle "Enter your name: "
    name <- liftM init $ hGetLine hHandle
    writeChan chan ((name ++ " joined!"), clientNum)
    hPutStr hHandle "You can write your messages!\n> "

    inputChan <- dupChan chan
    readerThread <- forkIO $ fix $ \loop -> do
        (msg, nextClientNum) <- readChan inputChan
        when (clientNum /= nextClientNum) $ hPutStr hHandle ("\n" ++ msg ++ "\n> ")
        loop

    fix $ \loop -> do
        msg <- liftM init $ hGetLine hHandle
        hPutStr hHandle "> "
        
        if msg == "exit"
        then hPutStrLn hHandle "\nYou left!"
        else (writeChan chan ((name ++ "> " ++ msg), clientNum)) >> loop

    killThread readerThread
    writeChan chan ((name ++ " left!"), clientNum)
    hClose hHandle
