{-# LANGUAGE OverloadedStrings #-}


module NLP.Concraft.Croatian.Server
(
-- * Server
  runConcraftServer

-- * Client
, submit
) where


import           Control.Applicative ((<$>))
import           Control.Monad (forever, void, mapM_)
import           Control.Concurrent (forkIO)
import           System.IO (Handle, hFlush)
import qualified Network as N
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as S
import qualified NLP.Concraft.Morphosyntax as X
import           NLP.Concraft.Croatian.Morphosyntax
import qualified NLP.Concraft.Croatian as C
import qualified NLP.Concraft.Croatian.Request as R
import qualified Data.Tagset.Positional as P

import qualified NLP.Morphosyntax.Analyzer as A

------------------------------------------------
-- Server
-------------------------------------------------


-- | Run a Concraft server on a given port.
runConcraftServer :: A.Analyzer -> C.Concraft -> N.PortID -> IO ()
runConcraftServer analyzer concraft port = N.withSocketsDo $ do
    sock <- N.listenOn port
    forever $ sockHandler analyzer concraft sock


-- | Read and process short requests from the socket.
sockHandler :: A.Analyzer -> C.Concraft -> N.Socket -> IO ()
sockHandler analyzer concraft sock = do
    (handle, _, _) <- N.accept sock
    -- putStrLn "Connection established"
    void $ forkIO $ do
        -- putStrLn "Waiting for input..."
        inp <- recvMsg handle              -- TODO make things streaming, not strict
        -- putStr "> " >> T.putStrLn inp
        out <- R.short analyzer concraft inp
        -- putStr "No. of sentences: " >> print (length out)
        sendMsg handle out


-------------------------------------------------
-- Client
-------------------------------------------------


-- | Submit the given request.
submit :: N.HostName -> N.PortID -> R.Request R.TagWork -> IO (Either [X.Sent Word P.Tag] [[(S.Set P.Tag, P.Tag)]])
submit host port inp = do
    handle <- N.connectTo host port
    -- putStrLn "Connection established"
    -- putStr "Send request: " >> T.putStrLn inp
    sendMsg handle inp
    recvMsg handle


-------------------------------------------------
-- Communication
-------------------------------------------------


sendMsg :: B.Binary a => Handle -> a -> IO ()
sendMsg h msg = do
    let x = B.encode msg
        n = fromIntegral $ BS.length x
    sendInt h n
    BS.hPut h x
    hFlush h


recvMsg :: B.Binary a => Handle -> IO a
recvMsg h = do
    n <- recvInt h
    B.decode <$> BS.hGet h n


sendInt :: Handle -> Int -> IO ()
sendInt h x = BS.hPut h (B.encode x)


recvInt :: Handle -> IO Int
recvInt h = B.decode <$> BS.hGet h 8
