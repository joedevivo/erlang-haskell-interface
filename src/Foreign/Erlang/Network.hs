module Foreign.Erlang.Network (
  -- * Low-level communication with the Erlang Port-Mapper Daemon.
    epmdGetNames
  , epmdGetPort
  , epmdGetPortR4
  , ErlRecv
  , ErlSend
  , erlConnectC
  , erlConnectS
  , toNetwork
  , epmdSend
  , epmdAlive2Req
  , handshakeS
  ) where

import Control.Concurrent       (threadDelay)
import Control.Exception        (assert, bracketOnError)
import Control.Monad            (liftM)
import Data.Binary              (decode, encode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits                ((.|.))
import Data.Char                (chr, ord)
import Data.Digest.Pure.MD5     (md5)
import Data.List                (unfoldr)
import Data.Word
import Foreign.Erlang.Types
import Network                  (PortID(..), connectTo, withSocketsDo)
import Network.Socket
import Numeric                  (readHex)
import System.Directory         (getHomeDirectory)
import System.FilePath          ((</>))
import System.IO
import System.IO.Unsafe         (unsafePerformIO)
import System.Random            (randomIO)
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as B

erlangVersion = 5
erlangProtocolVersion = 131
passThrough = 'p'

flagPublished          =  0x01
flagAtomCache          =  0x02
flagExtendedReferences =  0x04
flagDistMonitor        =  0x08
flagFunTags            =  0x10
flagDistMonitorName    =  0x20
flagHiddenAtomCache    =  0x40
flagNewFunTags         =  0x80
flagExtendedPidsPorts  = 0x100

getUserCookie = do
    home <- getHomeDirectory
    readFile $ home </> ".erlang.cookie"

toNetwork     :: Int -> Integer -> [Word8]
toNetwork b n = reverse . take b $ unfoldr toNetwork' n ++ repeat 0
  where
    toNetwork' 0 = Nothing
    toNetwork' n = let (b, a) = n `divMod` 256 in Just (fromIntegral a, b)

ntohs n = let (b, a) = n `divMod` 256 in 256*a + b

erlDigest                  :: String -> Word32 -> [Word8]
erlDigest cookie challenge = let
    [(n, _)] = readHex . show . md5 . B.pack $ cookie ++ show challenge
    in toNetwork 16 n

packn, packN :: B.ByteString -> Put
packn msg = putn (fromIntegral . B.length $ msg) >> putLazyByteString msg
packN msg = putN (fromIntegral . B.length $ msg) >> putLazyByteString msg

sendMessage :: (B.ByteString -> Put) -> (B.ByteString -> IO ()) -> B.ByteString -> IO ()
sendMessage pack out = out . runPut . pack

recvMessage            :: Int -> (Int -> IO B.ByteString) -> IO B.ByteString
recvMessage hdrlen inf = (liftM (unpack hdrlen) $ inf hdrlen) >>= inf
  where
    unpack 2 = runGet getn
    unpack 4 = runGet getN

type ErlSend = (Maybe ErlType, Maybe ErlType) -> IO ()
type ErlRecv = IO (Maybe ErlType, Maybe ErlType)

erlSend :: (B.ByteString -> IO ()) -> ErlSend
erlSend send (Nothing, _)    = send B.empty
erlSend send (Just ctl, msg) = send . runPut $ do
    tag passThrough
    putMsg ctl
    maybe (return ()) putMsg msg
    flush
  where
    putMsg msg = do
      putC erlangProtocolVersion
      putErl msg

erlRecv      :: IO B.ByteString -> ErlRecv
erlRecv recv = do
    bytes <- recv
    return . flip runGet bytes $ do
      empty <- isEmpty
      if empty
        then return (Nothing, Nothing)
        else do
          pt <- getC
          assert (chr pt == passThrough) $ return ()
          ctl <- getMsg
          empty <- isEmpty
          if empty
            then return (Just ctl, Nothing)
            else case ctl of
                   ErlTuple (ErlInt n:_) | n `elem` [2, 6] -> do
                     msg <- getMsg
                     return (Just ctl, Just msg)
                   _ -> return (Just ctl, Nothing)
  where
    getMsg = do
      ver <- getC
      assert (ver == erlangProtocolVersion) $ getErl

erlConnectC           :: String -> String -> IO (ErlSend, ErlRecv)
erlConnectC self node = withSocketsDo $ do
    port <- epmdGetPort node
    let port' = PortNumber (PortNum . fromIntegral . ntohs $ port)
    bracketOnError
      (connectTo epmdHost port' >>= \h -> hSetBuffering h NoBuffering >> return h)
      hClose $ \h -> do
        let out = sendMessage packn (B.hPut h)
        let inf = recvMessage 2 (B.hGet h)
        handshakeC out inf self
        let out' = sendMessage packN (\s -> B.hPut h s >> hFlush h)
        let inf' = recvMessage 4 (B.hGet h)
        return (erlSend out', erlRecv inf')

--erlConnectC :: ErlConnect
--erlConnectC = erlConnect handshakeC

handshakeC  :: (B.ByteString -> IO ()) -> IO B.ByteString -> String -> IO ()
handshakeC out inf self = do
    cookie <- getUserCookie
    sendName
    recvStatus
    challenge <- recvChallenge
    let reply = erlDigest cookie challenge
    challenge' <- liftM fromIntegral (randomIO :: IO Int)
    challengeReply reply challenge'
    recvChallengeAck cookie challenge'

  where
    sendName = out . runPut $ do
        tag 'n'
        putn erlangVersion
        putN $ flagExtendedReferences .|. flagExtendedPidsPorts
        putA self

    recvStatus = do
        msg <- inf
        assert ("sok" == B.unpack msg) $ return ()

    recvChallenge = do
        msg <- inf
        return . flip runGet msg $ do
            tag <- getC
            version <- getn
            flags <- getN
            challenge <- getWord32be
            return challenge

    challengeReply reply challenge = out . runPut $ do
        tag 'r'
        putWord32be challenge
        puta reply

    recvChallengeAck cookie challenge = do
        let digest = erlDigest cookie challenge
        msg <- inf
        let reply = take 16 . drop 1 . map (fromIntegral . ord) . B.unpack $ msg
        assert (digest == reply) $ return ()

erlConnectS :: Handle -> String -> IO (String, ErlSend, ErlRecv)
erlConnectS h self = withSocketsDo $ do
    --port <- epmdGetPort node
    --let port' = PortNumber (PortNum . fromIntegral . ntohs $ port)
    bracketOnError
      (return h)
      hClose $ \h -> do
        let out = sendMessage packn (B.hPut h)
        let inf = recvMessage 2 (B.hGet h)
        to <- handshakeS out inf self
        let out' = sendMessage packN (\s -> B.hPut h s >> hFlush h)
        let inf' = recvMessage 4 (B.hGet h)
        return (to, erlSend out', erlRecv inf')


handshakeS :: (B.ByteString -> IO ()) -> IO B.ByteString -> String -> IO (String)
handshakeS out inf self = do
    cookie <- getUserCookie
    (_, _, _, name) <- recvName
    putStrLn $ "SEND_NAME received: " ++ show name
    sendStatus
    putStrLn $ "SEND_STATUS sent: sok"
    challenge <- liftM fromIntegral (randomIO :: IO Int)
    sendChallenge challenge
    --threadDelay $ 5 * 1000000
    (challenge', reply) <- recvChallengeReply
    putStrLn $ "SEND_CHALLENGE_REPLY received: " ++ show challenge' ++ ":" ++ show reply
    let reply' = erlDigest cookie challenge'
    challengeAck reply'
    return (name)

  where
    --nodename = takeWhile (\x -> x /= '@') self
    recvName = do
        msg <- inf
        return . flip runGet msg $ do
            msgType <- getC
            erlVer <- getn
            flags <- getN
            name <- liftM B.unpack getRemainingLazyByteString
            return (msgType, erlVer, flags, name)

    sendStatus = out . runPut $ do
        tag 's'
        putA "ok"
        flush

    sendChallenge challenge = out . runPut $ do
        tag 'n'
        putn erlangVersion
        putN $ flagExtendedReferences .|. flagExtendedPidsPorts
        putN challenge
        putA $ self ++ "@127.0.0.1"
        flush

    recvChallengeReply = do
        msg <- inf
        return . flip runGet msg $ do
            tag <- getC
            challengejr <- getWord32be
            digest <- liftM B.unpack getRemainingLazyByteString
            return (challengejr, digest)

    challengeAck reply = out . runPut $ do
        tag 'a'
        puta reply
        flush



epmdHost = "127.0.0.1"
epmdPort = Service "epmd"

withEpmd = withSocketsDo . bracketOnError
    (connectTo epmdHost epmdPort >>= \h -> hSetBuffering h NoBuffering >> return h)
    hClose

epmdSend     :: String -> IO B.ByteString
epmdSend msg = withEpmd $ \hdl -> do
    let out = runPut $ putn (length msg) >> putA msg
    B.hPut hdl out
    hFlush hdl
    B.hGetContents hdl

epmdAlive2Req :: String -> Int -> IO ()-- B.ByteString
epmdAlive2Req node port = withEpmd $ \hdl -> do
    --let name = takeWhile (\x -> x /= '@') node
    let msg = runPut $ tag 'x' >>
                       putn port >>
                       putC 77 >> -- node type
                       putC 0 >>  -- protocol
                       putn erlangVersion >>
                       putn erlangVersion >>
                       putn (length node) >>
                       putA node >>
                       putn 0 -- "Extra" length, 0 for none
    let len = fromIntegral $ B.length msg
    let out = runPut $ putn len >> putLazyByteString msg
    let loop n = loop n
    B.hPut hdl out
    hFlush hdl
    B.hGetContents hdl
    return ()
    loop 0

-- | Return the names and addresses of all registered Erlang nodes.
epmdGetNames :: IO [String]
epmdGetNames = do
    reply <- epmdSend "n"
    let txt = runGet (getN >> liftM B.unpack getRemainingLazyByteString) reply
    return . lines $ txt

-- | Return the port address of a named Erlang node.
epmdGetPort      :: String -> IO Int
epmdGetPort name = do
    reply <- epmdSend $ 'p' : name
    return $ runGet getn reply

-- | Returns (port, nodeType, protocol, vsnMax, vsnMin, name, extra)
epmdGetPortR4      :: String -> IO (Int, Int, Int, Int, Int, String, String)
epmdGetPortR4 name = do
    reply <- epmdSend $ 'z' : name
    return $ flip runGet reply $ do
        getn
        port <- getn
        nodeType <- getC
        protocol <- getC
        vsnMax <- getn
        vsnMin <- getn
        name <- getn >>= getA
        extra <- liftM B.unpack getRemainingLazyByteString
        return (port, nodeType, protocol, vsnMax, vsnMin, name, extra)
