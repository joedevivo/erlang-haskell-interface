module Foreign.Erlang.Server (
    serve
  , plainHandler
) where

import Control.Monad            (liftM)
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Maybe          (fromJust)
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import Foreign.Erlang.Network
import Foreign.Erlang.Types
import qualified Data.ByteString.Lazy.Char8 as B

type HandlerFunc = String -> Handle -> B.ByteString -> IO ()

serve :: String -> IO ()
serve nodename = withSocketsDo $
    do
        addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just "0") -- Zero gives us a rando port later
        let serveraddr = head addrinfos

        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        setSocketOption sock NoDelay 1
        setSocketOption sock KeepAlive 1
        bindSocket sock (addrAddress serveraddr)

        port <- socketPort sock
        putStrLn $ "Opening up on port " ++ (show port)

        forkIO $ epmdAlive2Req nodename $ read $ show port

        --(out, inf) <- erlConnectS sock nodename

        listen sock 5
         -- Create a lock to use for synchronizing access to the handler
        lock <- newMVar ()

        -- Loop forever waiting for connections.  Ctrl-C to abort.
        procRequests lock sock
        --loop 0
    where
          loop n = loop n
          -- | Process incoming connection requests
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock =
              do (connsock, clientaddr) <- accept mastersock
                 handleLog lock clientaddr $
                    B.pack "Foreign.Erlang.Server: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl NoBuffering
                 (out, inf) <- erlConnectS connhdl nodename
                 (mctl, mmsg) <- inf
                 case mctl of
                     -- Nothing is a keepalive.  All we want to do is echo it.
                     Nothing  -> handleLog lock clientaddr $ B.pack "Nothing"
                     -- A real message goes to self to be dispatched.
                     Just ctl -> handleLog lock clientaddr $ B.pack $ show (fromJust mmsg)

                 {-
                 From 127.0.0.1:50869:
                  "ErlTuple [ErlAtom \"$gen_call\",ErlTuple [ErlPid (ErlAtom \"erlang@127.0.0.1\") 38 0 2,ErlNewRef (ErlAtom \"erlang@127.0.0.1\") 2 [0,0,0,228,0,0,0,0,0,0,0,0]],ErlTuple [ErlAtom \"is_auth\",ErlAtom \"erlang@127.0.0.1\"]]"
                 -}
                 --messages <- B.hGetContents connhdl
                 --mapM_ (handle lock clientaddr) (B.lines messages)
                 --handleMessage lock nodename connhdl messages
                 handleLog lock clientaddr $
                    B.pack "Something Erlangy this way comes"
                 --hClose connhdl
                 --handleLog lock clientaddr $
                 --   B.pack "Foreign.Erlang.Server: client disconnected"

          -- Lock the handler before passing data to it.
          handleLog :: MVar () -> SockAddr -> B.ByteString -> IO ()
          -- This type is the same as
          -- handle :: MVar () -> SockAddr -> String -> IO ()
          handleLog lock clientaddr msg =
              withMVar lock
                 (\a -> plainHandler clientaddr msg >> return a)
          --handleMessage :: MVar() -> HandlerFunc
          --handleMessage lock nodename sock msg =
          --    withMVar lock
          --       (\a -> erlangHandler nodename sock msg >> return a)

-- A simple handler that prints incoming packets
plainHandler ::  SockAddr -> B.ByteString -> IO ()
plainHandler addr msg =
    putStrLn $ "From " ++ show addr ++ ": " ++ show msg

{-
We need an erlang handler that:

1) Manages the distribution handshake
  http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html#id92374

  a) The existing code in the handshake module does the receiver half
  of the handshake, so we need to implement the inverse here.  Note:
  the good news then is that both handshake_out and handshake_in can
  be used to test eachother :D

2) After success there, parses the rest of the message into Erlang
   Types

-}
--erlangHandler :: HandlerFunc
--erlangHandler nodename handle msg =
--    handshakeS nodename handle
