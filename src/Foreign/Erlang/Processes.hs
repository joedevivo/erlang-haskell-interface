module Foreign.Erlang.Processes (
  -- * Low-level communication.
  -- ** Represents a Haskell node (program).
    Self
  , createSelf
  -- ** Represents a Haskell process (thread).
  , MBox
  , createMBox
  , createNamedMBox
  , mboxRef
  , mboxSelf
  -- ** Represents Erlang nodes and processes.
  , Node
  , Pid
  -- ** Communication to/from Erlang.
  , mboxRecv
  , mboxRecv'
  , mboxSend
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad            (liftM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe          (fromJust)
import Data.List
import Foreign.Erlang.Network
import Foreign.Erlang.Types
import Network.BSD
import qualified Network.Socket as S
import System.IO
import System.Log.Logger

-- | The name of an Erlang node on the network.
type Node = String

data ErlMessage = ErlRegister (ErlType -> IO ())
                | ErlRegisterName String (ErlType -> IO ())
                | ErlGenRef ErlType
                | ErlSend String ErlType ErlType
                | ErlRegSend ErlType String String ErlType
                | ErlLink ErlType String ErlType
                | ErlUnlink ErlType String ErlType
                | ErlExit ErlType String ErlType ErlType
                | ErlExit2 ErlType String ErlType ErlType
                | ErlDispatch ErlType ErlType
                | ErlStop
                | ConnectedNode String ((Maybe ErlType, Maybe ErlType) -> IO ())
                deriving Show

instance Show (a -> b) where
    show _ = "<function>"

-- | Represents a Haskell node.  There should be one of these per process.
data Self = Self { send :: ErlMessage -> IO () }
instance Show Self where
    show _ = "*self*"

genPid nodename id = ErlPid (ErlAtom nodename) id 0 1
genRef nodename id = ErlNewRef (ErlAtom nodename) 1 . toNetwork 4 . fromIntegral $ id

-- | Instantiate a Haskell node.  This initializes the FFI.
createSelf          :: String -> IO Self
createSelf nodename = do
    inbox <- newEmptyMVar
    forkIO $ serve nodename inbox
    forkIO $ self nodename inbox

    node <- return .  Self $ putMVar inbox

    -- Try spawning net_kernel mbox
    nk_mbox <- createNamedMBox "net_kernel" node
    forkIO $ net_kernel nk_mbox

    return node  --Self $ putMVar inbox

self                :: String -> MVar ErlMessage -> IO ()
self nodename inbox = loop 1 [] [] []
  where
    --TODO: type signature for loop
    loop id registered mboxes nodes = do
        msg <- takeMVar inbox
        debugM "Foreign.Erlang.Processes" $ "loop msg recv'd: " ++ (show msg)
        case msg of
          ErlRegister mbox -> do
            let pid = genPid nodename id
            mbox pid
            loop (id+1) registered ((pid, mbox) : mboxes) nodes
          ErlRegisterName name mbox -> do
            let pid = ErlAtom name
            mbox pid
            loop id ((pid, mbox):registered) mboxes nodes
          ErlGenRef pid -> do
            let ref = genRef nodename id
            maybe (return ()) ($ ref) $ lookup pid mboxes
            loop (id+1) registered mboxes nodes
          ErlSend node pid msg -> do
            let ctl = toErlang (ErlInt 2, ErlAtom "", pid)
            (mnode, nodes') <- findNode node nodes
            case mnode of
              Just n  -> n (Just ctl, Just msg)
              Nothing -> return ()
            loop id registered mboxes nodes'
          ErlRegSend from node pid msg -> do
            let ctl = toErlang (ErlInt 6, from, ErlAtom "", ErlAtom pid)
            (mnode, nodes') <- findNode node nodes
            case mnode of
              Just n  -> n (Just ctl, Just msg)
              Nothing -> return ()
            loop id registered mboxes nodes'
          ErlLink from to pid -> do
              let ctl = toErlang (ErlInt 1, from, pid)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id registered mboxes nodes'
          ErlUnlink from to pid -> do
              let ctl = toErlang (ErlInt 4, from, pid)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id registered mboxes nodes'
          ErlExit from to pid reason -> do
              let ctl = toErlang (ErlInt 3, from, to, reason)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id registered mboxes nodes'
          ErlExit2 from to pid reason -> do
              let ctl = toErlang (ErlInt 8, from, to, reason)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id registered mboxes nodes'
          ErlDispatch ctl msg -> do
            case ctl of
              ErlTuple [ErlInt 2, _, pid] ->
                maybe (return ()) ($ msg) $ lookup pid mboxes
              ErlTuple [ErlInt 6, from, _, pid] ->
                maybe (return ()) ($ (ErlTuple [from, msg])) $ lookup pid registered
              _ -> return ()
            loop id registered mboxes nodes
          -- This clause is for when Erlang has connected to this node
          -- we're just telling this node to add it to the connected nodes.
          ConnectedNode to node -> do
            case lookup to nodes of
                Just n ->
                  loop id registered mboxes nodes
                Nothing ->
                  loop id registered mboxes ((to, node):nodes)
          ErlStop -> return ()
    {-
    TODO: Wrote this so I can remove some debug putStrLns, but still need to
          do the typing work.
    findNode
      to: String
      nodes: [(String, (a->b))]

      returns  (Just (a->b), nodes)
    I'm pretty sure this is right, but it breaks, so I know i'm wrong

    It's a monad thing.
    findNode :: String -> [(String, (a -> b))] -> (Maybe (a -> b), [(String, (a -> b))])
    -}
    findNode to nodes =
        case lookup to nodes of
          Just node -> return (Just node, nodes)
          Nothing   -> do
            (send, recv) <- erlConnectC nodename to
            mvar <- newEmptyMVar
            forkIO $ nodeSend mvar send
            forkIO $ nodeRecv mvar recv inbox
            let node = putMVar mvar
            return (Just node, ((to, node) : nodes))

{-
A `nodeSend` thread is responsible for communication to an Erlang
process.  It receives messages in an `MVar` and forwards them across
the network.
-}
nodeSend mvar send = loop
  where
    loop =
      takeMVar mvar >>= send >> loop

{-
A `nodeRecv` thread is responsible for communication from an Erlang
process.  It receives messages from the network and dispatches them as
appropriate.
-}
nodeRecv mvar recv outbox = loop
  where
    loop = do
        (mctl, mmsg) <- recv
        case mctl of
            -- Nothing is a keepalive.  All we want to do is echo it.
            Nothing  -> putMVar mvar (Nothing, Nothing)
            -- A real message goes to self to be dispatched.
            Just ctl -> putMVar outbox $ ErlDispatch ctl (fromJust mmsg)
        loop

-- | Haskell threads don't natively have Erlang process IDs.  Instead, we
-- use a mailbox abstraction that we can assign PIDs to for communication
-- with Erlang.

data MBox = MBox ErlType (MVar ErlType) Self
instance Show MBox where
    show (MBox t _mvar self) = "MBox " ++ (show t) ++ " MVar " ++ (show self)

-- | Represents a foreign (Erlang) process.  A process can be identified
-- either by its low-level ID (Left pid) or by its registered name (Right name).

type Pid  = Either ErlType String

-- | Return the PID of the given mailbox.

mboxSelf                :: MBox -> ErlType
mboxSelf (MBox pid _ _) = pid

-- | Return a new unique object reference.

mboxRef                        :: MBox -> IO ErlType
mboxRef mbox@(MBox pid _ self) = send self (ErlGenRef pid) >> mboxRecv mbox

-- Send an arbitrary message to the specified node and process.

-- | {Node, Pid} ! Msg.
mboxSend :: Erlang a => MBox -> Node -> Pid -> a -> IO ()
mboxSend (MBox _    _ self) node (Left  pid) msg = send self $ ErlSend node pid (toErlang msg)
mboxSend (MBox from _ self) node (Right pid) msg = send self $ ErlRegSend from node pid (toErlang msg)

-- | Receive the next message addressed to this mailbox.

mboxRecv                  :: MBox -> IO ErlType
mboxRecv (MBox _ inbox _) = takeMVar inbox

-- | Receive a reply message.  That is, looks for the next message
-- identified by the given reference.

mboxRecv'          :: MBox -> ErlType -> IO ErlType
mboxRecv' mbox ref = do
    msg <- mboxRecv mbox
    case msg of
        ErlTuple [ref', result] | ref' == ref -> return result
        _                                     -> mboxRecv' mbox ref

-- | Create a new process on the Haskell side.  Usually corresponds
-- to a thread but doesn't need to.

createMBox      :: Self -> IO MBox
createMBox self = do
    inbox <- newEmptyMVar
    send self $ ErlRegister (putMVar inbox)
    pid <- takeMVar inbox
    return $ MBox pid inbox self

createNamedMBox :: String -> Self -> IO MBox
createNamedMBox name self = do
    inbox <- newEmptyMVar
    send self $ ErlRegisterName name (putMVar inbox)
    pid <- takeMVar inbox
    return $ MBox pid inbox self

-- This is the loop that receives erlang messages to the net_kernel
-- module. Without it, you can't ping this node
net_kernel mbox = do
    (ErlTuple [
        from@(ErlPid (ErlAtom node) a b c),
        msg@(ErlTuple [_,ErlTuple [_,ref],_])
        ]) <- mboxRecv mbox

    debugM "Test" $ "net_kernel from: " ++ (show from)
    debugM "Test" $ "            msg: " ++ (show msg)
    mboxSend mbox node (Left from) $ ErlTuple [ref, ErlAtom "yes"]
    net_kernel mbox

{-
Erlang Message Listener
-}
type HandlerFunc = String -> Handle -> B.ByteString -> IO ()

serve :: String -> MVar ErlMessage -> IO ()
serve nodename outbox = S.withSocketsDo $
    do
        addrinfos <- S.getAddrInfo
                    (Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
                    Nothing (Just "0") -- Zero gives us a rando port later
        let serveraddr = head addrinfos

        sock <- S.socket (S.addrFamily serveraddr) S.Stream S.defaultProtocol
        S.setSocketOption sock S.NoDelay 1
        S.setSocketOption sock S.KeepAlive 1
        S.bindSocket sock (S.addrAddress serveraddr)

        port <- S.socketPort sock
        debugM "Foreign.Erlang.Processes" $ "Opening up on port " ++ (show port)

        forkIO $ epmdAlive2Req nodename $ read $ show port
        S.listen sock 5
         -- Create a lock to use for synchronizing access to the handler
        lock <- newMVar ()

        -- Loop forever waiting for connections.  Ctrl-C to abort.
        procRequests lock sock
    where
          -- | Process incoming connection requests
          procRequests :: MVar () -> S.Socket -> IO ()
          procRequests lock mastersock =
              do (connsock, clientaddr) <- S.accept mastersock
                 handleLog lock clientaddr $
                    B.pack "Foreign.Erlang.Server: client connnected"
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> S.Socket -> S.SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- S.socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl NoBuffering
                 (to, send, recv) <- erlConnectS connhdl nodename
                 mvar <- newEmptyMVar
                 forkIO $ nodeSend mvar send
                 forkIO $ nodeRecv mvar recv outbox
                 let node = putMVar mvar
                 putMVar outbox $ ConnectedNode to node

          -- Lock the handler before passing data to it.
          handleLog :: MVar () -> S.SockAddr -> B.ByteString -> IO ()
          -- This type is the same as
          -- handle :: MVar () -> SockAddr -> String -> IO ()
          handleLog lock clientaddr msg =
              withMVar lock
                 (\a -> plainHandler clientaddr msg >> return a)

-- A simple handler that prints incoming packets
plainHandler ::  S.SockAddr -> B.ByteString -> IO ()
plainHandler addr msg =
    debugM "Foreign.Erlang.Processes" $ "From " ++ show addr ++ ": " ++ show msg
