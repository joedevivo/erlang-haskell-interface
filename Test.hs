import Foreign.Erlang
import System.Environment (getArgs)
import System.Exit

import System.Log.Logger

main = do
    args <- getArgs
    case args of
        [node] ->
            start node
        _ ->
            start "haskell"

start nodename = do
    setupLoggers DEBUG

    infoM "Test" $ "Starting Node: " ++ nodename
    self <- createSelf nodename
    mbox <- createMBox self
    debugM "Test" $ "mbox: " ++ (show mbox)
    mbox' <- createNamedMBox "net_kernel" self
    debugM "Test" $ "mbox': " ++ (show mbox')
    loop mbox'

loop mbox = do
    (ErlTuple [from@(ErlPid (ErlAtom node) a b c), msg@(ErlTuple [_,ErlTuple [_,ref],_])]) <- mboxRecv mbox

    debugM "Test" $ "mboxRecv from: " ++ (show from)
    debugM "Test" $ "          msg: " ++ (show msg)
    mboxSend mbox node (Left from) $ ErlTuple [ref, ErlAtom "yes"]
    loop mbox

setupLoggers :: Priority -> IO ()
setupLoggers p = do
    let loggers = [
                        "Test"
                      , "Foreign.Erlang"
                      , "Foreign.Erlang.Network"
                      , "Foreign.Erlang.OTP"
                      , "Foreign.Erlang.Processes"
                      , "Foreign.Erlang.Types"
                      , "Foreign.Erlang.Utilities"
                      , "Foreign.Erlang.OTP.GenServer"
                      , "Foreign.Erlang.OTP.Mnesia"
                    ]

    head [ updateGlobalLogger x (setLevel p) | x <- loggers ]
    return ()