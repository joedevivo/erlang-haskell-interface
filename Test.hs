import Control.Concurrent
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

    nk_mbox <- createNamedMBox "net_kernel" self
    debugM "Test" $ "nk_mbox: " ++ (show nk_mbox)
    --net_kernel nk_mbox
    forkIO $ net_kernel nk_mbox

    rex_mbox <- createNamedMBox "rex" self
    forkIO $ rex rex_mbox

    loop 0

loop x = do
    loop x

net_kernel mbox = do
    (ErlTuple [
        from@(ErlPid (ErlAtom node) a b c),
        msg@(ErlTuple [_,ErlTuple [_,ref],_])
        ]) <- mboxRecv mbox

    debugM "Test" $ "net_kernel from: " ++ (show from)
    debugM "Test" $ "            msg: " ++ (show msg)
    mboxSend mbox node (Left from) $ ErlTuple [ref, ErlAtom "yes"]
    net_kernel mbox

rex mbox = do
    (ErlTuple [
        from@(ErlPid (ErlAtom node) a b c),
        msg@(ErlTuple [_,ErlTuple [_,ref],ErlTuple [
            -- [ErlAtom "call",ErlAtom "mod",ErlAtom "fun",ErlList [ErlAtom "args"]
                                                        call, --ErlAtom "call",
                                                        ErlAtom modName,
                                                        ErlAtom funName,
                                                        args,
                                                        _ --from@(ErlPid (ErlAtom node) a b c)
                                                    ]
            ])
        ]) <- mboxRecv mbox
    debugM "Test" $ "rpc " ++ modName ++ ":" ++ funName ++ "(" ++ (show args) ++ ")"
    {-
erlang: rpc:call('haskell@127.0.0.1', 'mod', 'fun', ['args']).

rex from: ErlPid (ErlAtom "erlang@127.0.0.1") 38 0 2
     msg: ErlTuple [ErlAtom "$gen_call",
                    ErlTuple [ErlPid (ErlAtom "erlang@127.0.0.1") 38 0 2,
                              ErlNewRef (ErlAtom "erlang@127.0.0.1") 2 [0,0,0,191,0,0,0,0,0,0,0,0]],
                    ErlTuple [ErlAtom "call",
                              ErlAtom "mod",
                              ErlAtom "fun",
                              ErlList [ErlAtom "args"],
                              ErlPid (ErlAtom "erlang@127.0.0.1") 31 0 2]]
 -}
    mboxSend mbox node (Left from) $ ErlTuple [ref, ErlAtom "haskell_equals_very_yes"]
    rex mbox

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