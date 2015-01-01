import Control.Concurrent       (threadDelay)
import Foreign.Erlang

main = do
    self <- createSelf "haskell"
    mbox <- createMBox self
    putStrLn $ "mbox: " ++ (show mbox)
    mbox' <- createNamedMBox "net_kernel" self
    putStrLn $ "mbox': " ++ (show mbox')

    --threadDelay $ 5 * 1000000
    --msg <- mboxRecv mbox' :: IO ErlType
    --putStrLn "mboxRecv: " ++ (show msg)
    loop mbox'

loop mbox = do
    (ErlTuple [from@(ErlPid (ErlAtom node) a b c), msg@(ErlTuple [_,ErlTuple [_,ref],_])]) <- mboxRecv mbox

    putStrLn $ "mboxRecv from: " ++ (show from)
    putStrLn $ "          msg: " ++ (show msg)
    mboxSend mbox node (Left from) $ ErlTuple [ref, ErlAtom "yes"]
    loop mbox