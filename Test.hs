import Foreign.Erlang

main = do
    self <- createSelf "haskell"
    --erlConnectS "haskell" "erlang@127.0.0.1"
    loop 0

loop n = loop $ n + 1