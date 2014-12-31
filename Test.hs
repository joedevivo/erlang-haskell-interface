import Foreign.Erlang

main = do
    self <- createSelf "haskell"
    loop 0

loop n = loop $ n + 1