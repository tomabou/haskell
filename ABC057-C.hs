main = do
    n <- readLn :: IO Integer
    print.minimum.(map $ length.show.(div n))
            .(filter $ (==0).(mod n)) $ [1..(ceiling .sqrt. fromIntegral $ n)]
