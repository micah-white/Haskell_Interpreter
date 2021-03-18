module FileIO where

    import System.IO
    import System.IO.Unsafe

    copyfile infile outfile =
        do
            inh <- openFile infile ReadMode
            outh <- openFile outfile WriteMode
            copy inh outh
            hClose inh
            hClose outh
        

    copy inh outh =
        do
            atEof <- hIsEOF inh
            if atEof
                then return ()
                else do 
                    c <- hGetChar inh
                    hPutChar outh c
                    copy inh outh

    fileToString :: String -> String
    fileToString infile =
        do
            text <- unsafePerformIO (readFile infile)
            -- doSomethingWith text
            return text
