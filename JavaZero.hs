module JavaZero where

import JavaZero.Types as J
import JavaZero.Parser as P
import JavaZero.CodeGen.CIL as C
import System.FilePath.Posix

compile filename = do
    result <- P.parse filename
    case result of     
        Left err -> putStrLn $ show err
        Right unit -> do
            let cil = C.generate (takeBaseName filename) unit
            putStr cil
            writeFile (filename ++ ".cil") cil
        
