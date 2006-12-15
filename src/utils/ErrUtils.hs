module ErrUtils where

import IO

type ErrMsg = String
type WarnMsg = ErrMsg

errMsg :: String -> ErrMsg
errMsg s = s

printError :: ErrMsg -> IO ()
printError str = putStr $ "Error: "++ str ++ "\n"