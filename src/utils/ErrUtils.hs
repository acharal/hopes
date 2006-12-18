module ErrUtils where

import IO
import Control.Monad.Error

newtype ErrMsg = Err String
    deriving Show

type WarnMsg = ErrMsg

instance Error ErrMsg where
    strMsg str = Err str

errMsg :: String -> ErrMsg
errMsg s = Err s

printError :: ErrMsg -> IO ()
printError (Err str) = putStr $ "Error: "++ str ++ "\n"