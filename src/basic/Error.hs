module Error (
    module Error,
    module Control.Monad.Error
) where

import Loc
import Pretty
import Control.Monad.Error

{- this is a comment -}

type ErrDesc = Doc

data Message =
   Msg { 
        errloc  :: Loc,
        errtyp  :: ErrType,
        level   :: ErrLevel,
        desc    :: ErrDesc
    }

instance HasLocation Message where
    loc (Msg l _ _ _) = l

type Messages = ([Message], [Message])

data ErrLevel = 
      Fatal
    | Failure
    | Warning
  deriving Eq

data ErrType = 
      ParseError
    | TypeError
    | Internal

instance Error Message where
    strMsg str = internalErr (text str)

instance Error Messages where
    strMsg str = mkMsgs $ strMsg str

instance Pretty Message where
    ppr err
       | errloc err == bogusLoc = desc err
       | otherwise              = hang (ppr (errloc err) <>colon) 4 (desc err)


{-
class Monad m => MonadWarn m w where
    addWarning :: w -> m ()
    addWarning w = fail $ show w
-}


isWarn msg = level msg == Warning
isFail = not . isWarn

mkErr typ lev msg = Msg bogusLoc typ lev msg

mkErrWithLoc l typ lev msg = Msg l typ lev msg

internalErr msg = mkErr Internal Fatal msg

mkMsgs :: Message -> Messages
mkMsgs msg = ([msg], [])

concatMsgs (e1, w1) (e2,w2) = (e1 ++ e2, w1 ++ w2)  -- m1 `mappend` m2

emptyMsgs = ([],[]) --`mempty`

hasErrs (errs,_) = not (null errs)
hasWarns (_,warns) = not (null warns)

hasMsgs ([],[]) = False
hasMsgs _ = True
