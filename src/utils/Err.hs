module Err (
        ErrorT(..), throwError, catchError,
        ErrLevel(..), ErrType(..),
        Message, Messages, processMsgs, mkMsgs, concatMsgs, emptyMsgs,
        Context, ErrDesc,
        internalErr, mkErr, mkErrWithLoc,
        hasErrs, hasWarns, hasMsgs
    ) where

import Loc
import Pretty

import Control.Monad.Error


type ErrDesc = Doc
type Context = Doc

data Message =
   Msg { 
        errloc :: (Maybe Loc),
        errtyp :: ErrType,
        level  :: ErrLevel,
        desc   :: ErrDesc,
        ctxt   :: [Context]
    }

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

instance HasLocation Message where
    loc m =
        case errloc m of
            Nothing -> bogusLoc
            Just l  -> l

instance Error Message where
    strMsg str = internalErr (text str)

instance Error Messages where
    strMsg str = mkMsgs $ strMsg str

{-
class Monad m => MonadWarn m w where
    addWarning :: w -> m ()
    addWarning w = fail $ show w
-}


isWarn msg = level msg == Warning
isFail = not.isWarn

mkErr typ lev msg diagn = Msg Nothing typ lev msg diagn

mkErrWithLoc l typ lev msg diagn = Msg (Just l) typ lev msg diagn

internalErr msg = mkErr Internal Fatal msg []

processMsgs :: Messages -> [Message]
processMsgs (errs,warns) = (reverse errs) ++ (reverse warns)

mkMsgs :: Message -> Messages
mkMsgs msg = ([msg], [])

concatMsgs (e1, w1) (e2,w2) = (e1 ++ e2, w1 ++ w2)  -- m1 `mappend` m2

emptyMsgs = ([],[]) --`mempty`

hasErrs (errs,_) = not (null errs)
hasWarns (_,warns) = not (null warns)

hasMsgs ([],[]) = False
hasMsgs _ = True


instance Pretty Message where
    ppr err =
        case errloc err of
            Nothing -> vcat [ desc err, ppr_ctxt ]
            Just l  -> hang (ppr l<>colon) 4 (vcat [desc err, ppr_ctxt])
        where ppr_ctxt = vcat $ map ppr (ctxt err)
