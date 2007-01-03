module Err (
        ErrorT(..), throwError, catchError,
        ErrLevel(..), ErrType(..),
        Message, Messages, processMsgs, mkMsgs, concatMsgs, emptyMsgs,
        Context, ErrDesc,
        internalErr, mkErr, mkLocErr,
        hasErrs, hasWarns, hasMsgs
    ) where

import Loc
import Pretty

import Control.Monad.Error
import Control.Monad.State

type ErrDesc = Doc
type Context = Doc

data Message =
   Msg { 
        loc    :: (Maybe Loc),
        errtyp :: ErrType,
        level  :: ErrLevel,
        desc   :: ErrDesc,
        ctxt   :: [Context]
    }

data ErrLevel = 
      Fatal
    | Failure
    | Warning
  deriving Eq

data ErrType = 
      ParseError
    | TypeError
    | Internal

type Messages = ([Message], [Message])

isWarn msg = level msg == Warning
isFail = not.isWarn

mkErr typ lev msg diagn = Msg Nothing typ lev msg diagn

mkLocErr loc typ lev msg diagn = Msg (Just loc) typ lev msg diagn

internalErr msg = mkErr Internal Fatal msg []

processMsgs :: Messages -> [Message]
processMsgs (errs,warns) = (reverse errs) ++ (reverse warns)

mkMsgs :: Message -> Messages
mkMsgs msg = ([msg], [])

concatMsgs (e1, w1) (e2,w2) = (e1 ++ e2, w1 ++ w2)

emptyMsgs = ([],[])

hasErrs (errs,_) = not (null errs)
hasWarns (_,warns) = not (null warns)

hasMsgs ([],[]) = False
hasMsgs _ = True

instance Error Message where
    strMsg str = internalErr (text str)

instance Error Messages where
    strMsg str = mkMsgs $ strMsg str

instance Pretty Message where
    ppr err =
        case loc err of
            Nothing -> vcat [ desc err, ppr_ctxt ]
            Just l  -> hang (ppr l<>colon) 4 (vcat [desc err, ppr_ctxt])
        where ppr_ctxt = vcat $ map ppr (ctxt err)
