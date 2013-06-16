--  Copyright (C) 2013 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
--                     Emmanouil Koukoutos   <manoskouk@softlab.ntua.gr>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Error (
    module Error,
    module Control.Monad.Error
) where

import Pos
import Pretty
import Control.Monad.Error


type ErrDesc = Doc

data Message =
   Msg { 
        errSpan :: PosSpan,
        errtyp  :: ErrType,
        level   :: ErrLevel,
        desc    :: ErrDesc
    }

instance Show Message where show = show.ppr

instance HasPosition Message where
    posSpan (Msg p _ _ _) = p

-- Errors and warnings
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
       | errSpan err == bogusSpan = desc err
       | otherwise                = hang (ppr (errSpan err) <>colon) 4 (desc err)


{-
class Monad m => MonadWarn m w where
    addWarning :: w -> m ()
    addWarning w = fail $ show w
-}

-- Utilities 

isWarn msg = level msg == Warning
isFail = not . isWarn

mkErr typ lev msg = Msg bogusSpan typ lev msg

mkErrWithPos pos typ lev msg = Msg pos typ lev msg

internalErr msg = mkErr Internal Fatal msg

mkMsgs :: Message -> Messages
mkMsgs msg = ([msg], [])

concatMsgs (e1, w1) (e2,w2) = (e1 ++ e2, w1 ++ w2)  -- m1 `mappend` m2

emptyMsgs = ([],[]) --`mempty`

hasErrs (errs,_) = not (null errs)
hasWarns (_,warns) = not (null warns)

hasMsgs ([],[]) = False
hasMsgs _ = True
