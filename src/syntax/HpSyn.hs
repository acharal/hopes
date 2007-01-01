module HpSyn where

{-
    Higher order Prolog abstract syntax
-}

import Loc
import Types
import Char (isUpper)
import Pretty

type HpName = String

data HpSrc = HpSrc {
        tysigs  :: [LHpTySig],
        clauses :: [LHpClaus]
    } deriving Show

data HpStmt   =
      HpCl LHpClaus      -- clause
    | HpTS LHpTySig      -- type signature
  deriving Show

data HpTySig  = HpTySig HpName Type
  deriving Show

data HpClaus  = HpClaus LHpAtom HpBody
  deriving Show

type HpBody   = [LHpAtom]
type HpGoal   = HpBody

data HpAtom   =
      HpAtom LHpExpr
    | HpCut
   deriving Show

data HpExpr   = 
      HpTerm  LHpTerm
    | HpTyExp LHpExpr Type
    | HpPar   LHpExpr
    | HpApp   LHpExpr [LHpExpr]
    | HpPred  HpName
--    | HpLam HpName LHpExpr
  deriving Show

data HpTerm   =
      HpCon  HpName                     -- constant
    | HpVar  HpName                     -- free variable
    | HpId   HpName                     -- not yet know wtf this identifier is
    | HpFun  HpName [LHpTerm]           -- term with function symbol
    | HpList [LHpTerm] (Maybe LHpTerm)  -- list
    | HpSet  [LHpTerm]                  -- relation written as set
    | HpTup  [LHpTerm]                  -- tuple
    | HpWild                            -- wildcard
  deriving Show

data HpType   =
      HpTyGrd HpName                    -- ground type
    | HpTyFun LHpType LHpType           -- type of function
    | HpTyTup [LHpType]                 -- type of tuple
    | HpTyRel LHpType                   -- type of relation / isomorfic to a function type
  deriving Show


type LHpSrc   = Located HpSrc
type LHpStmt  = Located HpStmt
type LHpClaus = Located HpClaus
type LHpTySig = Located HpTySig
type LHpExpr  = Located HpExpr
type LHpAtom  = Located HpAtom
type LHpTerm  = Located HpTerm
type LHpType  = Located HpType
type LHpBody  = Located HpBody

isBound :: HpName -> Bool
isBound = isUpper . head

getVarsT :: HpTerm -> [HpName]
getVarsT (HpVar v) = [v]
getVarsT (HpFun f tl) = concatMap (getVarsT.unLoc) tl
getVarsT (HpList tl mbt) = concatMap (getVarsT.unLoc) tl ++ maybe [] (getVarsT.unLoc) mbt
getVarsT (HpTup tl) = concatMap (getVarsT.unLoc) tl
getVarsT (HpSet tl) = concatMap (getVarsT.unLoc) tl
getVarsT _ = []

getVarsE (HpPar e) = getVarsE (unLoc e)
getVarsE (HpApp e el) = concatMap (getVarsE.unLoc) (e:el)
getVarsE (HpPred p) = [p]
getVarsE (HpTerm t) = getVarsT (unLoc t)
getVarsE (HpTyExp e t) = getVarsE (unLoc e)


getVarsA (HpAtom e) = getVarsE (unLoc e)
getVarsA _ = []

getVars (HpClaus h b) = concatMap (getVarsA.unLoc) (h:b)

getBoundedVars c = filter isBound (getVars c)


getPred :: HpAtom -> Maybe HpName
getPred (HpAtom le) = 
    let getPE (HpApp e _) = getPE (unLoc e)
        getPE (HpPred p)   = Just p
        getPE _            = Nothing
    in  getPE (unLoc le)
getPred _ = Nothing

getHead :: HpClaus -> HpAtom
getHead (HpClaus h b) = unLoc h

{- pretty printing -}

instance Pretty HpExpr where
    ppr (HpTerm t)     = ppr (unLoc t)
    ppr (HpTyExp e ty) = hsep [ ppr (unLoc e), dcolon, ppr ty ]
    ppr (HpPar e)      = parens (ppr (unLoc e))
    ppr (HpPred n)     = text n
    ppr (HpApp e es)   = ppr (unLoc e) <> parens (sep (punctuate comma (map (ppr.unLoc) es)))

instance Pretty HpTerm where
    ppr (HpId name)  = text name
    ppr (HpVar v)  = text "$" <> text v
    ppr (HpCon v)  = text "#" <> text v
    ppr (HpFun f tl) = text f <> parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr  HpWild      = char '_'
    ppr (HpTup tl)   = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpSet tl)   = braces (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpList tl tail) = brackets (sep (punctuate comma (map (ppr.unLoc) tl)))

instance Pretty HpType where
    ppr (HpTyGrd t)  = text t
    ppr (HpTyFun t t') = sep [ ppr (unLoc t) <+> arrow , ppr (unLoc t') ]
    ppr (HpTyTup tl) = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpTyRel t)  = braces (ppr (unLoc t))

instance Pretty HpAtom where
    ppr (HpAtom e) = ppr (unLoc e)
    ppr  HpCut     = char '!'

instance Pretty HpClaus where
    ppr (HpClaus h []) = ppr (unLoc h) <> dot
    ppr (HpClaus h b)  = hang (ppr (unLoc h) <> text ":-") 4 $ 
                            sep (punctuate comma (map (ppr.unLoc) b)) <> char '.'

instance Pretty HpTySig where
    ppr (HpTySig n lt) = hang (text n <+> dcolon) (length n + 4) (ppr lt)

instance Pretty HpSrc where
    ppr src = vcat (map (ppr.unLoc) (tysigs src) ++ map (ppr.unLoc) (clauses src))

instance Pretty HpStmt where
    ppr (HpCl lc)  = ppr (unLoc lc)
    ppr (HpTS lts) = ppr (unLoc lts)
