module HpSyntax where

import Utils
import ParseUtils

type HpName = String

data HpSrc    = HpSrc [LHpStmt]
  deriving Show

data HpStmt   =
      HpCl LHpClaus      -- clause
    | HpTS LHpTySig      -- type signature
  deriving Show

data HpTySig  = HpTySig HpName LHpType
  deriving Show

data HpClaus  = HpClaus LHpAtom HpBody
  deriving Show

type HpBody   = [LHpAtom]
type HpGoal   = HpBody

data HpAtom   =
      HpAtom HpName [LHpExpr]
    | HpCut
   deriving Show

data HpExpr   = 
      HpTerm  LHpTerm
    | HpTyExp LHpExpr LHpType
    | HpPar   LHpExpr
    | HpApp   LHpTerm LHpExpr
--    | HpLam HpName LHpExpr
  deriving Show

data HpTerm   =
      HpCon  HpName                     -- constant
    | HpBVar HpName                     -- bound variable
    | HpFVar HpName                     -- free variable
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

instance Pretty HpExpr where
    ppr (HpTerm t)     = ppr (unLoc t)
    ppr (HpTyExp e ty) = hcat [ ppr (unLoc e), colon <> colon, ppr (unLoc ty) ]
    ppr (HpPar e)      = parens (ppr (unLoc e))

instance Pretty HpTerm where
    ppr (HpId name)  = text name
    ppr (HpFun f tl) = text f <> parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr  HpWild      = char '_'
    ppr (HpTup tl)   = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpSet tl)   = braces (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpList tl tail) = brackets (sep (punctuate comma (map (ppr.unLoc) tl)))

instance Pretty HpType where
    ppr (HpTyGrd t)  = text t
    ppr (HpTyFun t t') = sep [ ppr (unLoc t), text "->" , ppr (unLoc t') ]
    ppr (HpTyTup tl) = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpTyRel t)  = braces (ppr (unLoc t))

instance Pretty HpAtom where
    ppr (HpAtom a []) = text a
    ppr (HpAtom a tl) = text a <> parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr  HpCut        = char '!'

instance Pretty HpClaus where
    ppr (HpClaus h []) = ppr (unLoc h) <> char '.'
    ppr (HpClaus h b)  = hang (ppr (unLoc h) <> text ":-") 4 $ 
                            sep (punctuate comma (map (ppr.unLoc) b)) <> char '.'

instance Pretty HpTySig where
    ppr (HpTySig n lt) = text n <+> colon <> colon <+> ppr (unLoc lt)

instance Pretty HpSrc where
    ppr (HpSrc lt) = vcat (map (ppr.unLoc) lt)

instance Pretty HpStmt where
    ppr (HpCl lc)  = ppr (unLoc lc)
    ppr (HpTS lts) = ppr (unLoc lts)

getName :: Located Token -> HpName
getName (L _ (TKid x)) = x
getName _ = error "not a valid token"
