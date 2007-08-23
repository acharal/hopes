module HpSyn where

{-
    Higher order Prolog abstract syntax
-}

import Loc
import Types
import Char (isUpper)
import Pretty
import List (nub)

type HpName = String

data HpSource = HpSrc {
        tysigs  :: [LHpTySign],
        clauses :: [LHpClause]
    } deriving Show

data HpStmt   =
      HpCl LHpClause      -- clause
    | HpTS LHpTySign      -- type signature
  deriving Show

data HpTySign = HpTySig HpName Type
  deriving Show

data HpClause = HpClaus LHpAtom HpBody
  deriving Show

type HpBody   = [LHpAtom]
type HpGoal   = HpBody

data HpAtom   =
      HpAtom LHpExpr
    | HpCut
   deriving Show

data HpExpr   = 
      HpTerm  LHpTerm
    | HpAnno  LHpExpr Type
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


type LHpSource = Located HpSource
type LHpStmt   = Located HpStmt
type LHpClause = Located HpClause
type LHpTySign = Located HpTySign
type LHpExpr   = Located HpExpr
type LHpAtom   = Located HpAtom
type LHpTerm   = Located HpTerm
type LHpType   = Located HpType
type LHpBody   = Located HpBody
type LHpGoal   = Located HpGoal


hpVarsT (L _ (HpVar v))  = [v]
hpVarsT (L _ (HpTup tl)) = concatMap hpVarsT tl
hpVarsT (L _ (HpSet tl)) = concatMap hpVarsT tl
hpVarsT (L _ (HpFun f tl)) = concatMap hpVarsT tl
hpVarsT (L _ (HpList tl mbt)) = 
    concatMap hpVarsT tl ++ 
    maybe [] hpVarsT mbt
hpVarsT _ = []

hpVarsE (L _ (HpPar e))    = hpVarsE e
hpVarsE (L _ (HpApp e el)) = concatMap hpVarsE (e:el)
hpVarsE (L _ (HpPred p))   = []
hpVarsE (L _ (HpTerm t))   = hpVarsT t
hpVarsE (L _ (HpAnno e t)) = hpVarsE e

hpVarsA (L _ (HpAtom e)) = hpVarsE e
hpVarsA _ = []

hpVarsL = concatMap hpVarsA

hpVarsC c = hpVarsL $ lits c


argsE (L _ (HpApp e args)) = argsE e ++ args
argsE (L _ (HpPar e)) = argsE e
argsE (L _ (HpAnno e t)) = argsE e
argsE (L _ _) = []

argsA (L _ (HpAtom e)) = argsE e
argsA e = []

headE (L _ (HpApp e _))  = headE e
headE (L _ (HpPar e))    = headE e
headE (L _ (HpAnno e t)) = headE e
headE e = e

headA (L _ (HpAtom e)) = headE e
headA e = error ("headA of " ++ show e)

hLit  (L _ (HpClaus h _)) = h
bLits (L _ (HpClaus _ b)) = b
lits c = (hLit c):bLits c

fact c = null (bLits c)

bound :: HpName -> Bool
bound = isUpper . head

-- quantified (bounded) variables of a clause
bvarST = nub.hpVarsT
bvarSE = nub.hpVarsE
bvarSA = nub.hpVarsA
bvarSL = nub.hpVarsL
bvarSC = nub.hpVarsC
bvarS  = bvarSC


{- pretty printing -}

instance Pretty HpExpr where
    ppr (HpTerm t)     = ppr (unLoc t)
    ppr (HpAnno e ty)  = hsep [ ppr (unLoc e), dcolon, ppr ty ]
    ppr (HpPar e)      = parens (ppr (unLoc e))
    ppr (HpPred n)     = text n
    ppr (HpApp e es)   = ppr (unLoc e) <> parens (sep (punctuate comma (map (ppr.unLoc) es)))

instance Pretty HpTerm where
    ppr (HpId name)  = text name
    ppr (HpVar v)    = text v
    ppr (HpCon v)    = text v
    ppr (HpFun f tl) = text f <> parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr  HpWild      = char '_'
    ppr (HpTup tl)   = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpSet tl)   = braces (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpList tl tail) = brackets $ sep $ (punctuate comma (map (ppr.unLoc) tl)) ++ ppr_tail
        where ppr_tail =
                case tail of
                    Nothing -> []
                    Just t  -> [text "|" <+> ppr (unLoc t)]

instance Pretty HpType where
    ppr (HpTyGrd t)    = text t
    ppr (HpTyFun t t') = sep [ ppr (unLoc t) <+> arrow , ppr (unLoc t') ]
    ppr (HpTyTup tl)   = parens (sep (punctuate comma (map (ppr.unLoc) tl)))
    ppr (HpTyRel t)    = braces (ppr (unLoc t))

instance Pretty HpAtom where
    ppr (HpAtom e) = ppr (unLoc e)
    ppr  HpCut     = char '!'

instance Pretty HpClause where
    ppr (HpClaus h []) = ppr (unLoc h) <> dot
    ppr (HpClaus h b)  = hang (ppr (unLoc h) <> entails) 4 $ 
                            sep (punctuate comma (map (ppr.unLoc) b)) <> char '.'

instance Pretty HpTySign where
    ppr (HpTySig n lt) = hang (text n <+> dcolon) (length n + 4) (ppr lt)

instance Pretty HpSource where
    ppr src = vcat (map (ppr.unLoc) (tysigs src) ++ map (ppr.unLoc) (clauses src))

instance Pretty HpStmt where
    ppr (HpCl lc)  = ppr (unLoc lc)
    ppr (HpTS lts) = ppr (unLoc lts)
