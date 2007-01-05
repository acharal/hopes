module HpSyn where

{-
    Higher order Prolog abstract syntax
-}

import Loc
import Types
import Char (isUpper)
import Pretty

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

isBound :: HpName -> Bool
isBound = isUpper . head


varsTerm (L _ (HpVar v))  = [v]
varsTerm (L _ (HpTup tl)) = concatMap varsTerm tl
varsTerm (L _ (HpSet tl)) = concatMap varsTerm tl
varsTerm (L _ (HpFun f tl)) = concatMap varsTerm tl
varsTerm (L _ (HpList tl mbt)) = 
    concatMap varsTerm tl ++ 
    maybe [] varsTerm mbt
varsTerm _ = []

varsExpr (L _ (HpPar e))    = varsExpr e
varsExpr (L _ (HpApp e el)) = concatMap varsExpr (e:el)
varsExpr (L _ (HpPred p))   = [p]
varsExpr (L _ (HpTerm t))   = varsTerm t
varsExpr (L _ (HpAnno e t)) = varsExpr e


varsAtom (L _ (HpAtom e)) = varsExpr e
varsAtom _ = []

varsClause (L _ (HpClaus h b)) = concatMap (varsAtom) (h:b)

argsAtom (L _ (HpAtom e)) = go e
    where go (L _ (HpApp e args)) = go e ++ args
          go (L _ (HpPred _)) = []
          go e = [e]

boundVarSet c = filter isBound (varsClause c)

predAtom a =
    case getPred a of
        Nothing -> error ("not expected")
        Just p -> p

getPred :: LHpAtom -> Maybe HpName
getPred (L _ (HpAtom le)) = 
    let getPE (HpApp e _) = getPE (unLoc e)
        getPE (HpPred p)   = Just p
        getPE _            = Nothing
    in  getPE (unLoc le)
getPred _ = Nothing

headC :: LHpClause -> LHpAtom
headC (L _ (HpClaus h b)) = h

bodyC :: LHpClause -> [LHpAtom]
bodyC (L _ (HpClaus h b)) = b

isFactC :: LHpClause -> Bool
isFactC cl = null (bodyC cl)

{- pretty printing -}

instance Pretty HpExpr where
    ppr (HpTerm t)     = ppr (unLoc t)
    ppr (HpAnno e ty) = hsep [ ppr (unLoc e), dcolon, ppr ty ]
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
