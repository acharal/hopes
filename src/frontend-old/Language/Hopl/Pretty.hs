--  Copyright (C) 2006-2012 Angelos Charalambidis <a.charalambidis@di.uoa.gr>
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

module Language.Hopl.Pretty () where


import Pretty
import Language.Hopl
import Lang
import Language.Hopl.Syntax.Pretty ()


-- hopl core


instance (Pretty a, Eq a, Symbol a, HasLogicConstants (Expr a), HasSignature (Expr a) a) => Pretty (Expr a) where
    ppr a =  pprPrec1 v' 1 a
        where {- v x = case lookup x vlist of
                      Nothing -> ppr x
                      Just n  -> text n
              vlist = zip (vars a) varnames
              letters = [ "X", "Y", "Z", "A", "B", "C", "D", "E", "F", "H", "K", "L" ]
              varnames = letters ++ [ x ++ (show i) | x <- letters, i <- [1..]] -}
              v' x = ppr x


pprPrec1 f p (Flex sym)    = (f sym)
pprPrec1 f p (Rigid sym)   = ppr sym
pprPrec1 f p e@(App e1 e2) =
    let fu = functor e
    in if (fu == ceq) then
            (sep (punctuate (text "=") (map (pprPrec1 f  p) $ args e)))
       else if fu == cand then
            (sep (punctuate comma (map (pprPrec1 f p) $ args e)))
       else if fu == cor then
            (sep (punctuate semi (map (pprPrec1 f p) $ args e)))
       else if fu == (Rigid (liftSym ".")) then
            pprList f p e
       else 
            (pprPrec1 f p (functor e)) <> parens (sep (punctuate comma (map (pprPrec1 f p) $ args e)))

pprPrec1 f p (Lambda a e) =
    text "\\" <> (f a) <> text "." <+> (pprPrec1 f p e)

pprList f p (Rigid x) = ppr x
pprList f p e = 
    let listelem (App (App (Rigid x) e1) e2) = if (x == liftSym ".") then e1:(listelem e2) else []
	listelem _ = []
	listtail (App (App (Rigid x) _) e2) = if (x == liftSym ".") then listtail e2 else Nothing
        listtail e1@(Flex x) = Just e1
	listtail (Rigid x) = Nothing
        xs = listelem e
	t = listtail e
    in case t of 
	Nothing -> brackets (sep (punctuate comma (map (pprPrec1 f p) xs)))
	Just e' -> brackets (sep (punctuate comma (map (pprPrec1 f p) xs)) <> text "|" <> (pprPrec1 f p e'))

instance (Pretty a, Eq a, Symbol a,  HasLogicConstants (Expr a), HasSignature (Expr a) a) => Pretty (Clause a) where
    ppr (C h b) = hang ((ppr h) <+> text "<:=") 4 $ ppr b

instance (Pretty a, Eq a, Symbol a,  HasLogicConstants (Expr a), HasSignature (Expr a) a) => Pretty (KnowledgeBase a) where
    ppr a = vcat $ map ppr (clauses a)



{-
instance Pretty a => Pretty (Expr a) where
    ppr (Flex  sym)        = ppr sym
    ppr (Rigid sym)        = ppr sym
    ppr (Set es vs)        = curly  $ sep $ (punctuate comma (map ppr es)) ++ map (\v -> text "|" <+> ppr v) vs
    ppr (Tup es)           = parens $ sep $ (punctuate comma (map ppr es))
    ppr (App e e'@(Tup _)) = ppr e <> ppr e'
    ppr (App e e')         = ppr e <> parens (ppr e')

instance Pretty a => Pretty (Clause a) where
    ppr (h,b) = hang (sep [ppr h, entails]) 4 (sep (punctuate comma (map ppr  b)))

instance Pretty a => Pretty (Goal a) where
    ppr g = text "-?" <+> sep (punctuate comma (map ppr g))


instance Pretty a => Pretty (KB.KnowledgeBase a) where
    ppr p = vcat $ map ppr (KB.clauses p)
-}


isbs (Rigid x) s = x == s
isbs _ _ = False

praPrec n p@(Lambda _ _) = ppr p --pprBasicSet p --ppr p
praPrec n p = ppr p

pprBasicSet e = 
   let getbindings (Lambda x e) = x:(getbindings e)
       getbindings e = []
       getbody (Lambda x e) = getbody e
       getbody e = e

       pprElemsList e = 
         let xs = getbindings e
             bd = getbody e
	 in case bd of 
             (App (App x y) e') -> if x == cor then ((pprElem xs y):(pprElemsList (removeapp (reverse xs) e'))) else if (xs == []) then [ppr bd] else error ""
             e -> [ppr e]

       removeapp (x:xs) (App e (Flex y)) = if (x == y) then removeapp xs e else error ""
       removeapp [] e = e

       splitand e@(App (App x e1) e2) = if (x == cand) then e1:(splitand e2) else [e]
       splitand e = [e]

       pprElem xs e = if length pes == 1 then sep (punctuate comma pes) else parens (sep (punctuate comma pes))
	     where es = map spliteq (splitand e)
                   pes = map (ppr.snd) es
       spliteq (App (App x e1) e2) = if (x == ceq) then (e1, e2) else error ""
       spliteq e = (undefined, e)
       pprElems e = pprElemsList e
   in curly (sep (punctuate comma (pprElems e)))

