module Subst where

import Hopl
import Pretty
import Data.Monoid (mconcat)

type Subst a = [ (a, Expr a) ]

success :: Subst a
success = []

bind :: a -> Expr a -> Subst a
bind v e = [(v, e)]


isTaut :: Eq a => (a, Expr a) -> Bool
isTaut (v, Flex v') = v == v'
isTaut _ = False

subst theta (App e1 e2) = 
    App (subst theta e1) (subst theta e2)
subst theta e@(Flex a) = 
    case lookup a theta of
        Nothing -> e
        Just e' -> e'
subst theta (Set es vs) =
    let aux x = case lookup x theta of
                    Nothing -> ([], [x])
                    Just e -> case e of
                                Set es' vs' -> (es', vs')
                                Flex v -> ([], [v])
                                _ -> error "No idea how to substitute that"
        (es', vs') = mconcat (map aux vs)
    in  Set ((map (subst theta) es) ++ es') vs'


subst theta (Tup es) = Tup (map (subst theta) es)
subst theta e = e

restrict :: Eq a => [a] -> Subst a -> Subst a
restrict ss xs = [ (v, e) | (v, e) <- xs, v `elem` ss ]

compose :: Eq a => Subst a -> Subst a -> Subst a
compose theta zeta = [ (v, e') | (v, e) <- theta, let e' = subst zeta e, not (isTaut (v,e')) ]

combine theta zeta  = 
    let ss    = map fst theta
        zeta' = [ (v, e) | (v, e) <- zeta, v `notElem` ss ]
    in  compose theta zeta ++ zeta'


instance Pretty a => Pretty (Subst a) where
    ppr xs = vcat $ map ppr_bind xs
        where ppr_bind (v,t) = sep [ ppr v <+> text "=", ppr t ]
