module Builtin where

import Unify
import Subst
import HopesIO
import Core

-- arith
is :: Builtin Subst
is = do
  res  <- getArg 0
  expr <- getArg 1
  expr_res <- evalNumExpr expr
  unify res (CNumber (Left expr_res))

evalNumExpr (CNumber (Left n)) = return n
evalNumExpr (CApp _ (CConst "+") [e1,e2]) = do
  n1 <- evalNumExpr e1
  n2 <- evalNumExpr e2
  return $ n1 + n2
evalNumExpr _ = fail ""

--rel_leq
--rel_geq
--rel_gt
--rel_lt


-- var
--


-- system
-- op
-- halt
-- include
