module CNF where

import Language.Verilog.AST

data CNF = VAR String
         | NOT CNF
         | AND CNF CNF
         | OR CNF CNF

optimize :: Module -> Module
optimize (Module moduleName arguments body) =
  (Module moduleName arguments (modulize . flatten_list . normalize $ body))

all_names :: CNF -> String
all_names (VAR s) = s
all_names (NOT cnf) = "n_" ++ all_names cnf
all_names (OR a b) = "or_" ++ all_names a ++ "_" ++ all_names b
all_names (AND a b) = "and_" ++ all_names a ++ "_" ++ all_names b

modulize_expr :: CNF -> Expr
modulize_expr (VAR a) = Ident a
modulize_expr (NOT a) = UniOp BWNot (modulize_expr a)
modulize_expr (OR a b) = BinOp BWOr (modulize_expr a) (modulize_expr b)
modulize_expr (AND a b) = BinOp BWAnd (modulize_expr a) (modulize_expr b)

modulize :: CNF -> [ModuleItem]
modulize (OR a b) = [Assign (LHS $ all_names (OR a b)) (BinOp BWOr (modulize_expr a) (modulize_expr b))]
-- modulize (OR a b) = [Assign (LHS $ all_names $ OR a b) (BinOp BWOr (modulize a) (modulize b))]
modulize (AND a b) = modulize a ++ modulize b

flatten_list :: [CNF] -> CNF
flatten_list [a] = a
flatten_list (a : r) = (AND a (flatten_list r))

normalize :: [ModuleItem] -> [CNF]
normalize [] = []
normalize ((Assign (LHS s) exp) : r) = OR (VAR s) (normalize_expr exp) : normalize r
normalize (a : r) = normalize r

normalize_expr :: Expr -> CNF
normalize_expr (Ident s) = VAR s
normalize_expr (UniOp BWNot e) = NOT (normalize_expr e)
normalize_expr (BinOp BWAnd e1 e2) = AND (normalize_expr e1) (normalize_expr e2)
normalize_expr (BinOp BWOr e1 e2) = OR (normalize_expr e1) (normalize_expr e2)

normalize_expr (IdentBit s e) = VAR s
