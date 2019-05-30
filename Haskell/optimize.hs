module Optimize where

import Language.Verilog.AST

data RAW = RAW_VAR String
         | RAW_VAR_BIT String Expr
         | RAW_NOT RAW
         | RAW_AND RAW RAW
         | RAW_OR RAW RAW
         | RAW_XOR RAW RAW

data SIMPLE = SIMPLE_VAR String
         | SIMPLE_VAR_BIT String Expr
         | SIMPLE_NOT SIMPLE
         | SIMPLE_AND SIMPLE SIMPLE
         | SIMPLE_OR SIMPLE SIMPLE

data CONSTANT = CONSTANT_VAR String
         | CONSTANT_VAR_BIT String Expr
         | CONSTANT_NOT CONSTANT
         | CONSTANT_NAND CONSTANT CONSTANT
         | CONSTANT_NOR CONSTANT CONSTANT
         | CONSTANT_DANGLE_NOT CONSTANT

optimize :: Module -> Module
optimize (Module moduleName arguments body) =
  (Module moduleName arguments ((optimize_body 0) $ body))

modulize_expr :: CONSTANT -> Integer -> ([ModuleItem],Expr,Integer)
modulize_expr (CONSTANT_VAR a) n = ([], Ident a,n)
modulize_expr (CONSTANT_VAR_BIT a e) n = ([], IdentBit a e,n)
modulize_expr (CONSTANT_NOT a) n =
  let (extra,e,n') = (modulize_expr a n)
  in (extra,UniOp BWNot e,n')
modulize_expr (CONSTANT_NOR a b) n =
  let (extra1,e1,n1) = (modulize_expr a n) in
    let (extra2,e2,n2) = (modulize_expr b n1) in
      let name = "nor_" ++ show n2 in
      (extra1 ++ extra2 ++ [Wire Nothing [(name, Nothing)],
                            Instance "nor_" [] ("gate_" ++ name) [("a",Just e1),("b",Just e2),("c",Just (Ident name))]], Ident name,n2+1)
modulize_expr (CONSTANT_NAND a b) n =
  let (extra1,e1,n1) = (modulize_expr a n) in
    let (extra2,e2,n2) = (modulize_expr b n1) in
      let name = "nand_" ++ show n2 in
      (extra1 ++ extra2 ++ [Wire Nothing [(name, Nothing)],
                            Instance "nand_" [] ("gate_" ++ name) [("a",Just e1),("b",Just e2),("c",Just (Ident name))]], Ident name,n2+1)
modulize_expr (CONSTANT_DANGLE_NOT a) n =
  let (extra,e,n') = (modulize_expr a n) in
  let name = "dangle_" ++ show n'
  in (extra ++ [Wire Nothing [("gate_" ++ name,Nothing)],
                Wire Nothing [(name, Nothing)],
                Assign (LHS ("gate_" ++ name)) (UniOp BWNot (Ident name)),
                Assign (LHS name) e],(Ident name),n'+1)

optimize_body :: Integer -> [ModuleItem] -> [ModuleItem]
optimize_body _ [] = []
optimize_body n ((Assign lhs exp) : r) =
  let (extra,e,n') = modulize_expr (optimize_constant $ constant_power $ optimize_simple $ simplify $ optimize_raw $ normalize_expr exp) n in
  extra ++ Assign lhs e : optimize_body n' r
optimize_body n (a : r) = a : optimize_body n r

simplify :: RAW -> SIMPLE
simplify (RAW_XOR e1 e2) = (\a b -> SIMPLE_OR (SIMPLE_AND (SIMPLE_NOT a) b) (SIMPLE_AND a (SIMPLE_NOT b))) (simplify e1) (simplify e2)
simplify (RAW_VAR s) = SIMPLE_VAR s
simplify (RAW_VAR_BIT s e) = SIMPLE_VAR_BIT s e
simplify (RAW_AND e1 e2) = SIMPLE_AND (simplify e1) (simplify e2)
simplify (RAW_OR e1 e2) = SIMPLE_OR (simplify e1) (simplify e2)
simplify (RAW_NOT e) = SIMPLE_NOT (simplify e)

constant_power :: SIMPLE -> CONSTANT
constant_power (SIMPLE_AND e1 e2) = CONSTANT_NOT (CONSTANT_NAND (constant_power e1) (constant_power e2))
constant_power (SIMPLE_OR e1 e2) = CONSTANT_NOT (CONSTANT_NOR (constant_power e1) (constant_power e2))
constant_power (SIMPLE_NOT e) = CONSTANT_DANGLE_NOT (CONSTANT_NOT (constant_power e))
constant_power (SIMPLE_VAR s) = CONSTANT_VAR s
constant_power (SIMPLE_VAR_BIT s e) = CONSTANT_VAR_BIT s e

optimize_raw :: RAW -> RAW
optimize_raw = id

optimize_simple :: SIMPLE -> SIMPLE
optimize_simple = id

optimize_constant :: CONSTANT -> CONSTANT
optimize_constant = id

normalize_expr :: Expr -> RAW
normalize_expr (Ident s) = RAW_VAR s
normalize_expr (UniOp BWNot e) = RAW_NOT (normalize_expr e)
normalize_expr (BinOp BWAnd e1 e2) = RAW_AND (normalize_expr e1) (normalize_expr e2)
normalize_expr (BinOp BWOr e1 e2) = RAW_OR (normalize_expr e1) (normalize_expr e2)
normalize_expr (BinOp BWXor e1 e2) = RAW_XOR (normalize_expr e1) (normalize_expr e2)

normalize_expr (IdentBit s e) = RAW_VAR_BIT s e
