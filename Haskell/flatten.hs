module Flatten where

import Language.Verilog.AST

flatten :: Module -> Module
flatten (Module moduleName arguments body) = (Module moduleName arguments (replace body))

replace :: [ModuleItem] -> [ModuleItem]
replace [] = []
replace (Assign lhs (BinOp BWXor e1 e2) : r) = (Assign lhs (BinOp BWOr (BinOp BWAnd (UniOp BWNot e1) e2) (BinOp BWAnd e1 (UniOp BWNot e2)))) : replace r
replace (Assign lhs (Mux e1 e2 e3) : r) = (Assign lhs (BinOp BWOr (BinOp BWAnd e1 e2) (BinOp BWAnd (UniOp BWNot e1) e3))) : replace r
replace (a : r) = a : replace r
