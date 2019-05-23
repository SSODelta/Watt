module VerilogParse where

import Language.Verilog.Parser
import Language.Verilog.Parser.Lex
import Language.Verilog.Parser.Tokens
import Language.Verilog.AST

print_tokens = putStrLn . show . alexScanTokens
print_tokenStrings = putStrLn . (foldr (\a b -> a ++ " " ++ b) "") . (map tokenString) . alexScanTokens

fileParse in_path out_path computation = readFile in_path >>= writeFile out_path . foldr (\a b -> show a ++ b) "" . map computation . (parseFile [] in_path)
fileParsePrint in_path out_path computation = readFile in_path >>= print_tokenStrings . foldr (\a b -> show a ++ b) "" . map computation . (parseFile [] in_path)
