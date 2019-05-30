import System.Environment
import Flatten
import VerilogParse
import Optimize

process_input "0" = flatten
process_input "1" = optimize

show_file "0" = foldr (\a b -> show a ++ b) ""
show_file "1" = (\k -> (foldr (\a b -> show a ++ b) "" k) ++ "module nand_(input a,b,output c); endmodule\nmodule nor_(input a,b,output c); endmodule")

main = getArgs >>= \args -> fileParse (args!!1) (args!!2) (process_input (args!!0)) (show_file (args!!0))
