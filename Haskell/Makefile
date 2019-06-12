compile:
	ghc *.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f Main
	rm -f temp_*.v
	rm -f temp_*.dot

yosys_flatten $(INPUT) $(OUTPUT):
	yosys -p "hierarchy; proc; opt; fsm; opt; memory; opt; techmap; opt; write_verilog" -o $(OUTPUT) $(INPUT)

remove_comments $(INPUT) $(OUTPUT):
	cat $(INPUT) | perl remove_coment.pl > $(OUTPUT)

run $(INPUT) $(OUTPUT): 
	make compile 
	make yosys_flatten INPUT=$(INPUT) OUTPUT=temp_flat.v
	make remove_comments INPUT=temp_flat.v OUTPUT=temp_flat_no_comments.v
	./Main 0 temp_flat_no_comments.v temp_out.v
	make yosys_flatten INPUT=temp_out.v OUTPUT=temp_out_flat.v
	make remove_comments INPUT=temp_out_flat.v OUTPUT=temp_out_flat_no_comments.v
	./Main 1 temp_out_flat_no_comments.v $(OUTPUT)

image $(INPUT):
	yosys -p "show -stretch -prefix $(INPUT) -format dot" $(INPUT)
