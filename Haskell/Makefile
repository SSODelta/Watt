compile:
	ghc *.hs

clean:
	rm -f *.hi
	rm -f *.o
	rm -f Main
	rm -f testfile_*.v
	rm -f testfile_*.dot

yosys_flatten $(INPUT) $(OUTPUT):
	yosys -p "hierarchy; proc; opt; fsm; opt; memory; opt; techmap; opt; write_verilog" -o $(OUTPUT) $(INPUT)

remove_comments $(INPUT) $(OUTPUT):
	cat $(INPUT) | perl remove_coment.pl > $(OUTPUT)

run: 
	make compile 
	make yosys_flatten INPUT=testfile.v OUTPUT=testfile_flat.v
	make remove_comments INPUT=testfile_flat.v OUTPUT=testfile_flat_no_comments.v
	./Main 0 testfile_flat_no_comments.v testfile_out.v
	make yosys_flatten INPUT=testfile_out.v OUTPUT=testfile_out_flat.v
	make remove_comments INPUT=testfile_out_flat.v OUTPUT=testfile_out_flat_no_comments.v
	./Main 1 testfile_out_flat_no_comments.v testfile_optimized.v

image $(INPUT):
	yosys -p "prep; show -stretch -prefix $(INPUT) -format dot" $(INPUT)
