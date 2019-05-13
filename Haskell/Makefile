compile:
	ghc *.hs

clean:
	rm *.hi
	rm *.o
	rm verilog
	testfile.dot

yosys_flatten:
	yosys -p "hierarchy; proc; opt; fsm; opt; memory; opt; techmap; opt; write_verilog" -o testfile_flat.v testfile.v

remove_comments:
	cat testfile_flat.v | perl remove_coment.pl > perl_out_file.v

yosys_out_flatten:
	yosys -p "hierarchy; proc; opt; fsm; opt; memory; opt; techmap; opt; write_verilog" -o testfile_out_flat.v testfile_out.v

run:
	yosys_flatten
	./verilog
	yosys_out_flatten


image:
	yosys -p "prep; show -stretch -prefix testfile_flat.v -format dot" testfile_flat.v
