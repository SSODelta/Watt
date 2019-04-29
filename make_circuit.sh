#!/bin/bash
yosys -o ALU.v ALU.sv synth.ys
dot -Tpng show.dot > output.png
