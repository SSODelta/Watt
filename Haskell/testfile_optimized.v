module mux_2to1(Y, A, B, sel);
wire   _03_;
wire   _04_;
wire   _05_;
input  [1:0] A;
input  [1:0] B;
output [1:0] Y;
wire   [1:0] _0_;
wire   [1:0] _1_;
wire   [1:0] _2_;
input  sel;
wire   gate_dangle_0;
wire   dangle_0;
assign gate_dangle_0 = (~ dangle_0);
assign dangle_0 = (~ sel);
assign _05_ = dangle_0;
wire   nand_1;
nand_ gate_nand_1 (.a(sel), .b(_0_[1]), .c(nand_1));
assign _03_ = (~ nand_1);
wire   nand_2;
nand_ gate_nand_2 (.a(_05_), .b(_2_[1]), .c(nand_2));
assign _04_ = (~ nand_2);
wire   nor_3;
nor_ gate_nor_3 (.a(_03_), .b(_04_), .c(nor_3));
assign Y[1] = (~ nor_3);
wire   gate_dangle_4;
wire   dangle_4;
assign gate_dangle_4 = (~ dangle_4);
assign dangle_4 = (~ A[0]);
wire   nand_5;
nand_ gate_nand_5 (.a(dangle_4), .b(B[0]), .c(nand_5));
wire   gate_dangle_6;
wire   dangle_6;
assign gate_dangle_6 = (~ dangle_6);
assign dangle_6 = (~ B[0]);
wire   nand_7;
nand_ gate_nand_7 (.a(A[0]), .b(dangle_6), .c(nand_7));
wire   nor_8;
nor_ gate_nor_8 (.a((~ nand_5)), .b((~ nand_7)), .c(nor_8));
assign Y[0] = (~ nor_8);
wire   gate_dangle_9;
wire   dangle_9;
assign gate_dangle_9 = (~ dangle_9);
assign dangle_9 = (~ A[1]);
wire   nand_10;
nand_ gate_nand_10 (.a(dangle_9), .b(B[1]), .c(nand_10));
wire   gate_dangle_11;
wire   dangle_11;
assign gate_dangle_11 = (~ dangle_11);
assign dangle_11 = (~ B[1]);
wire   nand_12;
nand_ gate_nand_12 (.a(A[1]), .b(dangle_11), .c(nand_12));
wire   nor_13;
nor_ gate_nor_13 (.a((~ nand_10)), .b((~ nand_12)), .c(nor_13));
assign _2_[1] = (~ nor_13);
wire   gate_dangle_14;
wire   dangle_14;
assign gate_dangle_14 = (~ dangle_14);
assign dangle_14 = (~ _2_[1]);
wire   nand_15;
nand_ gate_nand_15 (.a(dangle_14), .b(_1_[0]), .c(nand_15));
wire   gate_dangle_16;
wire   dangle_16;
assign gate_dangle_16 = (~ dangle_16);
assign dangle_16 = (~ _1_[0]);
wire   nand_17;
nand_ gate_nand_17 (.a(_2_[1]), .b(dangle_16), .c(nand_17));
wire   nor_18;
nor_ gate_nor_18 (.a((~ nand_15)), .b((~ nand_17)), .c(nor_18));
assign _0_[1] = (~ nor_18);
wire   nand_19;
nand_ gate_nand_19 (.a(A[0]), .b(B[0]), .c(nand_19));
assign _1_[0] = (~ nand_19);
endmodule
module nand_(input a,b,output c); endmodule
module nor_(input a,b,output c); endmodule