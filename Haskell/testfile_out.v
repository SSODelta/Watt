module mux_2to1(Y, A, B, sel);
wire   [1:0] _0_;
wire   [1:0] _1_;
wire   [1:0] _2_;
input  [1:0] A;
input  [1:0] B;
output [1:0] Y;
input  sel;
assign Y[1] = ((sel & _0_[1]) | ((~ sel) & _2_[1]));
assign Y[0] = (A[0] ^ B[0]);
assign _2_[1] = (A[1] ^ B[1]);
assign _0_[1] = (_2_[1] ^ _1_[0]);
assign _1_[0] = (A[0] & B[0]);
endmodule
