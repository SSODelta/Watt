/* Generated by Yosys 0.7 (git sha1 61f6811, gcc 6.2.0-11ubuntu1 -O2 -fdebug-prefix-map=/build/yosys-OIL3SR/yosys-0.7=. -fstack-protector-strong -fPIC -Os) */


module mux_2to1(Y, A, B, sel);
  
  wire [1:0] _0_;
  
  wire [1:0] _1_;
  wire [1:0] _2_;
  
  input [1:0] A;
  
  input [1:0] B;
  
  output [1:0] Y;
  
  input sel;
  assign Y[1] = sel ?  _0_[1] : _2_[1];
  assign Y[0] = A[0] ^  B[0];
  assign _2_[1] = A[1] ^  B[1];
  assign _0_[1] = _2_[1] ^  _1_[0];
  assign _1_[0] = A[0] &  B[0];
endmodule