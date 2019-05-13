module mux_2to1(Y, A, B, sel);
   output [1:0] Y;
   input [1:0]  A, B;
   input 	 sel;
   reg [1:0] 	 Y;
   always @(A or B or sel)
     if (sel == 1'b0)
       Y = A ^ B;
     else
       Y = A + B;
endmodule
