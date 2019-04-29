`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date:    23:22:23 08/27/2017 
// Design Name: 
// Module Name:    ALU 
// Project Name: 
// Target Devices: 
// Tool versions: 
// Description: 
//
// Dependencies: 
//
// Revision: 
// Revision 0.01 - File Created
// Additional Comments: 
//
//////////////////////////////////////////////////////////////////////////////////
module ALU(
    input [3:0] a,
    input [3:0] b,
    input [0:0] ctrl,
    output zero,
    output reg [3:0] result
    );
	 
    always @ * begin
	case (ctrl)
	  1'b0: result = a & b;
	  1'b1: result = a | b;
	endcase
    end
	
    assign zero = (result == 0);

endmodule
