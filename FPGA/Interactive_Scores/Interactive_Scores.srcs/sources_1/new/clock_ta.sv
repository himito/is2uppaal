`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI
// Engineer: Jaime Arias
//
// Create Date: 07.11.2014 17:59:23
// Design Name:
// Module Name: clock_ta
// Project Name:
// Target Devices:
// Tool Versions:
// Description:
//
// Dependencies: clock_divider
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module clock_ta(
    input clk,
    input rst,
    output logic [31:0] clk_ta
    );

    logic div_clk;

    clock_divider divider (
      .clk(clk),
      .rst(rst),
      .max(100), // 1us(1Mhz)/10ns (100Mhz) = 100 divided into 2 (50%)
      .out_clk(div_clk)
    );

    always_ff @(posedge div_clk, posedge rst)
    begin
      if (rst) clk_ta <= 0;
      else clk_ta <= clk_ta + 1;
    end
endmodule
