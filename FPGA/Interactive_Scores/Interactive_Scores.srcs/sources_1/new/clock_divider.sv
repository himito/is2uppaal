`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI
// Engineer: Jaime Arias
//
// Create Date: 07.11.2014 12:19:12
// Design Name:
// Module Name: clock_divider
// Project Name: Interactive Scores
// Target Devices:
// Tool Versions:
// Description: Clock divider
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module clock_divider(
    input clk,      // Input clock
    input rst,      // reset
    input int max,      // max counter (frequency_clock / new_frequency)
    output out_clk // Output clock
    );

    logic [31:0] counter;

    always_ff @(posedge clk, posedge rst)
    begin
      if (rst) counter <= 0;
      else if (counter == max - 1) counter <= 0;
      else counter <= counter + 1;
    end

    assign out_clk = (counter >= max / 2);

endmodule
