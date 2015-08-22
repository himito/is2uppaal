`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 07.11.2014 18:32:56
// Design Name:
// Module Name: interactive_score_t
// Project Name:
// Target Devices:
// Tool Versions:
// Description:
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module interactive_score_t();

  // Parameters
  parameter PERIOD = 10.0;  // Clock period 100Mhz (10 ns)

  logic clk, rst, launch;

  interactive_score is (
    .clk (clk),
    .rst (rst),
    .launch (launch)
  );

  // Generation of clock
  initial begin
    clk = 0;
    forever #(PERIOD/2.0) clk = ~clk;
  end

  // Reset the system
  initial begin
      #500 rst = 1;
      #(PERIOD) rst = 0;
  end

  initial begin
    launch = 0;
    #1_500 launch = 1;
    #(PERIOD) launch = 0;

    #1_000_000;
    $stop;
    $finish;
  end


endmodule
