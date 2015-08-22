`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company:
// Engineer:
//
// Create Date: 07.11.2014 14:16:13
// Design Name:
// Module Name: clock_divider_t
// Project Name:
// Target Devices:
// Tool Versions:
// Description: clock_divider testbench
//
// Dependencies: clock_divider
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module clock_divider_t();

    parameter CLOCK_RATE = 50_000_000; // 50 Mhz
    parameter PERIOD = 1_000_000_000.0 / CLOCK_RATE;

    parameter TA_CLOCK_RATE = 1_000_000; // 1 Mhz
    parameter TA_PERIOD = 1_000_000_000.0 / TA_CLOCK_RATE;

    logic clk;
    logic rst;
    int max = TA_PERIOD / PERIOD;
    logic out_clk;


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


    clock_divider clock_1Mhz (
      .clk(clk),
      .rst(rst),
      .max(max),
      .out_clk(out_clk)
    );

endmodule
