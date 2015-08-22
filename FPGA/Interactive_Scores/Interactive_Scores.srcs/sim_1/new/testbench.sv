//////////////////////////////////////////////////////////////////////////////////
// Company: LABRI
// Engineer: Jaime Arias
//
// Create Date: 06.11.2014 19:57:28
// Design Name:
// Module Name: testbench
// Project Name:
// Target Devices:
// Tool Versions:
// Description: Testbench for the timed automaton
//
// Dependencies: rigid_ta.sv
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////

// All times in this testbench are expressed in units of nanoseconds, with
// precision of 1ps increments
`timescale 1ns / 1ps

module testbench();

  // Parameters
  parameter CLOCK_RATE = 200_000_000;
  parameter PERIOD = 1_000_000_000.0 / CLOCK_RATE; // Clock period

  parameter CLOCK_RATE_A = 1_000_000 ;
  parameter PERIOD_A = 1_000_000_000.0 / CLOCK_RATE_A;

  logic clk_FPGA;
  int clk;
  logic rst;

  integer duration = 3;

  // input actions
  logic launch=0;
  logic skip_p=0;
  logic kill_p=0;

  // output action
  logic started, stopped, skipped;

  // Generation of clock
  initial begin
    clk_FPGA = 0;
    forever #(PERIOD/2.0) clk_FPGA = ~clk_FPGA;
  end

  // Generation of clock automaton
  initial begin
    clk = 0;
    forever #(PERIOD_A/2.0) clk += 1;
  end


  // Reset the system
  initial begin
      #500 rst = 1;
      #(PERIOD) rst = 0;
  end

  // Instantiate the DUT
  rigid_ta dut (
    .duration(duration),
    .launch(launch),
    .skip_p(skip_p),
    .kill_p(kill_p),
    .started(started),
    .stopped(stopped),
    .skipped(skipped),
    .clk(clk_FPGA),
    .current_time(clk),
    .reset(rst)
  );

  initial begin
    #1_500 launch = 1;
    #(PERIOD) launch = 0;

    #1_00_000;
    $stop;
    $finish;
  end

endmodule
