`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI
// Engineer: Jaime Arias
//
// Create Date: 07.11.2014 18:20:33
// Design Name:
// Module Name: interactive_score
// Project Name:
// Target Devices:
// Tool Versions:
// Description:
//
// Dependencies: rigid_ta, clock_ta
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module interactive_score(
    input clk,
    input rst,
    input launch
    );

    // global clock of the model
    logic [31:0] clk_ta;

    // global clock generator (us)
    clock_ta clk_generator (
      .clk(clk),
      .rst(rst),
      .clk_ta(clk_ta)
    );


    // temporal interval 1
    rigid_ta interval_rigid_1 (
      .clk(clk),
      .reset(rst),
      .current_time(clk_ta),
      .duration(10), // 10 us
      .launch(launch),
      .skip_p(),
      .kill_p(),
      .started(),
      .stopped(),
      .skipped()
    );


endmodule
