`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI - Université de Bordeaux
// Engineer: Jaime ARIAS
//
// Create Date:
// Design Name: rigid_interval.sv
// Module Name: rigid_interval
// Project Name: Interactive-Scores
// Target Devices: ZedBoard
// Tool Versions: VIvado 2014.4
// Description: Implementation of a rigid interval.
//
// Dependencies: None
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module rigid_interval #(parameter int WIDTH=32)
    (input logic clk, rst, start, skip_p, kill_p,
    input logic [WIDTH-1:0] global_clock, duration,
    output logic min_elapsed, max_elapsed, skip, kill);

    // local clock
    logic [WIDTH-1:0] local_clock;

    // ------------- state declaration -------------
    enum logic [3:0] {IDLE   = 4'b0001,
                      WAIT   = 4'b0010,
                      FINAL  = 4'b0100,
                      URGENT = 4'b1000} state;

    // ---------- state + next state logic ----------
    always_ff @(posedge clk, posedge rst) begin: FSM
         if (rst) state <= IDLE;
         else begin: FSM_Sequencer
            unique case (state)
                IDLE: begin: idle_state
                    if (kill_p || skip_p) state <= FINAL; // priority channel
                    else if (start) begin
                        local_clock = global_clock;
                        state <= WAIT;
                    end
                end: idle_state

                WAIT: begin: wait_state
                    if (kill_p) state <= FINAL; // priority channel
                    else if ((global_clock - local_clock) == duration) state <= URGENT;
                end: wait_state

                URGENT: begin: urgent_state // a delay of one clock
                    state <= FINAL;
                end: urgent_state

                FINAL: begin: final_state
                    state <= FINAL;
                end: final_state
            endcase;
         end: FSM_Sequencer
     end: FSM

     // --------------- output logic ---------------
     always_comb
     begin: output_logic
        min_elapsed = ((state == WAIT) && ((global_clock - local_clock) == duration) && !kill_p);
        max_elapsed = (state == URGENT);
        skip        = ((state == IDLE) && skip_p && !kill_p);
        kill        = (((state == IDLE) || (state == WAIT)) && kill_p);
     end: output_logic

endmodule
