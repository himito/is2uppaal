`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI - Université de Bordeaux
// Engineer: Jaime ARIAS
//
// Create Date:
// Design Name: flexible_interval
// Module Name: flexible_interval
// Project Name: Interactive-Scores
// Target Devices: ZedBoard
// Tool Versions: Vivado 2014.4
// Description: Implementation of a (semi) flexible interval
//
// Dependencies: None
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module flexible_interval #(parameter int WIDTH=32)
    (input logic clk, rst, start, is_finite, external_e, skip_p, kill_p,
     input logic [WIDTH-1:0] global_clock, min_duration, max_duration,
     output logic min_elapsed, finished, skip, kill);

     // local clock
     logic [WIDTH-1:0] local_clock;

     // ------------- state declaration -------------
     enum logic [3:0] {IDLE     = 4'b0001,
                       WAIT_MIN = 4'b0010,
                       WAIT_END = 4'b0100,
                       FINAL    = 4'b1000} state;

     // ---------- state + next state logic ----------
     always_ff @(posedge clk, posedge rst) begin: FSM
        if (rst) state <= IDLE;
        else begin: FSM_Sequencer
            unique case (state)
                IDLE: begin: idle_state
                    if (kill_p || skip_p) state <= FINAL;
                    else if (start) begin
                        local_clock = global_clock;
                        state <= WAIT_MIN;
                    end
                end: idle_state

                WAIT_MIN: begin: wait_min_state
                    if (kill_p) state <= FINAL;
                    else if ((global_clock - local_clock) == min_duration) state <= WAIT_END;
                end: wait_min_state

                WAIT_END: begin: wait_end_state
                    if (kill_p || external_e ||
                    (is_finite && ((global_clock - local_clock) == max_duration))) state <= FINAL;
                end: wait_end_state

                FINAL: begin: final_state
                    state <= FINAL;
                end: final_state
            endcase
        end: FSM_Sequencer
     end: FSM

     // --------------- output logic ---------------
     always_comb
     begin: output_logic
        min_elapsed = ((state == WAIT_MIN) && ((global_clock - local_clock) == min_duration) && !kill_p);
        finished = ((state == WAIT_END) && !kill_p && (external_e || (is_finite && ((global_clock - local_clock) == max_duration))));
        skip = ((state == IDLE) && skip_p && !kill_p);
        kill = (((state == IDLE) || (state == WAIT_MIN) || (state == WAIT_END)) && kill_p);
     end: output_logic

endmodule
