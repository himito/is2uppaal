`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI - Université de Bordeaux
// Engineer: Jaime Arias
//
// Create Date:
// Design Name: controller_intervals
// Module Name: controller_intervals
// Project Name: Interactive-Scores
// Target Devices: ZedBoard
// Tool Versions: Vivado 2014.4
// Description: Implementation of a controller for handling many temporal intervals
//
// Dependencies: None
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module controller_intervals #(parameter int WIDTH=32)
    (input logic clk, rst, event_min, event_finished, skip_p, kill_p,
     input logic [WIDTH-1:0] number_r,
     output logic skip, synchronized);

    // local variables
    logic [WIDTH-1:0] counter;
    logic skip_value;

    // ------------- state declaration -------------
    enum logic [2:0] {IDLE  = 3'b001,
                      ERROR = 3'b010,
                      FINAL = 3'b100} state;

    // ---------- state + next state logic ----------
    always_ff @(posedge clk, posedge rst) begin: FSM
        if (rst) begin
            state <= IDLE;
            counter = 1;
            skip_value = 1;
        end
        else begin: FSM_Sequencer
            unique case (state)
                IDLE: begin: idle_state
                    if (kill_p) state <= FINAL;
                    else if ((counter < number_r) && (event_min || skip_p)) begin
                        if (event_min) skip_value = 0;
                        counter = counter + 1;
                        state <= IDLE;
                    end
                    else if ((counter == number_r) && (event_min || skip_p)) state <= FINAL;
                    else if (event_finished) state <= ERROR;
                end: idle_state

                ERROR: begin: error_state
                    state <= ERROR;
                end: error_state

                FINAL: begin: final_state
                    state <= FINAL;
                end: final_state
            endcase
        end: FSM_Sequencer
    end: FSM

     // --------------- output logic ---------------
     always_comb
     begin: output_logic
        skip = ((state == IDLE)&& (counter == number_r)  && skip_p && skip_value);
        synchronized = ((state == IDLE) && (counter == number_r) && (event_min || (skip_p && !skip_value)));
     end: output_logic

endmodule
