`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI - Université de Bordeaux
// Engineer: Jaime ARIAS
// 
// Create Date: 
// Design Name: multimedia-process
// Module Name: multimedia-process
// Project Name: Interactive Scores
// Target Devices: ZedBoard
// Tool Versions: Vivado 2014.4
// Description: Implementation of a multimedia process
// 
// Dependencies: None
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

package definitions_multimedia;
    localparam int WIDTH = 32;
    
    typedef struct packed {
        logic [WIDTH-1:0] value;
        logic [WIDTH-1:0] offset;
    } parameter_t;
    
endpackage

import definitions_multimedia::*;

module multimedia_process #(parameter int WIDTH=32, parameter int SIZE_PROCESS=1)
    (input logic clk, rst, start, stop, skip_p, kill_p,
     input logic [WIDTH-1:0] global_clock,
     input parameter_t [SIZE_PROCESS-1:0] process, // multimedia process
     output logic send,
     output logic [WIDTH-1:0] data);

     // local clock
     logic [WIDTH-1:0] local_clock;
     
     // local variable
     logic [WIDTH-1:0] index;
     
     // ------------- state declaration -------------
     enum logic [2:0] {IDLE     = 3'b001,
                       WAIT     = 3'b010,
                       FINAL    = 3'b100} state;
                       
    // ---------- state + next state logic ----------
    always_ff @(posedge clk, posedge rst) begin: FSM
        if (rst) state <= IDLE;
        else begin: FSM_Sequencer
            unique case (state)
                IDLE: begin: idle_case
                    if (kill_p || skip_p) state <= FINAL;
                    else if (start) begin
                        local_clock = global_clock;
                        index = 0;
                        state <= WAIT;
                    end
                end: idle_case
                
                WAIT: begin: wait_case
                    if (kill_p || stop) state <= FINAL;
                    else if ((global_clock-local_clock) == process[index].offset) begin
                        if (index == (SIZE_PROCESS-1)) state <= FINAL;
                        else begin // index < SIZE_PROCESS-1
                            local_clock = global_clock;
                            index = index + 1;
                            state <= WAIT;
                        end
                    end
                end: wait_case
                
                FINAL: begin: final_case
                    state <= FINAL;
                end: final_case
            endcase
        end: FSM_Sequencer
    end: FSM

     // --------------- output logic ---------------
     always_comb
     begin: output_logic
        send = ((state == WAIT) && !kill_p && ((global_clock-local_clock)==process[index].offset));
        data = process[index].value;
     end: output_logic

endmodule
