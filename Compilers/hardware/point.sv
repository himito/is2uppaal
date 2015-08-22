`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI - Université de Bordeaux
// Engineer: Jaime ARIAS
// 
// Create Date: 
// Design Name: point
// Module Name: point
// Project Name: Interactive-Scores
// Target Devices: ZedBoard
// Tool Versions: Vivado 2014.4
// Description: Implementation of a conditioned interaction point.
// 
// Dependencies: None
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

package definitions;
    typedef enum logic [5:0]{LESS       = 6'b000001, 
                            GREATER     = 6'b000010, 
                            LESS_EQ     = 6'b000100, 
                            GREATER_EQ  = 6'b001000, 
                            EQ          = 6'b010000, 
                            TRIGGER     = 6'b100000} op_t;
endpackage

import definitions::*;

module point #(parameter int WIDTH=32)
     (input logic clk, rst, start, event_e, external_e, kill_p, skip_p, urgent, enable,
      input logic [WIDTH-1:0] value_event, value_condition,
      input op_t op_condition,
      output logic skip, event_t, set_enable);
      
     // conditon output
     logic condition_output;
              
     // ----------------- conditional block ----------------------
     always_comb begin: conditional_block
        unique case (op_condition)
            LESS: begin: less_case
                condition_output = (value_event < value_condition);
            end: less_case
            
            LESS_EQ: begin: less_eq_case
                condition_output = (value_event <= value_condition);
            end: less_eq_case
            
            GREATER: begin: greater_case
                condition_output = (value_event > value_condition);
            end: greater_case
            
            GREATER_EQ: begin: greater_eq_case
                condition_output = (value_event >= value_condition);
            end: greater_eq_case
            
            EQ: begin: eq_case
                condition_output = (value_event == value_condition);
            end: eq_case
            
            TRIGGER: begin: trigger_case
                condition_output = 1;
            end: trigger_case
        endcase
     end: conditional_block
                       
     // ------------- state declaration -------------
      enum logic [3:0] {IDLE     = 4'b0001,
                        ENABLED  = 4'b0010,
                        URGENT   = 4'b0100,
                        FINAL    = 4'b1000} state;
      
     // ---------- state + next state logic ----------
    always_ff @(posedge clk, posedge rst) begin: FSM
        if (rst) state <= IDLE;
        else begin: FSM_Sequencer
            unique case (state)
                IDLE: begin: idle_state
                    if (kill_p || skip_p) state <= FINAL;
                    else if (start) state <= ENABLED;
                end: idle_state
               
                ENABLED: begin: enabled_state
                    if (kill_p) state <= FINAL;
                    else if(external_e && condition_output) state <= URGENT;
                    else if (event_e) state <= FINAL;
                end: enabled_state
               
                URGENT: begin: urgent_state // generate a delay
                    state <= FINAL;
                end: urgent_state
               
                FINAL: begin: final_state
                    state <= FINAL;
                end: final_state
            endcase
        end: FSM_Sequencer   
    end: FSM
    
    // --------------- output logic ---------------
    always_comb
    begin: output_logic
        skip = (((state == IDLE) && !(kill_p) && skip_p) || ((state == ENABLED) && event_e && ((!enable) || (!urgent && enable))));
        event_t = ((state == ENABLED) && !kill_p && enable && ((event_e && urgent) || (external_e && condition_output))); 
        set_enable = ((state == ENABLED) && external_e && event_t);
    end: output_logic

endmodule
