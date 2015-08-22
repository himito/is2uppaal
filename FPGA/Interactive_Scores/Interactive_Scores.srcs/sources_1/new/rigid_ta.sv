`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: LaBRI
// Engineer: Jaime Arias Almeida
//
// Create Date: 03.11.2014 13:41:30
// Design Name:
// Module Name: rigid_ta
// Project Name:
// Target Devices:
// Tool Versions:
// Description: Implementation of the automaton for rigid intervals
//
// Dependencies:
//
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
//
//////////////////////////////////////////////////////////////////////////////////


module rigid_ta(
    input logic clk,                          // clock FPGA
    input logic reset,                         // reset
    input logic [31:0] current_time,          // current_time
    input int duration,                       // duration
    input logic launch, skip_p, kill_p,       // input actions
    output logic started, stopped, skipped   // output actions
    );

    // Definition of states
    typedef enum logic [5:0] {IDLE, START, WAIT, STOP, SKIP, KILL} state_t;
    state_t state;

    // local clock;
    logic [31:0] clock_t;

     // Automaton
    always_ff @(posedge clk, posedge reset)
    begin: automaton
        if (reset) begin
            started <= 0;
            stopped <= 0;
            skipped <= 0;
            clock_t <= current_time;
            state <= IDLE;
        end
        else begin: automaton_sequence
            unique case (state)

            IDLE: begin: idle_state
                started <= 0;
                stopped <= 0;
                skipped <= 0;
                if (kill_p) begin
                    state <= KILL;
                end
                else if (launch) begin
                    clock_t <= current_time;
                    state <= START;
                end
                else if (skip_p) begin
                    state <= SKIP;
                end
            end: idle_state

            START: begin: start_commited_state
                stopped <= 0;
                skipped <= 0;
                started <= 1;
                state <= WAIT;
            end: start_commited_state

            WAIT: begin: wait_state
                started <= 0;
                stopped <= 0;
                skipped <= 0;
                if (kill_p) begin
                    state <= KILL;
                end
                else if ((current_time - clock_t) == duration) begin
                    stopped <= 1;
                    state <= STOP;
                end
            end: wait_state

            STOP: begin: stop_final_state
                started <= 0;
                stopped <= 0;
                skipped <= 0;
                state <= STOP;
            end: stop_final_state

            SKIP: begin: skip_commited_state
                started <= 0;
                stopped <= 0;
                skipped <= 1;
                state <= STOP;
            end: skip_commited_state

            KILL: begin: kill_commited_state
                started <= 0;
                skipped <= 0;
                stopped <= 1;
                state <= STOP;
            end: kill_commited_state

            default: state <= IDLE;
            endcase

        end: automaton_sequence
    end: automaton

endmodule
