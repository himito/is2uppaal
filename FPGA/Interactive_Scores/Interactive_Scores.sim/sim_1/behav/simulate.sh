#!/bin/sh -f
xv_path="/opt/Xilinx/Vivado/2014.3.1"
ExecStep()
{
"$@"
RETVAL=$?
if [ $RETVAL -ne 0 ]
then
exit $RETVAL
fi
}
ExecStep $xv_path/bin/xsim interactive_score_t_behav -key {Behavioral:sim_1:Functional:interactive_score_t} -tclbatch interactive_score_t.tcl -log simulate.log
