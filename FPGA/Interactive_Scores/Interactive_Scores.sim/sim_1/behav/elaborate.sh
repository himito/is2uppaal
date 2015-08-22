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
ExecStep $xv_path/bin/xelab -wto 805fd80e6b9848c18407be201346f8f9 -m64 --debug typical --relax -L xil_defaultlib -L unisims_ver -L unimacro_ver -L secureip --snapshot interactive_score_t_behav xil_defaultlib.interactive_score_t xil_defaultlib.glbl -log elaborate.log
