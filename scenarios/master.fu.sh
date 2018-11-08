#!/bin/bash



qsub -q bf -t 1-10 -v SIMNO=1, runsim.cl.fu.sh
qsub -q bf -t 1-10 -v SIMNO=2, runsim.cl.fu.sh

#qsub -q bf -t 1-40 -v SIMNO=5, runsim.cl.fu.sh
qsub -q bf -t 1-40 -v SIMNO=3, runsim.cl.fu.sh
qsub -q bf -t 1-40 -v SIMNO=4, runsim.cl.fu.sh
#qsub -q bf -t 1-40 -v SIMNO=6, runsim.cl.fu.sh





