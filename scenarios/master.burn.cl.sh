#!/bin/bash

# runs burnin model
qsub -q bf -t 1-25 -v SIMNO=1, runsim.burn.cl.sh

