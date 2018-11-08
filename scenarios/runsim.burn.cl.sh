#!/bin/bash

### User specs
#PBS -N burnin
#PBS -l nodes=1:ppn=16,mem=50gb,feature=16core,walltime=12:00:00:00
#PBS -o /gscratch/csde/deven/Camp/scenarios/Campcl/out
#PBS -e /gscratch/csde/deven/Camp/scenarios/Campcl/out
#PBS -j oe
#PBS -d /gscratch/csde/deven/Camp/scenarios/Campcl
#PBS -m n



### Standard specs
HYAK_NPE=$(wc -l < $PBS_NODEFILE)
HYAK_NNODES=$(uniq $PBS_NODEFILE | wc -l )
HYAK_TPN=$((HYAK_NPE/HYAK_NNODES))
NODEMEM=`grep MemTotal /proc/meminfo | awk '{print $2}'`
NODEFREE=$((NODEMEM-2097152))
MEMPERTASK=$((NODEFREE/HYAK_TPN))
ulimit -v $MEMPERTASK
export MX_RCACHE=0

### Modules
module load r_3.2.4

### App
ALLARGS="${SIMNO} ${PBS_ARRAYID}"
echo runsim variables: $ALLARGS
echo


### App
Rscript sim.burn.cl.R ${ALLARGS}