
## Packages
library("methods")
library("EpiModelHIV")
library("EpiModelHPC")


#sourceDir("source/", FALSE)

## Environmental Arguments
args <- commandArgs(trailingOnly = TRUE)
simno <- args[1]
jobno <- args[2]
fsimno <- paste(simno, jobno, sep = ".")
print(fsimno)

## Parameters
load("est/nwstats.rda")

# Base model
param <- param_cl(nwstats = st,
                  FU =FALSE,
                  riskh.start = Inf,
                  prep.start.asmm = Inf,
                  prep.start = Inf,
                  ai.pers.scale = 1.2316111,
                  ai.asmm.scale = 6.2866832,
                  cond.asmm.BB.prob = 0.2338672,
                  cond.asmm.BW.prob = 0.2338672,
                  cond.asmm.WW.prob = 0.2338672)

init <- init_cl(st,
                prev.B = 0.18,
                prev.W = 0.18,
                prev.asmm = 0.045)


control <- control_cl(simno = fsimno,
                      nsims = 3,
                      ncores = 8,
                      nsteps = 8000,
                      start = 1,
                      save.other = c("attr","el","temp", "p"),
                      verbose = FALSE)

## Simulation
## Simulation
netsim_hpc("est/fit.rda", param, init, control, save.min=TRUE, save.max=TRUE, compress = "gzip")

#process_simfiles(simno = simno, min.n = njobs, compress = TRUE,
#                 outdir = "data/", verbose = FALSE)
