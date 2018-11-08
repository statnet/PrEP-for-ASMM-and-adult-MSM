
## Packages
library("methods")
suppressMessages(library("EpiModelHIV"))
library("EpiModelHPC")

## Environmental Arguments
args <- commandArgs(trailingOnly = TRUE)
simno <- args[1]
jobno <- args[2]

# ## Parameters
fsimno <- paste(simno, jobno, sep = ".")
load("est/nwstats.rda")

param <- param_cl(nwstats = st,
                  testing.pattern = "interval",
                  
                  
                  prep.start = Inf,
                  prep.elig.model = "cdc3",
                  prep.coverage.adol.naive = .4,
                  prep.coverage.adol.exp = .4,
                  
                  prep.start.asmm = Inf,
                  prep.elig.model.asmm = "adol.AI.older.time",
                  prep.coverage.b.asmm = .2,
                  prep.coverage.w.asmm = .2,

                  ai.pers.scale = 1.3661,
                  ai.asmm.scale = 11.11735,
                  cond.asmm.BB.prob = .2201,
                  cond.asmm.BW.prob = .2201,
                  cond.asmm.WW.prob = .2201)

init <- init_cl(st)

control <- control_msm(simno = fsimno,
                       start = 6001,
                       nsteps = (52 * 45),
                       nsims = 8,
                       ncores = 8,
                       save.other = c("attr" , "riskhist"),
                       initialize.FUN = reinit_msm)

## Simulation
netsim_hpc("est/p2.burnin.rda", param, init, control, verbose = FALSE)

#process_simfiles(simno = simno, min.n = 8, outdir = "data/")
