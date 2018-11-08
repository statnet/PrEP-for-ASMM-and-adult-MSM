
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
                  
                  FU = TRUE,
                  prep.start = 8261,
                  prep.elig.model = "cdc3",
                  prep.coverage.adol.naive = .4,
                  prep.coverage.adol.exp = .4,
                  
                  prep.start.asmm = 8261,
                  prep.elig.model.asmm = "adol.AI.older.time",
                  prep.coverage.asmm = .2,

                  ai.pers.scale = 1.2316111,
                  ai.asmm.scale = 6.2866832,
                  cond.asmm.BB.prob = 0.2338672,
                  cond.asmm.BW.prob = 0.2338672,
                  cond.asmm.WW.prob = 0.2338672)

init <- init_cl(st)

control <- control_cl(simno = fsimno,
                       start = 8001,
                       nsteps = 8000 + (52 * 45),
                       nsims = 3,
                       ncores = 8,
                       resim.int = 4,
                       save.int = 100, 
                       verbose.int = 10,
                       save.network = FALSE,
                       save.other = c("attr" , "age.inf.vec", "riskh", "riskhist"),
                       initialize.FUN = reinit_msm)

## Simulation
netsim_hpc("est/p2.burnin.rda", param, init, control, verbose = FALSE)

#process_simfiles(simno = simno, min.n = 8, outdir = "data/")
