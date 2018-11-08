
## Lifecycle

suppressPackageStartupMessages(library(EpiModelHIV))
#rm(list = ls())

library(parallel)
np = detectCores()

load("~/Campcl/scenarios/est/nwstats.rda")

# 1. Main Model -----------------------------------------------------------

# Initialize network
nw.main <- base_nw_msm(st)
x <- nw.main %v% 'deg.main'
y <- nw.main %v% 'deg.pers'
z<-which(nw.main %v% 'asmm'==1)
x[z]<-0
y[z]<-0
nw.main %v% 'deg.main' <-x
nw.main %v% 'deg.pers' <-y

# Assign degree
nw.main <- assign_degree(nw.main, deg.type = "pers", nwstats = st)


# Formulas
formation.m <- ~edges +
  nodefactor("deg.pers") +
  absdiff("sqrt.age") +
  offset(nodefactor("asmm",base=1)) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Fit model
fit.m <- netest(nw.main,
                formation = formation.m,
                coef.form = c(-Inf, -Inf, -Inf),
                target.stats = st$stats.m,
                coef.diss = st$coef.diss.m,
                constraints = ~bd(maxout = 1),
                set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e10,
                                                init.method = "zeros",
                                                MCMLE.maxit = 250))


# 2. Casual Model ---------------------------------------------------------

# Initialize network
nw.pers <- nw.main

# Assign degree
nw.pers <- assign_degree(nw.pers, deg.type = "main", nwstats = st)


# Formulas


formation.p <- ~edges +
  nodefactor("deg.main") +
  concurrent +
  absdiff("sqrt.age") +
  nodefactor("oamsm") +
  offset(nodefactor("asmm",base=1)) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

#control.ergm(init=coef(prev.fit))

# Fit model
fit.p <- netest(nw.pers,
                formation = formation.p,
                coef.form = c(-Inf, -Inf, -Inf),
                target.stats = st$stats.p,
                coef.diss = st$coef.diss.p,
                constraints = ~bd(maxout = 3),
                set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e8,
                                                init.method = "zeros",
                                                MCMLE.maxit = 500))




# 3. ASMM Model ----------------------------------------------------------

# Initialize network
nw.asmm <- nw.main


# Formulas
formation.asmm <- ~edges +
  nodefactor("riskg",base = 3)+
  nodefactor("yamsm", base=1) +
  offset(nodefactor("oamsm",base = 1)) +
  offset(nodematch("yamsm", diff = TRUE, keep = 2)) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
  offset(nodefactor("debuted",base=2))




# Fit model
fit.asmm <- netest(nw.asmm,
                   formation = formation.asmm,
                   coef.form = c(-Inf, -Inf,-Inf, -Inf, -Inf),
                   target.stats = st$stats.asmm,
                   coef.diss = st$coef.diss.asmm,
                   set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e3,
                                                   init.method = "MPLE",
                                                   MCMLE.maxit = 250))

# 4. One-off Model ----------------------------------------------------------

# Initialize network
nw.inst <- nw.main

# Assign degree
nw.inst <- set.vertex.attribute(nw.inst, "deg.main", nw.pers %v% "deg.main")
nw.inst <- set.vertex.attribute(nw.inst, "deg.pers", nw.main %v% "deg.pers")
table(nw.inst %v% "deg.main", nw.inst %v% "deg.pers")

# Formulas
formation.i <- ~edges +
  nodefactor(c("deg.main", "deg.pers"),base=1) +
  nodefactor("riskg", base = 3) +
  absdiff("sqrt.age") +
  offset(nodefactor("asmm",base=1)) +
  offset(nodematch("role.class", diff = TRUE, keep = 1:2))

# Fit model
fit.i <- netest(nw.inst,
                formation = formation.i,
                coef.form = c(-Inf, -Inf, -Inf),
                target.stats = st$stats.i,
                coef.diss = dissolution_coefs(~offset(edges), 1),
                set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e4,
                                                init.method = "MPLE",
                                                MCMLE.maxit = 250))


summary(fit.i)
# Save data
est <- list(fit.m, fit.p, fit.asmm, fit.i)
save(est, file = "~/Campcl/scenarios/est/fit.rda")



# Diagnostics -------------------------------------------------------------
#
# dx <- netdx(est[[3]], nsims = 10000, ncores = 1, dynamic = FALSE)
# dx
