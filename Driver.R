source("Libraries.R")
source("Constants.R")
source("DataInit.R")

overall_means <- list()
best_results <- list()
best_tsnes <- list()
best_pam_plots <- list()
best_hac_plots <- list()
best_mixmod_plots <- list()
best_kamila_plots <- list()
best_kproto_plots <- list()
best_lcm_plots <- list()

pam_time <- 0
kamila_time <- 0
hac_time <- 0
lcm_time <- 0
kproto_time <- 0
mixmod_time <- 0

source("MixmodAnalysis.R")
source("AlternativeKamilaProcessing.R")
source("ExperimentalPam.R")
source("AlternativeHACProcessing.R")
source("KProtoAnalysis.R")
source("LCAAnalysis.R")
#source("LCM.R") # very long running
