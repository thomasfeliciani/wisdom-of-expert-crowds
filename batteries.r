# This script runs a battery of simulations to explore the specified 
# parameter space. Sourcing this script is not advised, as it will take 
# several hours to complete the task.

# Cleaning environment and loading resources:
rm (list = ls( ))
source("simulation.r")
library("compiler")
library("parallel")
library("doSNOW")
#set.seed(12345)

parallelExecutions <- 100
smartParameterSpace <- TRUE

# Setting up the parameter space.
#
# We start with the variables that will vary *between* simulations: 
tqd <- c ("high", "low", "bimodal") #"symmetric bell",
scale <- c(2, 3, 4, 5, 7, 10)
glh <- c(0, 0.05, 0.1)#0:4/20#0:8/40
nSubmissions <- c(100)
nReviewersPerProp <- 2:12
truthNoise <- 0#c(0, 0.2, 0.4)
discreteMerit <- FALSE#c(TRUE, FALSE)
reviewerCompetence <- c(1, 0.9, 0.8, 0.6)
ruleVariant <- "none"#c("none", "gloomy", "sunny")
aggrRule <- c(
  "control",
  "mean",
  "excludeExtremes",
  "hypermean",
  "sunnyMean",
  "gloomyMean",
  "lowestScore",
  "highestScore",
  "median",
  "majorityJudgement",
  "bordaCount"
)
battery <- expand.grid(
  tqd = tqd,
  scale = scale,
  glh = glh,
  nSubmissions = nSubmissions,
  nReviewersPerProp = nReviewersPerProp,
  truthNoise = truthNoise,
  discreteMerit = discreteMerit,
  reviewerError = 1 - reviewerCompetence,
  ruleVariant = ruleVariant,
  aggrRule = aggrRule
)

# Removing parameter configurations that differ from the baseline from more than
# two dimensions. This considerably reduces the parameter space that needs to 
# be simulated.
# To do this, we first define a baseline parameter configuration:
baseline = data.frame(t(c(
  tqd = "high",
  scale = 5,
  glh = 0.05,
  nSubmissions = 100,
  nReviewersPerProp = 5,
  truthNoise = 0,
  discreteMerit = FALSE,
  reviewerError = 0.2,
  ruleVariant = "none",
  aggrRule = "mean"
)))

# Then, for each parameter configuration in the data.frame battery, we count
# how many differences there are, and we discard runs that have more than 3
# differences.
if (smartParameterSpace) {
  variables <- c(
    "tqd", "scale", "glh", "nSubmissions", "nReviewersPerProp",
    "truthNoise", "discreteMerit", "reviewerError", "ruleVariant", "aggrRule")
  #closeToBaseline <- apply(
  battery$distToBaseline <- 0
  for (r in 1:nrow(battery)) {
    battery$distToBaseline[r] <- sum(battery[r,variables] != baseline)
  }
  
  # We also keep this param. config because it's of special interest.
  # This configuration has all grading-scale-related enhancements:
  # High granularity, no GL heterogeneity, and any of the aggregation rules.
  battery$distToBaseline[
    battery$tqd == "high" &
    battery$scale == 10 & ###
    battery$glh == 0 & ###
    battery$nSubmissions == 100 &
    battery$nReviewersPerProp == 5 &
    battery$truthNoise == 0 &
    battery$discreteMerit == FALSE &
    battery$reviewerError == "0.2" &
    battery$ruleVariant == "none"# &
    #battery$aggrRule == "mean" ###
  ] <- 1
  
  battery <- battery[battery$distToBaseline <= 3,]
  battery$distToBaseline <- NULL
}
#x = battery[2223,]
#battery <- battery[51:100,] # This shortens the battery further (for testing).



runBattery <- function(battery, debug = FALSE){
  
  # This are the funding rates for which we measure panel performance:
  nAccepted <- c(5, 10, 20, 50)#c(1:10 * 5)

  
  # We create a unique random seed for each simulation run we're going to run:
  randomSeeds <- sample(
    -999999999:999999999,
    size = nrow(battery),
    replace = FALSE
  )
  
  # Last bit of preparation work: we prepare the data.structure where we will be
  # saving the results from all the simulations.
  ri <- list()

  
  # Here we run the actual simulation battery:
  #for (b in 1:10)  #######################
  for (b in 1:nrow(battery)) { # For all parameter combinations...
    print(paste0(
      "Running parameter combination ", b,
      " of ", nrow(battery), ". Time: ", Sys.time() 
    ))
    
    
    if (debug) print(paste("seed:", randomSeeds[b]))
    
    #alpha <- beta <- 1
    #if (battery$tqd[b] == "symmetric bell") {
    #  alpha <- beta <- 3
    #}
    #if (battery$tqd[b] == "top skew") {
    #  alpha <- 5
    #  beta <- 2
    #}
    #if (battery$tqd[b] == "bottom skew") {
    #  alpha <- 2
    #  beta <- 5
    #}
    
    # .. we run the simulation according to the specified parameters:
    r <- simulation (
      criteria = cbind.data.frame(
        name    = c("q1"),
        implicitScale = as.character(battery$tqd[b]),
        #alpha   = alpha,
        #beta    = beta,
        scale   = battery$scale[b],
        glh     = battery$glh[b],
        weights = c(1)
      ), 
      nSubmissions = battery$nSubmissions[b],
      nReviewersPerProp = battery$nReviewersPerProp[b],
      nPropPerReviewer = battery$nSubmissions[b],# All reviewers review all
      truthNoise = battery$truthNoise[b],
      reviewerError = battery$reviewerError[b],
      reviewerVariability = 0,
      aggrRule = as.character(battery$aggrRule[b]),
      ruleVariant = battery$ruleVariant[b],
      nAccepted = nAccepted,
      discreteMerit = battery$discreteMerit[b],
      seed = randomSeeds[b]
    )
    
    # We save the independent variables:
    df <- data.frame(
      seed = r$parameters$seed,
      batteryConfiguration = b,
      timestamp = r$parameters$timestamp,
      aggrRule = as.character(r$parameters$aggrRule),
      ruleVariant = r$parameters$ruleVariant,
      tqd = as.character(r$parameters$criteria$implicitScale),
      #alpha = r$parameters$criteria$alpha,
      #beta = r$parameters$criteria$beta,
      scale = r$parameters$criteria$scale,
      glh = r$parameters$criteria$glh,
      nSubmissions = r$parameters$nSubmissions,
      nReviewersPerProp = r$parameters$nReviewersPerProp,
      nPropPerReviewer = r$parameters$nPropPerReviewer,
      truthNoise = r$parameters$truthNoise,
      discreteMerit = r$parameters$discreteMerit,
      reviewerError = r$parameters$reviewerError
    )
    
    # And then we add the outcome variables:
    results <- r$results[[r$parameters$aggrRule]]$outcomeMetrics
    
    df$qualityEff <- results$qualityEfficacy 
    
    for (n in 1:length(r$parameters$nAccepted)){
      df[1,paste0("RankEff",r$parameters$nAccepted[n])] <- 
        results$rankingEfficacy[n]
      
      df[1,paste0("AUC",r$parameters$nAccepted[n])] <- 
        results$auc[n]
      
      df[1,paste0("CohensKappa",r$parameters$nAccepted[n])] <- 
        results$CohensKappa[n]
    }
    df$KTD <- results$ktd
    df$KTC <- results$ktc
    df$Spearman <- results$spearman
    
    # we transform the normalized Kendall distance into similarity
    df$kts <- 1 - df$KTD
    df$KTD <- NULL
    
    # Recoding some of the variables:
    df$aggrRule[df$aggrRule == "bordaCount"] <- "Borda count"
    df$aggrRule[df$aggrRule == "excludeExtremes"] <- "trimmed mean"
    df$aggrRule[df$aggrRule == "lowestScore"] <- "lowest score"
    df$aggrRule[df$aggrRule == "highestScore"] <- "highest score"
    df$aggrRule[df$aggrRule == "sunnyMean"] <- "sunny mean"
    df$aggrRule[df$aggrRule == "gloomyMean"] <- "gloomy mean"
    df$aggrRule[df$aggrRule == "majorityJudgement"] <- "majority judgment"
    df$aggrRule[is.na(df$aggrRule)] <- "null"
    
    #df$tqd <- "uniform"
    #df$tqd[df$alpha == 2 & df$beta == 5] <- "bottom skewed"
    #df$tqd[df$alpha == 5 & df$beta == 2] <- "top skewed"
    #df$alpha <- df$beta <- NULL
    df$competence <- 1 - df$reviewerError
    
    
    
    # Last, we append df to the results:
    ifelse(b == 1, ri <- df, ri[b,] <- df)
  }
  #audio::play(sin(1:20000 / 10)) # gives out scary audio notification :)
  
  return(ri)
}




#_______________________________________________________________________________
# 
# Running simulations in parallel.
# This can take several hours.
#_______________________________________________________________________________
#
# runBattery(nRepetitions, debug = TRUE)
# 
print(paste("Simulation battery started on", Sys.time()))
enableJIT(1)
cl <- snow::makeCluster(
  parallel::detectCores() - 2,
  outfile = "./output/log.txt"
)
registerDoSNOW(cl)
pb <- txtProgressBar(max = parallelExecutions, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
ri <- foreach(
  i = 1:parallelExecutions,
  .combine = rbind,
  .options.snow = opts
) %dopar% runBattery(battery, debug = TRUE)
close(pb)
stopCluster(cl)
enableJIT(0)
print(paste("Simulation battery completed on", Sys.time()))



# Recoding..
ri$aggrRule[ri$aggrRule == "highestScore"] <- "highest score"
ri$aggrRule <- factor(
  ri$aggrRule,
  levels = rev(c(
    "median", "mean", "trimmed mean", "hypermean", "majority judgment",
    "lowest score", "highest score", "gloomy mean", "sunny mean",
    "Borda count", "control"))
)




#_______________________________________________________________________________
# 
# Calculating parameter-configuration-level
# averages and s.d. of all outcome variables
#_______________________________________________________________________________
#
battery$reviewerCompetence <- 1 - battery$reviewerError

# For each unique parameter configuration, we measure the average and s.d.
# of all outcome variables and we add them to the data.frame "battery".
# This can take several minutes.
print("Calculating aggregate statistics.")
enableJIT(1)
pb <- txtProgressBar(max = nrow(battery), style = 3); setTxtProgressBar(pb, 0)
for (b in 1:nrow(battery)){
  x <- ri[ri$batteryConfiguration == b,]
  
  battery$nRuns[b] <- nrow(x)
  for (var in 13:28){ # index of variables in x (see names(x))
    battery[b, paste0(names(x)[var], "_sd")] <- 
      sd(x[, var], na.rm = TRUE)
    battery[b, paste0(names(x)[var], "_mean")] <-
      mean(x[,var], na.rm = TRUE)
  }
  setTxtProgressBar(pb, b)
}
enableJIT(0)
print(paste("Calculation of aggregate statistics completed on", Sys.time()))




# Saving results to file________________________________________________________
save(
  file = "./output/ri.RData",
  ri, battery
)

print(object.size(ri), units="Mb")

