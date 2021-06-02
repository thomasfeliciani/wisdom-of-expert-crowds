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
nRepetitions <- 1 # repetitions per execution
smartParameterSpace <- TRUE

# Setting up the parameter space.
#
# We start with the variables that will vary *between* simulations: 
tqd <- c ("top skew", "bottom skew") #"symmetric bell",
scale <- c(2, 5, 10)
glh <- c(0, 0.05, 0.1)#0:4/20#0:8/40
nSubmissions <- c(100)
nReviewersPerProp <- 2:13
truthNoise <- c(0, 0.2, 0.4) ###########
reviewerCompetence <- c(1, 0.9, 0.8, 0.6)
ruleVariant <- c("none", "gloomy", "sunny")
battery <- expand.grid(
  tqd = tqd,
  scale = scale,
  glh = glh,
  nSubmissions = nSubmissions,
  nReviewersPerProp = nReviewersPerProp,
  truthNoise = truthNoise,
  reviewerError = 1 - reviewerCompetence,
  ruleVariant = ruleVariant
)

# Removing parameter configurations that differ from the baseline from more than
# two dimensions. This considerably reduces the parameter space that needs to 
# be simulated.
# To do this, we first define a baseline parameter configuration:
baseline = c(
  tqd = "top skew",
  scale = 5,
  glh = 0.05,
  nSubmissions = 100,
  nReviewersPerProp = 5,
  truthNoise = 0, ###########
  reviewerError = 0.2,
  ruleVariant = "none"
)

# Then, for each parameter configuration in the data.frame battery, we count
# how many differences there are, and we discard runs that have more than 3
# differences.
if (smartParameterSpace) {
  variables <- c(
    "tqd", "scale", "glh", "nSubmissions", "nReviewersPerProp",
    "truthNoise", "reviewerError", "ruleVariant")
  closeToBaseline <- apply(
    X = battery,
    MARGIN = 1,
    FUN = function(x) {
      nDiff <- sum(x[variables] != baseline) # tally of differences from basel.
      ifelse(nDiff > 3, return(FALSE), return(TRUE))
    }
  )
  battery <- battery[closeToBaseline,]
}
#battery <- battery[51:100,] # This shortens the battery further (for testing).



runBattery <- function(nRepetitions, battery, debug = FALSE){
  
  # Tese are the parameters that vary within the simulation:
  aggrRule <- c(
    "control",
    "mean",
    "excludeExtremes",
    "hypermean",
    "lowestScore",
    "highestScore",
    "median",
    "majorityJudgement",
    "bordaCount"
  )
  nAccepted <- c(5, 10, 20, 50)#c(1:10 * 5)
  
  # Finally, we set some global parameters and initialize the object where
  # results will be stored.
  
  runCounter <- 1
  
  # We create a unique random seed for each simulation run we're going to run:
  randomSeeds <- sample(
    -999999999:999999999,
    size = nrow(battery) * nRepetitions,
    replace = FALSE
  )
  
  # Last bit of preparation work: we prepare the data.structure where we will be
  # saving the results from all the simulations.
  temp = data.frame(seed = rep(NA, nrow(battery) * nRepetitions))
  ri <- list(
    "control" = temp,
    "mean" = temp,
    "excludeExtremes" = temp,
    "hypermean" = temp,
    "lowestScore" = temp,
    "highestScore" = temp,
    "median" = temp,
    "majorityJudgement" = temp,
    "bordaCount" = temp
  )
  rm(temp)
  
  
  # Here we run the actual simulation battery (it can take a long time):
  #for (b in 1:10) { # For all parameter combinations... #######################
  for (b in 1:nrow(battery)) { # For all parameter combinations...
    print(paste0(
      "Running parameter combination ", b,
      " of ", nrow(battery), ". Time: ", Sys.time() 
    ))
    
    for (rep in 1:nRepetitions){ # and for as many times as we need repetitions...
      
      if (debug) print(paste("seed:", randomSeeds[runCounter]))
      
      alpha <- beta <- 1
      if (battery$tqd[b] == "symmetric bell") {
        alpha <- beta <- 3
      }
      if (battery$tqd[b] == "top skew") {
        alpha <- 5
        beta <- 2
      }
      if (battery$tqd[b] == "bottom skew") {
        alpha <- 2
        beta <- 5
      }
      
      # .. we run the simulation according to the specified parameters:
      r <- simulation (
        criteria = cbind.data.frame(
          name    = c("q1"),
          alpha   = alpha,
          beta    = beta,
          scale   = battery$scale[b],
          #gradeLanguage = c("asymmetric"),
          glh     = battery$glh[b],
          weights = c(1)#,
          #debug = debug
        ), 
        nSubmissions = battery$nSubmissions[b],
        nReviewersPerProp = battery$nReviewersPerProp[b],
        nPropPerReviewer = battery$nSubmissions[b],# All reviewers review all
        truthNoise = battery$truthNoise[b],
        reviewerError = battery$reviewerError[b],
        aggrRule = aggrRule,
        ruleVariant = battery$ruleVariant[b],
        nAccepted = nAccepted,
        seed = randomSeeds[runCounter]
      )
      
      # Saving the results to the results list.
      #confusionM <- list()
      for (rule in aggrRule) {
        
        # We save the independent variables:
        df <- data.frame(
          seed = r$parameters$seed,
          timestamp = r$parameters$timestamp,
          aggrRule = rule,
          ruleVariant = r$parameters$ruleVariant,
          alpha = r$parameters$criteria$alpha,
          beta = r$parameters$criteria$beta,
          scale = r$parameters$criteria$scale,
          #gradeLanguage = r$parameters$criteria$gradeLanguage,
          glh = r$parameters$criteria$glh,
          nSubmissions = r$parameters$nSubmissions,
          nReviewersPerProp = r$parameters$nReviewersPerProp,
          nPropPerReviewer = r$parameters$nPropPerReviewer,
          truthNoise = r$parameters$truthNoise,
          reviewerError = r$parameters$reviewerError
        )
        
        # And then we add the outcome variables:
        results <- r$results[[rule]]$outcomeMetrics
        
        df$qualityEff <- results$qualityEfficacy 
        
        for (n in 1:length(r$parameters$nAccepted)){
          df[1,paste0("RankEff",r$parameters$nAccepted[n])] <- 
            results$rankingEfficacy[n]
          
          df[1,paste0("AUC",r$parameters$nAccepted[n])] <- 
            results$auc[n]
          
          df[1,paste0("CohensKappa",r$parameters$nAccepted[n])] <- 
            results$CohensKappa[n]
          
          #df[1,paste0("typeIperf",r$parameters$nAccepted[n])] <- 
          #  results$typeIperf[n]
          #  
          #df[1,paste0("typeIIperf",r$parameters$nAccepted[n])] <- 
          #  results$typeIIperf[n]
          
          #df[1,paste0("KTDtop",r$parameters$nAccepted[n])] <- 
          #  results$ktdTop[n]
          
          #df[1,paste0("spearmanTop",r$parameters$nAccepted[n])] <- 
          #  results$spearmanTop[n]
        }
        df$KTD <- results$ktd
        df$KTC <- results$ktc
        df$Spearman <- results$spearman
        
        # Last, we append df to the results:
        ifelse(
          runCounter == 1,
          {
            df[(nrow(df) + 1):(nrow(battery) * nRepetitions),] <- NA
            ri[[rule]] <- df
          },
          ri[[rule]][runCounter,] <- df
        )
      }
      
      runCounter <- runCounter + 1
    }
  }
  #audio::play(sin(1:20000 / 10)) # gives out scary audio notification :)
  
  
  
  
  # Merging data into one data.frame______________________________________________
  for (d in 1:length(ri)){
    ifelse(d == 1, temp <- ri[[d]], temp <- rbind(temp, ri[[d]]))
  }
  ri <- temp
  rm(temp)
  
  # we transform the normalized Kendall distance into similarity
  ri$kts <- 1 - ri$KTD
  ri$KTD <- NULL
  
  # Recoding some of the variables:
  ri$aggrRule[ri$aggrRule == "bordaCount"] <- "Borda count"
  #ri$aggrRule[ri$aggrRule == "bordaCountExtended"] <- "Borda count ext."
  ri$aggrRule[ri$aggrRule == "excludeExtremes"] <- "trimmed mean"
  ri$aggrRule[ri$aggrRule == "lowestScore"] <- "lowest score"
  ri$aggrRule[ri$aggrRule == "highestScore"] <- "highest score"
  ri$aggrRule[ri$aggrRule == "majorityJudgement"] <- "majority judgment"
  #ri$aggrRule[ri$aggrRule == "control"] <- "null"
  ri$aggrRule[is.na(ri$aggrRule)] <- "null"
  #ri$aggrRule[ri$aggrRule == "null-panel"] <- "null"
  
  ri$tqd <- "uniform"
  ri$tqd[ri$alpha == 2 & ri$beta == 5] <- "bottom skewed"
  ri$tqd[ri$alpha == 5 & ri$beta == 2] <- "top skewed"
  ri$alpha <- ri$beta <- NULL
  ri$competence <- 1 - ri$reviewerError
  
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
) %dopar% runBattery(nRepetitions, battery, debug = TRUE)
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
    "lowest score", "highest score", "Borda count", "control"))
)




#_______________________________________________________________________________
# 
# Calculating parameter-configuration-level
# averages and s.d. of all outcome variables
#_______________________________________________________________________________
#
# We format the data.frame "battery" where each row is a unique parameter
# configuration.
#battery$reviewerCompetence <- 1 - battery$reviewerError
battery <- expand.grid(
  tqd = unique(ri$tqd),
  scale = unique(ri$scale),
  glh = unique(ri$glh),
  nReviewersPerProp = unique(ri$nReviewersPerProp),
  truthNoise = unique(ri$truthNoise),
  reviewerCompetence = 1 - unique(ri$reviewerError),
  reviewerError = unique(ri$reviewerError),
  aggrRule = unique(ri$aggrRule),
  ruleVariant = unique(ri$ruleVariant)
)

# For each unique parameter configuration, we measure the average and s.d.
# of all outcome variables and we add them to the data.frame "battery".
# This can take several minutes.
print("Calculating aggregate statistics.")
enableJIT(1)
pb <- txtProgressBar(max = nrow(battery), style = 3); setTxtProgressBar(pb, 0)
for (i in 1:nrow(battery)){
  x <- subset(
    ri,
    ri$tqd == battery$tqd[i] &
      ri$scale == battery$scale[i] &
      ri$glh == battery$glh[i] &
      ri$nReviewersPerProp == battery$nReviewersPerProp[i] &
      ri$truthNoise == battery$truthNoise[i] &
      ri$reviewerError == battery$reviewerError[i] &
      ri$aggrRule == battery$aggrRule[i] &
      ruleVariant == battery$ruleVariant[i]
  )
  battery$nRuns[i] <- nrow(x)
  for (var in 12:27){ # index of variables in x (see names(x))
    battery[i, paste0(names(x)[var], "_sd")] <- 
      sd(x[, var], na.rm = TRUE)
    battery[i, paste0(names(x)[var], "_mean")] <-
      mean(x[,var], na.rm = TRUE)
  }
  setTxtProgressBar(pb, i)
}
enableJIT(0)
print(paste("Calculation of aggregate statistics completed on", Sys.time()))




# Saving results to file________________________________________________________
save(
  file = "./output/ri.RData",
  ri, battery
)

print(object.size(ri), units="Mb")

