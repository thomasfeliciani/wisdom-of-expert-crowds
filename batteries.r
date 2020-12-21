# This script runs a battery of simulations to explore the specified 
# parameter space. Sourcing this script is not advised, as it will take 
# several hours to complete the task.

# Cleaning environment and loading resources:
rm (list = ls( ))
source("simulation.r")
library(compiler)
#set.seed(12345)


# Setting up the parameter space.
#
# We start with the variables that will vary *between* simulations: 
tqd <- c ("top skew", "bottom skew") #"symmetric bell",
scale <- c(2,5,10,20)
glh <- c(0, 0.05, 0.1, 0.2)#0:4/20#0:8/40
nSubmissions <- c(100)
nReviewersPerProp <- 2:13
reviewerCompetence <- c(1, 0.9, 0.8, 0.6)
battery <- expand.grid(
  tqd = tqd,
  scale = scale,
  glh = glh,
  nSubmissions = nSubmissions,
  nReviewersPerProp = nReviewersPerProp,
  reviewerError = 1 - reviewerCompetence
)

# Then, the parameters that vary within the simulation:
aggrRule <- c(
  "control",
  "mean",
  "hypermean",
  "lowestScore",
  "median",
  "majorityJudgement",
  "bordaCount"
)
nAccepted <- c(10,20,30,40,50)

# Finally, we set some global parameters and initialize the object where
# results will be stored.

# This is the number of independent repetitions per condition:
nRepetitions <- 200
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
  "hypermean" = temp,
  "lowestScore" = temp,
  "median" = temp,
  "majorityJudgement" = temp,
  "bordaCount" = temp
)
rm(temp)


enableJIT(1)

# Here we run the actual simulation battery (it can take a long time):
for (b in 1:nrow(battery)) { # For all parameter combinations...
  print(paste0(
    "Running parameter combination ", b,
    " of ", nrow(battery), ". Time: ", Sys.time() 
  ))
  
  for (rep in 1:nRepetitions){ # and for as many times as we need repetitions...
    
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
        gradeLanguage = c("asymmetric"),
        glh     = battery$glh[b],
        weights = c(1)
      ), 
      nSubmissions = battery$nSubmissions[b],
      nReviewersPerProp = battery$nReviewersPerProp[b],
      nPropPerReviewer = battery$nSubmissions[b],# All reviewers review all prop
      reviewerError = battery$reviewerError[b],
      aggrRule = aggrRule,
      nAccepted = nAccepted,
      seed = randomSeeds[runCounter]
    )
    
    # Saving the results to the results list.
    for (rule in aggrRule) {
      
      # We save the independent variables:
      df <- data.frame(
        seed = r$parameters$seed,
        timestamp = r$parameters$timestamp,
        aggrRule = rule,
        alpha = r$parameters$criteria$alpha,
        beta = r$parameters$criteria$beta,
        scale = r$parameters$criteria$scale,
        gradeLanguage = r$parameters$criteria$gradeLanguage,
        glh = r$parameters$criteria$glh,
        nSubmissions = r$parameters$nSubmissions,
        nReviewersPerProp = r$parameters$nReviewersPerProp,
        nPropPerReviewer = r$parameters$nPropPerReviewer,
        reviewerError = r$parameters$reviewerError
      )
      
      # And then we add the outcome variables:
      results <- r$results[[rule]]$outcomeMetrics
      
      df$qualityEff <- results$qualityEfficacy 
      
      for (n in 1:length(r$parameters$nAccepted)){
        df[1,paste0("RankEff",r$parameters$nAccepted[n])] <- 
          results$rankingEfficacy[n]
        
        df[1,paste0("KTDtop",r$parameters$nAccepted[n])] <- 
          results$ktdTop[n]
        
        df[1,paste0("spearmanTop",r$parameters$nAccepted[n])] <- 
          results$spearmanTop[n]
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
enableJIT(0)



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
#ri$aggrRule[ri$aggrRule == "excludeExtremes"] <- "mean excl.extremes"
ri$aggrRule[ri$aggrRule == "lowestScore"] <- "lowest score"
ri$aggrRule[ri$aggrRule == "majorityJudgement"] <- "majority judgment"
ri$aggrRule[ri$aggrRule == "control"] <- "null"
ri$aggrRule[is.na(ri$aggrRule)] <- "null"
#ri$aggrRule[ri$aggrRule == "null-panel"] <- "null"

ri$tqd <- "uniform"
ri$tqd[ri$alpha == 2 & ri$beta == 5] <- "bottom skewed"
ri$tqd[ri$alpha == 5 & ri$beta == 2] <- "top skewed"
ri$alpha <- ri$beta <- NULL
ri$competence <- 1 - ri$reviewerError


# Saving results to file________________________________________________________

save(
  file = "./output/ri.RData",
  ri
)

print(object.size(ri), units="Mb")

