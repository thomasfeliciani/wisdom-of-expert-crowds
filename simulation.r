# This script defines the main simulation function.
# At the end of the script there is some example code showing how to run it.

# Loading auxiliary functions:
source("./util.r")
library("irr", include.only = "kappa2")
suppressMessages(library("AUC", include.only = c("roc", "auc")))

# The most important argument in this function is the criteria data.frame,
simulation <- function (
  criteria = cbind.data.frame(
    name    = c("q1"),    # name of the criterion
    implicitScale = "high", # or "low" or "bimodal"
    #alpha   = c(3),       # alpha (parameter in the beta distribution)
    #beta    = c(3),       # beta  (parameter in the beta distribution)
    scale   = c(5),       # scale (expressed as number of categories)
    glh     = c(0.1),     # grade language heterogeneity
    weights = c(1)        # relative weight of the criteria 
  ),
  nSubmissions = 20,
  nReviewersPerProp = 5,
  nPropPerReviewer = 3, # max
  truthNoise = 0.2,
  reviewerError = 0.2, # reviewer competence is defined as 1 - reviewerError
  reviewerVariability = 0,
  reviewerBias = 0,
  criteriaWeightsError = 0,
  aggrRule = c(
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
    "bordaCount"),
  ruleVariant = "none",# "gloomy" "sunny"
  nAccepted = c(5, 10),
  discreteMerit = FALSE,
  seed = runif(1, -999999999, 999999999),
  debug = FALSE
)
{
  # Setting random seed:
  set.seed(seed)
  if (any(nAccepted > nSubmissions)){warning(
    "Parameter nAccepted cannot be higher than nSubmissions.")}
  
  # Saving the parameter configuration into an object:
  parameters <- list(
    criteria = criteria,
    nSubmissions = nSubmissions,
    nReviewersPerProp = nReviewersPerProp,
    nPropPerReviewer = nPropPerReviewer,
    truthNoise = truthNoise,
    reviewerError = reviewerError,
    aggrRule = aggrRule,
    ruleVariant = ruleVariant,
    nAccepted = nAccepted,
    discreteMerit = discreteMerit,
    seed = seed,
    timestamp = as.character(Sys.time())
  )
  if (debug) print(parameters)
  #nEvaluationCriteria <- nrow(criteria)
  
  # Creating submissions. We fill in the true quality for each attribute,
  # drawing from the true quality distribution specified in the dataframe
  # "criteria":
  if (criteria$implicitScale == "high") referenceCat <- rbeta(nSubmissions,5,2)
  if (criteria$implicitScale == "extreme high 1") referenceCat <-
    rbeta(nSubmissions,5,1)
  if (criteria$implicitScale == "extreme high 2") referenceCat <-
    rbeta(nSubmissions,10,1)
  if (criteria$implicitScale == "extreme high 3") referenceCat <-
    rbeta(nSubmissions,20,1)
  if (criteria$implicitScale == "low") referenceCat <- rbeta(nSubmissions, 5, 2)
  if (criteria$implicitScale == "extreme low") referenceCat <-
    rbeta(nSubmissions,1,10)
  if (criteria$implicitScale == "extreme center") referenceCat <-
    rbeta(nSubmissions,100,100)
  if (criteria$implicitScale == "extreme bimodal") referenceCat <-
    rbeta(nSubmissions,0.6,0.6)
  if (criteria$implicitScale == "bimodal") {
    referenceCat <- apply(
      cbind(
        rbeta(nSubmissions, 5, 2),
        rbeta(nSubmissions, 2, 5)
      ),
      MARGIN = 1,
      FUN = mean
    )
  }
  
  submissions <- data.frame(
    referenceCat = referenceCat
  )
  if(discreteMerit) submissions$referenceCat <-
    round(submissions$referenceCat * 20) / 20
  
  submissions$refQuality <- truncate(rnorm(
    n = nSubmissions, mean = submissions$referenceCat, sd = truthNoise
  ))
  if(discreteMerit) submissions$refQuality <-
    truncate(round(submissions$refQuality * 20) / 20)
  
  # Based on their referenceCat, we calculate what grade they deserve in the
  # specified grading language:
  thresholds <- qbeta(
    1:(criteria$scale - 1) / criteria$scale,
    shape1 = 2, shape2 = 1
  )
  
  # There might be noise in the determination of the correct grade, too, since
  # the rating task might not be defined.
  # Note how the reference panel uses a finer-grained evaluation scale (one with
  # 100 categories) than the field panel (which only uses criteria$scale many).
  submissions$refGrade <- findInterval(
    submissions$refQuality,
    qbeta(
      1:99 / 100,
      shape1 = 2, shape2 = 1
    )
    #thresholds
  )
  
  # Next, we calculate, for each submission, what would be the true ranking
  # position of the submissions. We assume a strict ordering for now, so we
  # break all ties that occur.
  submissions$refRanking <- rank(
    #1 - submissions$trueGrade,
    1 - submissions$referenceCat, # "1 -" allows to get low ranking number when
    na.last = "keep",            # the true quality is high.
    ties.method = "first"
  )
  submissions$refRanking <- rank(
    1 - submissions$refGrade, #use refQuality if ref. precision is infinite
    na.last = "keep",
    ties.method = "first"
  )
  
  # Now it's time to call in the reviewers, of which we create as many as we
  # need.
  # We start by creating a review network, where rows are reviewers and columns
  # are proposals:
  rnw <- allocationNetwork(
    nSubmissions = nSubmissions,
    nReviewersPerProp = nReviewersPerProp,
    nPropPerReviewer = nPropPerReviewer
  )
  
  # The review network tells us exactly how many reviewers we need:
  nReviewers <- nrow(rnw)
  
  # Next, we give reviewers the attributes they need - for now, they are given
  # two parameters which govern the amount of error and bias they have in
  # their judgment.
  # Also, for now we are assuming that they have the no bias when evaluating
  # different criteria.
  reviewers <- data.frame(
    id = 1:nReviewers,
    #error = rep(reviewerError, times = nReviewers),
    error = truncate(rnorm(
      n = nReviewers, mean = reviewerError, sd = reviewerVariability)),
    bias = rep(reviewerBias, times = nReviewers)
  )
  #if(reviewerBias == "random") {
  #  reviewers$bias <- NULL
  #  reviewers$bias <- runif(min = -0.2, max = 0.2, n = nReviewers)
  #}
  
  # Next, we give each reviewer a grade language interpretation - that is, a
  # vector of thresholds that determine how the continuous evaluation scale
  # is to be discretized.
  gradeLanguages <- list()
  for (i in 1:nReviewers){
    gl <- truncate(rnorm(
      n = criteria$scale - 1,
      mean = thresholds,
      sd = criteria$glh
    ))
    #gl <- runif(n = criteria$scale - 1)###############################
    gradeLanguages[[i]] <- gl[order(gl)]
  }
  
  
  # Now we have all we need for reviewers to rate the submissions.
  # Each reviewer is going to rate the submission she's assigned to, giving a
  # score for each of the attributes, expressed in the correct scale as
  # specified in the criteria dataframe.
  #
  # So, we start by defining the objects where we will store the ratings:
  grades <- matrix(NA, nrow = nSubmissions, ncol = nReviewers)
  
  # For each reviewer and its of its submissions:
  for(rev in 1:nReviewers) {for(prop in which(rnw[rev,] == 1)) {
    
    # Determine the grade:
    grades[prop,rev] <- rate(
      evaluatedAttribute = # Fix in case of combinatorial aggregation
        submissions$referenceCat[prop] + reviewers$bias[rev],
      sd = reviewers$error[rev],
      rule = "quality",
      thresholds = gradeLanguages[[rev]],
      categories = criteria$scale # To fix in case of combinatorial aggregation
    )
  }}
  
  
  # In case we have a "gloomy" or a "sunny" panel, we accordingly remove from
  # each proposal the most positive/negative half of the grades it got.
  if(ruleVariant != "none" & nReviewers > 2) {
    ifelse(
      ruleVariant == "gloomy",
      grades <- gloomyfy(grades),
      grades <- sunnyfy(grades)
    )
  }
  
  # All is left to do is to aggregate.
  #
  # We create a list, where each item will pertain to the results of one of the
  # specified aggregation rules.
  r <- list()
  
  # Then, for each of the aggregation rules...
  for(rule in 1:length(aggrRule)){
    if (debug) print(aggrRule[rule])
    
    
    # ... we calculate the aggregate score for each proposal (x) ...
    if (
      aggrRule[rule] != "majorityJudgement" &
      aggrRule[rule] != "bordaCount" &
      aggrRule[rule] != "control" 
    ) {
      x <- aggregate(
        scores = grades,
        rule = aggrRule[rule]#,
        #weights = criteria$weights,
        #reviewers = reviewers
      )
      x <- x / (criteria$scale - 1) # Normalizing scores to range btw. 0 and 1
    }
    
    if (aggrRule[rule] == "control"){
      sampleReviewer <- sample(1:nReviewers, size = 1)
      x <- c()
      for (prop in 1:nSubmissions){
        x[prop] <- rate(
          evaluatedAttribute =
            submissions$referenceCat[prop] + reviewers$bias[sampleReviewer],
          sd = reviewers$error[sampleReviewer],
          rule = "quality",
          thresholds = gradeLanguages[[sampleReviewer]],
          categories = criteria$scale
        )
      }
      x <- x / (criteria$scale - 1)
    }
    
    if (aggrRule[rule] == "hypermean"){
      x <- hypermean(grades, dampingOutliers = TRUE)
      x <- x / (criteria$scale - 1)
    }
      
    if (aggrRule[rule] == "majorityJudgement"){
      x <- calcMajorityJudgement(grades)$majorityValue
      
      # We find the normalization term, which is the majorityValue that a
      #proposal could get if all its reviewers gave it the highest grade:
      magnitude <- 10 ^ (floor(log10(criteria$scale)) + 1)
      f <- (nReviewersPerProp:1) - 1
      norm <- rep(criteria$scale, times = nReviewersPerProp)
      for (c in 1:nReviewersPerProp){
        norm[c] <- norm[c] * (magnitude ^ f[c])
      }
      norm <- sum(norm)
      
      # Then we normalize:
      x <- x / norm
    }
    
    if (aggrRule[rule] == "bordaCount"){
      x <- modifiedBordaCount_extended(grades)$bordaCount
      x <- x / ((nSubmissions - 1) * nReviewersPerProp)# normalizing
    }
    
    
    # Then we calculate the resulting ranking position.
    # Note that what we rank is "1 / x". In this way, the ranking we obtain is
    # such that low ranking positions (e.g. 1st or 2nd) signify high aggregated
    # scores.
    if(aggrRule[rule] == "majorityJudgement"){
      g <- calcMajorityJudgement(grades)$majorityJudgment
    } else {
      g <- x
    }
    
    xr  <- rank(1 / g,  na.last = "keep", ties.method = "first")
    
    
    ## Here we calculate, for each value of nAccepted, which proposals have a
    ## referenceCat level worthy of acceptance, and which an estimQuality level
    ## that would get them accepted. We store everything in a dedicated list.
    #tqDeserved <- estimated <- list()
    #for (a in 1:length(nAccepted)){
    #  
    #  # Deserving proposals (deserved == TRUE) are here defined as the proposals
    #  # whose objective grade is equal to or higher than the true quality of the
    #  # proposals on the cutoff threshold.
    #  tqDeserved[[a]] <- submissions$trueGrade >=
    #    submissions[submissions$refRanking == nAccepted[a],]$trueGrade
    #  
    #  # Estimated is set to TRUE for all proposals whose estimated quality is 
    #  # equal to or greater than the estimated quality of the proposal on the 
    #  # cutoff threshold:
    #  estimated[[a]] <- x >= x[xr == nAccepted[a]]
    #}
    
    
    # Last, we calculate the outcome measures.
    # For every outcome variable we take two measurements:
    # - one that compares the panel grades / ranking with the true grades and 
    #   rankings;
    # - one that compares the panel grades / rankings with the grade/rankings 
    #   that an infallible panel would make (i.e. a panel with no reviewer
    #   error and 0 grade language heterogeneity).
    # The former allows to compare between aggregation rules; our main outcome
    # variables are of this kind. By contrast, the latter allows to gain better
    # insight into the performance of panels that adopt the same aggregation
    # rule.
    # To distinguish between these two measurements, the second one (panel vs
    # infallible panel) uses variable names that begin with "o*".
    #
    # So, we start with the error in the evaluation.
    # This outcome measure matches the main outcome variable from many
    # simulation models in the literature (e.g. those based on Squazzoni &
    # Gandelli 2012, 2013).
    # Note that this measure cannot be calculated when aggregation is done by
    # majority judgment. This is because majority judgment outputs only a 
    # ranking of proposals, and not an absolute grade for each.
    ifelse(
      aggrRule[rule] %in% c("majorityJudgement", "bordaCount"),
      qualityEfficacy <- NA,
      qualityEfficacy <-
        1 - mean(abs((submissions$refGrade / (criteria$scale - 1)) - x))
    )
    #oQualityEfficacy <- 1 - mean(abs(ox - x))
    
    # Then we compare the two rankings: based on their true quality and their
    # estimated quality (x).
    # rankingEfficacy is low when many proposal on top of the true ranking are
    # not on top according to the estimated ranking.
    # Note that, despite the underlying strict ordering, the way we calculate
    # this measure is equivalent to assuming a weak ordering of submissions.
    rankingEfficacy <- auc <- CohensKappa <- c()#typeIperf <- typeIIperf <- c()
    confusionMatrix <- list()
    #rankingEfficacy <- oRankingEfficacy <- c()
    
    # For every value of k (nAccepted) that we explore: 
    for (a in 1:length(nAccepted)){
      k = nAccepted[a]
      
      # The deserved grade of the the k-th best in the merit ranking:
      #thT <- submissions$trueGrade[submissions$refRanking == k]
      thT <- submissions$refQuality[submissions$refRanking == k]
      
      # The list of proposals that deserve funding: those that have a true
      # quality at least equal to that of the k-th proposal in the merit
      # ranking.
      #acceptableLogic <- submissions$trueGrade >= thT
      acceptableLogic <- submissions$refQuality >= thT
      acceptableOnes <- which(acceptableLogic)
      
      # Because there might be ties for the k-th position (although extremely
      # unlikely), the number of acceptable proposals might be equal to *or
      # higher than* k.
      # So, we need to re-calculate how many funding-worthy proposals there are,
      # and we call that number "kPrime":
      kPrime <- sum(acceptableLogic)
      
      
      # Let's now look at the panel ranking. The panel has to find the kPrime
      # best proposals.
      # The quality of the kPrime-th best in the panel ranking:
      thE <- x[xr == kPrime]
      
      # Submissions above the equivalence class of the kPrime-th best (We'll
      # deal with the equivalence class x==thE right after):
      surelyAcceptedLogic <- x > thE
      nonDiscretionaryPanelChoice <- which(surelyAcceptedLogic)
      A <- length(nonDiscretionaryPanelChoice)
      
      # How many of these are right:
      A1 <- sum(nonDiscretionaryPanelChoice %in% acceptableOnes)
      
      # Submissions in kPrime-th equivalence class (i.e. the discretionary
      # component of panel's choice):
      maybeAcceptedLogic <- x == thE
      kthEquivClass <- which(maybeAcceptedLogic)
      B <- sum(maybeAcceptedLogic)
      
      # Of which, these many are right:
      B1 <- sum(kthEquivClass %in% acceptableOnes)
      
      # Choice performance for this level of k is:
      rankingEfficacy[a] <- (((kPrime - A) * (B1 / B)) + A1) / kPrime
      
      # ROC AUC
      ifelse(
        length(table(acceptableLogic)) == 2,
        # If we have both acceptable and non-acceptable proposals, then AUC is:
        auc[a] <- AUC::auc(
          AUC::roc(
            labels = as.factor(as.numeric(acceptableLogic)),
            predictions = x),
          min = 0, max = 1
        ), 
        # else, the AUC can't be calculated:
        auc[a] <- NA 
      )
      
      #aucReject[a] <- AUC::auc(
      #  AUC::roc(
      #    labels = as.factor(as.numeric(!acceptableLogic)),
      #    predictions = x),
      #  min = 0, max = 1
      #)
      
      # We also calculate Cohen's Kappa to correct for the probability that
      # the panel chooses the correct submissions by chance.
      # For the panel decision, there might be ties in the kth equivalence
      # class. If so, we calculate Kappa for each of the possible selections
      # the panel can make in that class, and then we average across all
      # alternatives.
      panelChoice <- c()
      if (B > 1) { # If there are more than 1 proposal tieing for k...
        
        # The panel picks random proposals from the equivalence class until it
        # has k:
        randomPick <- sample(kthEquivClass, size = kPrime - A, replace = FALSE)
        panelChoice <- surelyAcceptedLogic# | maybeAcceptedLogic
        panelChoice[randomPick] <- TRUE
        
      } else { # Instead, if there are no ties in kth equivalence class:
        panelChoice <- surelyAcceptedLogic | maybeAcceptedLogic
      }
      
      
      # Now we can calculate the confusion matrix and its related measures of
      # performance:
      confusionMatrix[[a]] <- as.matrix(table(acceptableLogic, panelChoice))
      
      CohensKappa[a] <- irr::kappa2(as.matrix(cbind(
        acceptableLogic, panelChoice
      )))$value
      
      ## Info on the margins of the confusion matrix:
      #kmerit <- sum(acceptableLogic)
      #kpanel <- sum(panelChoice)
      #
      ## ... which we use to calculate the type I and type II error performance:
      #falsePositive <- sum(panelChoice & !acceptableLogic)
      #expectFalsePositive <- kmerit * (nSubmissions - kmerit) / nSubmissions
      #typeIperf[a] <- 1 - (falsePositive / expectFalsePositive)
      #  
      #falseNegative <- sum(!panelChoice & acceptableLogic)
      #expectFalseNegative <-  kmerit * (nSubmissions - kpanel) / nSubmissions
      #typeIIperf[a] <- 1 - (falseNegative / expectFalseNegative)
    }
    
    # For the next outcome metrics, we need to have the weak orderings of
    # submissions (based on their true quality and estimated quality).
    weakOrdRefGrade <- rank(submissions$refGrade, ties.method = "max")
    weakOrdEstimQuality <- rank(x, ties.method = "max")
    
    # Now we calculate the normalized Kendall-tau distance between the two
    # weak orderings: based on trueGrade and based on estimated quality (x).
    # The Kendall distance is calculated for the whole ranking (kend), and for
    # the objectively-best proposals only (kendTop).
    ifelse(
      length(table(weakOrdRefGrade)) > 1,
      ktd <- kendallTauDistance(
        weakOrdRefGrade,
        weakOrdEstimQuality
      )$normalized,
      ktd <- NA
    )
    
    
    #ktdTop <- c()
    #spearmanTop <- c()
    #for (a in 1:length(nAccepted)){
    #  thT <- submissions$trueGrade[submissions$refRanking == nAccepted[a]]
    #  top <- which(submissions$trueGrade >= thT)
    #  
    #  meritRank <- rank(submissions$trueGrade[top], ties.method = "max")
    #  panelRank <- rank(x[top], ties.method = "max")
    #  
    #  
    #  ktdTop[a] <- kendallTauDistance(
    #    meritRank,
    #    panelRank
    #  )$normalized
    #
    #  spearmanTop[a] <- suppressWarnings(cor(
    #    meritRank,
    #    panelRank,
    #    method = "spearman"
    #  ))
    #  if (is.na(spearmanTop[a])) {spearmanTop[a] <- 0}
    #}
    
    # We also calculate two correlation coefficients for the two rankings:
    ktc <- suppressWarnings(cor(
      weakOrdRefGrade,
      weakOrdEstimQuality,
      method = "kendall"
    ))

    spearman <- suppressWarnings(cor(
      submissions$refGrade,
      x,
      method = "spearman"
    ))
    
    # Sometimes it can happen that all proposals get the same aggregate score
    # and ranking position (e.g. when there are very few proposals, or when
    # the aggregation rule is lowest/highest score and there is a large number
    # of reviewers). When this happens, the correlation cannot be calculated
    # because one of the variables has no variability.
    # We interpret this as evidence that the aggregation rule performed
    # exceptionally poorly. For this reason, we replace the missing value
    # returned by the correlation function with a zero:
    if (is.na(ktc)) {ktc <- 0}
    if (is.na(spearman)) {spearman <- 0}
    
    
    # We store the outcome variables we have calculated in a dedicated list.
    outcomeMetrics <- list(
      
      # First, the measures that compare panel judgment with true merit:
      #bayesEfficacy = bayesEfficacy,
      confusionMatrix = confusionMatrix,
      qualityEfficacy = qualityEfficacy,
      rankingEfficacy = rankingEfficacy,
      auc = auc,
      #aucReject = aucReject,
      CohensKappa = CohensKappa,
      #typeIperf = typeIperf,
      #typeIIperf = typeIIperf,
      ktd = ktd,
      #ktdTop = ktdTop,
      #spearmanTop = spearmanTop,
      ktc = ktc,
      spearman = spearman
    )
    
    
    # Last, we add all data to the results list, and naming the dataframe
    # according to the appropriate aggregation rule adopted.
    r[[rule]] <- list(
      grades = grades,
      i = data.frame(
        estimQuality = x,
        estimRanking = xr
      ),
      #tqDeserved = tqDeserved,
      #estimated = estimated,
      outcomeMetrics = outcomeMetrics
    )
    names(r)[rule] <- aggrRule[rule]
    
  }
  
  # And we terminate the simulation by returning info on the initial
  # configuration and simulation outcomes:
  return(list(
    parameters = parameters,
    submissions = submissions,
    reviewers = reviewers,
    gradeLanguages = gradeLanguages,
    reviewNetwork = rnw,
    results = r
  ))
}




# Usage example_________________________________________________________________
if (FALSE) {
#  
#
sim <- simulation(
  criteria = cbind.data.frame(
    name    = c("q1"),    # name of the criterion
    implicitScale = "high",
    #alpha   = c(5),       # alpha (parameter in the beta distribution)
    #beta    = c(2),       # beta  (parameter in the beta distribution)
    scale   = c(5),       # scale (expressed as number of categories)
    glh     = c(0.1),     # grade language heterogeneity
    weights = c(1)        # relative weight of the criteria 
  ), 
  nSubmissions = 100,
  nReviewersPerProp = 3,
  nPropPerReviewer = 100,
  truthNoise = 0,
  reviewerError = 0.2,
  reviewerVariability = 0,
  aggrRule = c(
    "mean",
    "excludeExtremes",
    "lowestScore",
    "highestScore",
    "median",
    "majorityJudgement",
    "bordaCount",
    "control"
  ),
  ruleVariant = "none",
  nAccepted = c(5, 10, 20, 50),
  #seed = 12345,#sample(-999999:999999, size = 1)
  debug = FALSE
)
#
#
} 