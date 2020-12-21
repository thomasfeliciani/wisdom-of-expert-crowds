# This script contains all support functions for the simulation.
#


# Allocation rules______________________________________________________________
# 
# Here we create a network of reviewers and authors/applicants/submissions.
# This function creates a matrix, where rows are reviewers and columns are 
# proposals.
allocationNetwork <- function(
  nSubmissions, nReviewersPerProp, nPropPerReviewer
) {
  rnw <- matrix( #creating a minimal network
    0,
    nrow = ceiling(nSubmissions/nPropPerReviewer),
    ncol = ceiling(nSubmissions/nPropPerReviewer)
  )
  diag(rnw) <- 1
  
  # and then concatenating it to itself by row and col as many times as we need:
  rnw <- do.call(rbind, replicate(nReviewersPerProp, rnw, simplify=FALSE)) 
  rnw <- do.call(cbind, replicate(nPropPerReviewer, rnw, simplify=FALSE))
  
  return(rnw[,1:nSubmissions])
}



# Grading language _____________________________________________________________
# A grade language is a vector of thresholds used to discretize a continuous 
# quantity (e.g. a rate).
# This function takes as input a criterion and the scholars data.frame, and for
# each it returns a list where each item is the grade language of a scholar.
createGradeLanguage <- function(
  scholars = reviewers,
  criterion = criteria[1,] # A row of the criteria dataframe, the 1st by default
)
{
  gradeLanguages <- list()
  thresholds <- c()
  
  # If the grade language is random, then we create a grade language where the
  # thresholds are drawn from a uniform distribution.
  if (criterion$gradeLanguage == "random"){
    thresholds <- runif (
      n = criterion$scale - 1, # This determines how many thresholds we need
      min = 0, max = 1
    )
    #thresholds <- thresholds[order(thresholds)]
  }
  
  # If the grade language is symmetric, then the thresholds are evenly spread
  # in the range.
  if (criterion$gradeLanguage == "symmetric"){
    thresholds <- (1:criterion$scale - 1) / criterion$scale
    thresholds <- thresholds[-1]#[c(-1, -criterion$scale)]
  }
  
  # If the grade language is asymmetric, then the threshold are concentrated
  # around high quality values (where reviewers may be more focused)
  if (criterion$gradeLanguage == "asymmetric"){
    for (l in 1:(criterion$scale - 1)){
      thresholds[l] <- 1 - ((3 / 5) ^ l)  ## Equation 2
    }
    #thresholds <- thresholds[order(thresholds)]
  }
  
  
  # Then, for each agent, we determine how much noise (if any)
  # there needs to be around those thresholds
  for (i in 1:nrow(scholars)){
    gradeLanguages[[i]] <- thresholds
    for (l in 1:(criterion$scale - 1)) {
      gradeLanguages[[i]][l] <- truncate(rnorm( # Equation 3
        n = 1,
        mean = thresholds[l],
        sd = criterion$glh * (1 - ((l - 1) / criterion$scale))
      ))
    }
    
    # And finally we order all agent thresholds:
    gradeLanguages[[i]] <- gradeLanguages[[i]][order(gradeLanguages[[i]])]
  }
  
  return(gradeLanguages)
}



# How a referee reviews a proposal _____________________________________________
#
# This function takes as input a submission, and returns a referee's 
# assessment. For now we have three ways in which a reviewer produces an
# evaluation: random, based on quality, or replicating SG2013.
rate <- function(
  evaluatedAttribute,
  rule = "quality",
  sd = 0,
  categories = 2,
  probWeights = NA,
  thresholds = NA
)
{
  # Checking input for validity
  if(categories < 2 | categories != round(categories)){
    warning("Invalid number of categories fed to the function *rate*.")
  }
  
  # Random rating:
  # This rule returns one of the possible values of the evaluation scale
  # (i.e. integers from 1 to the number of categories), drawn at random from
  # a uniform distribution. If specified, probability weights can be assigned
  # for each of the categories.
  if (is.na(probWeights)) {probWeights <- rep(1/categories, categories)}
  if (rule=="random"){
    return(sample(1:categories, size=1, prob=probWeights)-1)#With 2 categories: 1=accept, 0=reject
  }
  
  # Quality-based rating:
  # A raw score in the range [0,1] is produced by estimating the true quality.
  # The estimation is drawn from a normal distribution, with mean equal to the
  # 'quality' of the evaluated attribute, and s.d. = a given amount of error.
  # Then, the raw score is transformed to the approrpiate evaluation scale,
  # which is essentially a discretization of the range [0,1]. By default, the
  # scale is discretized at regular intervals; however, in case thresholds are
  # provided, these are used as a "grade language" and used for discretizing.
  if (rule=="quality"){
    rawScore <- truncate(rnorm(1, mean=evaluatedAttribute, sd=sd))
    ifelse(
      is.na(thresholds),
      return(round((categories - 1) * rawScore, digits = 0)),
      return(findInterval(rawScore, thresholds))
    )
  }
}



# Aggregation of reviewers scores ______________________________________________
#
#
# This function calculates the majority judgement. It takes as input a matrix d,
# where each row is one observation (e.g. a proposal), and each column is one 
# of the grades to be aggregated (e.g. the score given by a reviewer).
# The function returns a vector with the ranking position of each of the 
# observations in d, with higher scores signifying higher grades and ranking
# position.
calcMajorityJudgement <- function(d, scale = max(d, na.rm = TRUE)){
  
  # For each row (i.e. proposal), we re-arrange scores from the lowest to the
  # highest.
  for(i in 1:nrow(d)){
    d[i,] <- d[i,order(d[i,])]
  }
  
  # Determines how columns from the matrix should be re-arranged. The median 
  # column goes first, the median +1 second, the median -1 third ...
  # So, for example, columns 1,2,3,4,5 are to be re-arranged into 5,3,1,2,4 .
  calcSequence <- function(n){
    x <- c(
      rev(seq(0, floor(n - 1), by = 2)), # odd
      seq(1, ceiling(n), by = 2)         # even
    ) + 1
    ifelse(
      n %% 2 == 0, # if n is even...
      return(x[1:n]), # ... we return this sequence, ...
      return(x[n:1]) # ... else we return its reverse.
    )
  }
  sequ <- calcSequence(ncol(d))
  
  # Rearranging columns accordingly.
  dd <- matrix (NA, nrow=nrow(d), ncol=ncol(d))
  for(c in 1:ncol(d)){
    dd[,sequ[c]] <- d[,c]
  }
  
  # Moving NA's to the left-most columns:
  ddd <- matrix(NA, ncol = ncol(dd), nrow = nrow(dd))
  for (i in 1:nrow(ddd)){
    ddd[i,] = dd[i,!is.na(dd[i,])][1:ncol(ddd)]
  }
  dd <- ddd
  
  # Constructing the majority gauge.
  # This code ranks proposals based on their median grade. Ties are broken by
  # comparing the subsequent columns of scores, whose re-arrangement reflects
  # the majority judgment rule.
  #cbind( dd, order(dd[,1], dd[,2], dd[,3]))
  #order(dd[,1], dd[,2], dd[,3])
  columns <- list()
  for (c in 1:ncol(dd)){
    columns[[c]] <- dd[,c]
  }
  rankSequence <- do.call("order", columns)
  
  
  # Creating a copy of the majority gauge matrix. The new matrix will be a
  # subset of the majority gauge, in case the majority gauge contains NA's. In
  # this case, the new matrix will contain as many columns as we can have
  # without NA's.
  NAcount <- 0
  for (i in 1:nrow(dd)){
    count <- sum(is.na(dd[i,]))
    if (count > NAcount) {NAcount <- count}
  }
  ddd <- dd[,1:(ncol(dd) - NAcount)]
  #matrix(NA, ncol = ncol(dd) - NAcount, nrow = nrow(dd))
  #for (i in 1:nrow(ddd)){
  #  ddd[i,] = dd[i,!is.na(dd[i,])][1:ncol(ddd)]
  #}
  #ddd <- dd[,1:(ncol(dd) - NAcount)]
  
  
  # Matrix ddd will be used to calculate the majority values, which essentially
  # are the integers that result from the concatenation of each row in ddd.
  magnitude <- 10 ^ (floor(log10(scale)) + 1)
  f <- (ncol(ddd):1) - 1
  for (c in 1:ncol(ddd)){
    ddd[,c] <- ddd[,c] * (magnitude ^ f[c])
  }
  
  # Calculating the majority judgment and majority value.
  majorityJudgment <- c()
  majorityValue <- c()
  for (i in 1:length(rankSequence)){
    
    majorityJudgment[rankSequence[i]] <- i 
    
    majorityValue[i] <- sum(ddd[i,])
  }
  
  # The above has broken all the ties, including those between proposals which
  # got exactly the same grades. This has to be fixed.
  # We do so by looping though all instances where proposals got the same grade:
  for(i in which(duplicated(d))){
    
    # For each of these instances, we give the tied proposals the same ranking
    # position:
    index <- which(apply(d, 1, function(x) identical(x[1:ncol(d)], d[i,])))
    majorityJudgment[index] <- max(majorityJudgment[index])
  }
  
  
  # Now we have the vector of rank positions according to the majority judgment
  # rule.
  return(list(
    majorityGauge = dd,
    majorityGrade = dd[,1],
    majorityValue = majorityValue,
    majorityJudgment = majorityJudgment
  ))
}


# The Borda count is here modified to allow weak orderings and NA's.
# NA's are always given 0 points, whereas ties are given the same number of
# points. The maximum number of points is for proposals that have a score
# higher than any other.
# This function takes as input the usual matrix d, where rows are proposals and
# columns are reviewers.
modifiedBordaCount <- function (d) {
  dd <- matrix(NA, nrow = nrow(d), ncol = ncol(d))
  for (c in 1:ncol(d)) {
    
    # Higher grades correspond to higher ranking positions and thus more points.
    # Note that the "-1" at the end means that this implementation of the Borda
    # count starts at 0 (i.e. the lowest ranking rows get zero points).
    #
    # Also note that ties.method = "min" implies that each row's points depend
    # on how many rows got a strictly lower score (as in the traditional
    # Borda count). By contrast, ties.method = "max" would mean that each row's
    # points depend on how many rows got an *equal or lower* score.
    ra <- rank(d[,c], ties.method = "min", na.last = FALSE) - 1
    
    # Ensuring that missing data are given 0 points
    ra[is.na(d[,c])] <- 0
    dd[,c] <- ra
  }
  
  return(list(
    countMatrix = dd,
    bordaCount = rowSums(dd)
  ))
}


# We also implement another extension of the Borda count where ties are
# treated differently. By using ties.method="average" instead of "min",
# proposals that tie for the same ranking position do not receive as many
# points as there are proposals with a lower grade, but the average points
# they would get across all possible ways in which that tie could be resolved.
modifiedBordaCount_extended <- function (d) {
  dd <- matrix(NA, nrow = nrow(d), ncol = ncol(d))
  for (c in 1:ncol(d)) {
    
    # Here we set ties.method = "average"
    ra <- rank(d[,c], ties.method = "average", na.last = FALSE) - 1
    
    # Ensuring that missing data are given 0 points
    ra[is.na(d[,c])] <- 0
    dd[,c] <- ra
  }
  
  return(list(
    countMatrix = dd,
    bordaCount = rowSums(dd)
  ))
}


# This special aggregation rule is based on the mean, and weights reviewers' 
# contribution to the panel by the correlation of their judgement with the
# average judgments of the rest of the panel. The parameter dampingOutliers 
# determines whether outliers should weight more or less on the panel judgment.
hypermean <- function(scores, dampingOutliers = TRUE) {
  revWeights <- c()
  
  rowSum <- rowSums(scores, na.rm = TRUE)
  # for each reviewer...
  for (rev in 1:ncol(scores)) {
    
    # calculate the ranking correlation between his grades with the average
    # grades given by the panel as a whole
    revWeights[rev] <- suppressWarnings(cor(
      x = scores[,rev], # reviewer's scores
      #y = rowSums(scores[,-rev], na.rm = TRUE), # sum of scores by the others
      y = rowSum, # sum of scores by the whole panel
      method = "spearman", # ranking correlation
      use = "complete.obs"
    ))
  }
  
  # Here we decide if outliers (i.e. reviewers whose grades correlate poorly
  # with those of the rest of the panel) are to be given a low weight
  # (dampingOutliers == TRUE) or a high one (dampingOutliers == FALSE).
  ifelse (
    dampingOutliers == TRUE, # if outliers must be dampened...
    revWeights <- (revWeights + 1) / 2, # ... then their weight is their corr.;
    revWeights <- 1 - ((revWeights + 1) / 2) # else, their weight is 1 - corr.
  )
  
  # Then, we set to zero the weights of reviewers for whom a correlation could
  # not be calculated (e.g. because they gave all proposals the same grade,
  # causing there to be no variability in their judgment).
  revWeights[is.na(revWeights)] <- 0
  
  # Now it's time to aggregate all score by weighing reviewers.
  aggregatedScores <- c()
  for (i in 1:nrow(scores)){aggregatedScores[i] <- 
    weighted.mean(x = scores[i,], na.rm = TRUE, w = revWeights)}
  
  return(aggregatedScores)
}




# The aggregation function takes as input a vector of reviewer's scores and
# returns the aggregated score.
aggregate <- function (
  scores,
  rule = "mean",
  weights = NA,
  criteriaWeightsError = 0,
  reviewers = NA,
  gradeLanguages = NA
){
  aggregatedScores <- c()
  
  if (rule == "mean"){
    for (i in 1:nrow(scores)){aggregatedScores[i] <- 
      mean(scores[i,], na.rm = TRUE)}
  }
  
  if (rule == "median"){
    for (i in 1:nrow(scores)){aggregatedScores[i] <- 
      median(scores[i,], na.rm = TRUE)}
  }
  
  if (rule == "weightedMean"){
    for (i in 1:nrow(scores)){
      if (criteriaWeightsError == 0) {
        aggregatedScores[i] <- weighted.mean(scores[i,], weights, na.rm = TRUE)
      } else {
        w <- weights + runif(
          n=length(weights),
          min= -criteriaWeightsError, max = criteriaWeightsError
        )
        aggregatedScores[i] <- weighted.mean(scores[i,], w, na.rm = TRUE)
      }
    }
  }
  
  if (rule == "lowestScore"){
    for (i in 1:nrow(scores)){aggregatedScores[i] <- 
      min(scores[i,], na.rm = TRUE)}
  }
  
  if (rule == "excludeExtremes"){
    for (i in 1:nrow(scores)){
      x <- scores[i,]
      x <- x[!is.na(x)]
      len <- length(x)
      
      ifelse(
        
        # if there are more than three reviews...
        len > 3,
        
        # ... take the mean excluding (one of) the minimum grades and (one of)
        # the maximum grades...
        aggregatedScores[i] <- mean(sort(x)[c(-1, -len)]),
        
        # ... else just take the mean.
        aggregatedScores[i] <- mean(x)
      )
    }
  }
  
  if (rule == "majorityJudgement"){
    aggregatedScores <- calcMajorityJudgement(scores)$majorityJudgment
  }
  
  if (rule == "bordaCount"){
    aggregatedScores <- modifiedBordaCount(scores)$bordaCount
  }
  
  return(aggregatedScores)
}


# Outcome measure ______________________________________________________________
#
# Adapted from "Muser" (November 2013), Stackoverflow. Url:
# https://stackoverflow.com/questions/20224871/kendall-tau-distance-a-k-a-bubble-sort-distance-between-permutations-in-base-r/20224872#20224872
# Last accessed on November 30, 2020.
kendallTauDistance <- function(x,y){
  if(length(x) != length(y)) { stop(
    "Function kendallTauDistance was fed vectors of unequal length.")}
  if(any(is.na( c(x,y)))) { warning(
    "Function kendallTauDistance was fed vectors with some NA's.")}
  
  mergeSort <- function(x){
    if(length(x) == 1){
      inv <- 0
    } else {
      n <- length(x)
      n1 <- ceiling(n/2)
      n2 <- n - n1
      y1 <- mergeSort(x[1:n1])
      y2 <- mergeSort(x[n1 + 1:n2])
      inv <- y1$inversions + y2$inversions
      x1 <- y1$sortedVector
      x2 <- y2$sortedVector
      i1 <- 1
      i2 <- 1
      while(i1 + i2 <= n1 + n2 + 1){
        if(i2 > n2 || (i1 <= n1 && x1[i1] <= x2[i2])){
          x[i1 + i2 - 1] <- x1[i1]
          i1 <- i1 + 1
        } else {
          inv <- inv + n1 + 1 - i1
          x[i1 + i2 - 1] <- x2[i2]
          i2 <- i2 + 1
        }
      }
    }
    return (list(inversions=inv,sortedVector=x))
  }
  inversionNumber <- function(x){
    r <- mergeSort(x)
    return (r$inversions)
  }
  distance <- inversionNumber(order(x)[rank(y)])
  return(list(
    distance = distance,
    normalized = distance / (length(x) * (length(x) -1)) * 2
  ))
}


# Miscellanea __________________________________________________________________
truncate <- function(x, min = 0, max = 1){
  if (length(x) > 1) {return(sapply(x, truncate, min = min, max = max))}
  ifelse(
    x < min,
    return(min),
    ifelse(
      x > max,
      return(max),
      return(x)
    )
  )
}