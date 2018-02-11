cv.customizedGlmnet <-
function(xTrain, yTrain, xTest = NULL, groupid = NULL, Gs = NULL,
    		dendrogram = NULL, dendrogramCV = NULL, lambda = NULL,
    		nfolds = 10, foldid = NULL, keep = FALSE,
    		family = c("gaussian", "binomial", "multinomial"), verbose = FALSE)
{
  if (nrow(xTrain) != length(yTrain)) {
    stop(paste('num. of rows in xTrain (', nrow(xTrain),
      'does not match length of yTrain (', length(yTrain), ')', sep = ''))
  } else if (!is.null(xTest) && ncol(xTrain) != ncol(xTest)) {
    stop(paste('num. of cols of xTrain (', ncol(xTrain),
      'does not match num. of cols of xTest (', ncol(xTest), ')', sep = ''))
  } else if (!is.null(groupid) && !is.null(xTest) &&
    nrow(xTest) != length(groupid)) {
    stop(paste('num. of rows of xTest (', nrow(xTest),
      'does not match length of groupid (', length(groupid), ')', sep = ''))
  } else if (is.null(xTest) & is.null(foldid)) {
    stop('Either xTest or foldid (or both) must be specified')
  }

  if (is.null(xTest) & is.null(groupid)) groupid = foldid

  family = family[1]
  if (is.null(lambda)) {
    lambda = glmnet(xTrain, yTrain, family = family)$lambda*nrow(xTrain)
  }

  if (family == "multinomial" | family == "binomial") {
    yTrain = as.factor(yTrain)
  }

  type = 'response'
  if (family == 'multinomial') type = 'class'

  if (!is.null(groupid)) {
    Gs = length(unique(groupid))
  } else {

    if (is.null(Gs)) {
      stop("Either groupid or Gs must be specified")}

    if (is.null(dendrogram)) {
  	  dendrogram = hclust(dist(rbind(xTrain, xTest)))}

    if (is.null(dendrogramCV)) {
  	  dendrogramCV = hclust(dist(xTrain))}
  }

  if (is.null(foldid)) {
  	foldid = sample(rep(1:nfolds, length.out = nrow(xTrain))) 
  }
  folds = sort(unique(foldid))

  error = matrix(NA, length(Gs), length(lambda))
  fit.cv = array(NA, c(length(yTrain), length(Gs), length(lambda)),
    dimnames = list(NULL, Gs, lambda))
  if (family == 'binomial') {
    fit.class = fit.cv
  }
  rownames(error) = Gs
  for (G in Gs) {
  	for (fold in folds) {
      if (verbose) cat('Predicting fold', fold, '...\n')
  		xTrain_k = xTrain[foldid != fold, ]
  		yTrain_k = yTrain[foldid != fold]
  		xTest_k = xTrain[foldid == fold, ]
  		yTest_k = yTrain[foldid == fold]
      if (!is.null(groupid)) {
        groupid_k = foldid[foldid == fold]
      } else groupid_k = NULL
  		dendrogramTestIndices = array(FALSE, nrow(xTrain))
  		dendrogramTestIndices[foldid == fold] = TRUE
  		fit = customizedGlmnet(xTrain_k, yTrain_k, xTest_k, groupid_k, G,
  			family, dendrogramCV, dendrogramTestIndices)
      fit.cv[foldid == fold, as.character(G), ] =
        predict(fit, lambda, type = type)
      if (family == 'binomial') {
        fit.class[foldid == fold, as.character(G), ] =
          predict(fit, lambda, type = 'class')
      }
  	}
    if (family == "gaussian") {
      error[as.character(G), ] =
        colMeans((fit.cv[, as.character(G), ] - yTrain)^2, na.rm = TRUE)
    } else if (family == "binomial") {
  	  error[as.character(G), ] =
        colMeans(fit.class[, as.character(G), ] != yTrain, na.rm = TRUE)
    } else if (family == "multinomial") {
      error[as.character(G), ] =
        colMeans(fit.cv[, as.character(G), ] != yTrain, na.rm = TRUE)
    }
  }

  G.min = Gs[which.min(apply(error, 1, min))]
  lambda.min = lambda[which.min(error[as.character(G.min), ])]
  output = list(call = match.call(), G.min = G.min, lambda = lambda,
    lambda.min = lambda.min, error = error)

  if (!is.null(xTest)) {
    output$fit = customizedGlmnet(xTrain, yTrain, xTest, groupid, G.min,
    	dendrogram, family = family)
  } else {
    output$fit = customizedGlmnet(xTrain, yTrain, xTrain, foldid, G.min,
      family = family)
  }
  output$selected = nonzero(output$fit, lambda = lambda.min)
  output$prediction = predict(output$fit, lambda = lambda.min, type = type)

  if (keep) output$fit.cv = fit.cv
  class(output) = "cv.customizedGlmnet"
  output
}
