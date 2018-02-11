customizedGlmnet <-
function(xTrain, yTrain, xTest, groupid = NULL, G = NULL,
    family = c("gaussian", "binomial", "multinomial"),
    dendrogram = NULL, dendrogramTestIndices = NULL)
{
  if (nrow(xTrain) != length(yTrain)) {
    stop(paste('num. of rows in xTrain (', nrow(xTrain),
    'does not match length of yTrain (', length(yTrain), ')', sep = ''))
  } else if (ncol(xTrain) != ncol(xTest)) {
    stop(paste('num. of cols of xTrain (', ncol(xTrain),
    'does not match num. of cols of xTest (', ncol(xTest), ')', sep = ''))
  } else if (!is.null(groupid) && nrow(xTest) != length(groupid)) {
    stop(paste('num. of rows of xTest (', nrow(xTest),
    'does not match length of groupid (', length(groupid), ')', sep = ''))
  }

  family = family[1]

  standard = glmnet(xTrain, yTrain, family = family)

  CTset = list()

  if (!is.null(groupid)) {

    groups = as.character(sort(unique(groupid)))
    G = length(groups)
    
    for (group in groups) {
        NN = get.knnx(xTrain, xTest[groupid == group, ])$nn.index
        CTset[[group]] = unique(c(NN))
    }

  } else {

    if (is.null(G)) {
    stop("Either G or group must be specified")
    }

    if (is.null(dendrogram)) {
        dendrogram = hclust(dist(rbind(xTrain, xTest)))
    }

    if (is.null(dendrogramTestIndices)) {
        dendrogramTestIndices =
            rep(c(FALSE, TRUE), times = c(nrow(xTrain), nrow(xTest)))
    }

    cluster = cutree(dendrogram, k = G)
    groupid = cluster[dendrogramTestIndices]
    groups = as.character(1:G)

    for (group in groups) {
        CTset[[group]] = which(cluster[!dendrogramTestIndices] == group)
    }
  }

  fit = list()
  for (group in groups) {
  	x = xTrain[CTset[[group]], ]
  	y = yTrain[CTset[[group]]]
  	if (family == "multinomial") y = as.factor(as.character(y))
    singleton = list(response = NA, class = NA)
    class(singleton) = 'singleton'
  	if (length(y) == 0) {
  		fit[[group]] = singleton
  	} else if (length(unique(y)) == 1) {
      if (family == "gaussian") {
        singleton$response = unique(y)
      } else if (family == "binomial") {
        singleton$class = as.character(unique(y))
        singleton$response = 1*(unique(y) == sort(unique(yTrain))[2])
      } else if (family == "multinomial") {
        singleton$class = as.character(unique(y))
        singleton$response = rep(0, length(unique(yTrain)))
        singleton$response[sort(unique(yTrain)) == unique(y)] = 1
      }
      fit[[group]] = singleton
    } else if (is.element(family, c("binomial", "multinomial")) &
      min(table(y)) < 2) {
      if (family == 'binomial') singleton$response = table(y)[2]/length(y)
      if (family == 'multinomial') singleton$response = table(y)/length(y)
      singleton$class = names(which.max(table(y)))
      fit[[group]] = singleton
  	} else {
  		fit[[group]] = glmnet(x, y, family = family)
  	}
  }

  model = list(call = match.call(), CTset = CTset, fit = fit,
    groupid = groupid, x = list(train = xTrain, test = xTest), y = yTrain,
    family = family, standard = standard)

  class(model) = "customizedGlmnet"
  return(model)
}
