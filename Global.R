# This takes quite a long time if for entire training data

#We do not need the parameter mode here because the way to obtain the CI value is the same for classification and regression :) 
globalCIU <- function(data, explainer, ninst = 200, class.idx = 1) {
    ### Compute CI for entire training set --> global overview = CI
    # data (df) - training data to compute global CIU (without the label column)
    # explainer - CIU explainer
    # class.idx (int) - index of the class we want to study (default 1 - works for regression) 
    # n.inst (int) to include from data, by default 200
    ### Returns dataFrame gCIU with mean CI for all features over ninst and corresponding std
    ninst <- min(ninst, nrow(data))
    nimps <- ncol(data)
    gCI <- matrix(0, nrow = ninst, ncol=nimps)
    instinds <- round(runif(ninst, 1, nrow(data)))
    for ( inst in 1:ninst ) {
        ciu.meta <- explainer$meta.explain(data[instinds[inst],])
        for ( inp in 1:nimps ) {
            gCI[inst,inp] <- ciu.list.to.frame(ciu.meta$ciuvals, out.ind = class.idx)[inp,]$CI
        }
        #so that sum is 1 
        gCI[inst,] <- gCI[inst,]/sum(gCI[inst,])
        #print(sum(gCI[inst,]))
    }
    cmeans <- colMeans(gCI)
    #print(cmeans)
    #print(apply(gCI/sum(cmeans), 2, sd))  
    gCIU <- data.frame(CI=cmeans, sd= apply(gCI, 2, sd))
    rownames(gCIU) <- colnames(data)
    return(gCIU)
}


globalShapley <- function(data, predictor, ninst = 200, class.idx=1) {
    ### Compute shpaley values for entire training set --> global overview mean(Shapley values)
    # predictor - to use to compute the model probabilities (for the shapley explanation)
    # data (df) - training data to compute global shapley (without the label column)
    # n.inst (int) - first n.inst rows to include from data, by default 200
    # class.idx (int) - index of the class we want to study (default 1 - works for regression)  
    ### Returns dataFrame gShapley with mean phi for all features over ninst and corresponding std
    ninst <- min(ninst, nrow(data))
    nimps <- ncol(data) 
    shapvals <- matrix(0, nrow = ninst, ncol=nimps)
    instinds <- round(runif(ninst, 1, nrow(data)))
    for ( inst in 1:ninst ) {
        s <- Shapley$new(predictor, x.interest = data[instinds[inst],])
        shapvals[inst,] <- s$results[(ncol(data)*(class.idx-1)+1):(ncol(data)*class.idx),]$phi
        #do abs
        shapvals[inst,] <- abs(shapvals[inst,])
        #so that sum is 1 
        shapvals[inst,] <- shapvals[inst,]/sum(shapvals[inst,])
    }
    cmeans <- colMeans(shapvals)
    #print(cmeans)  
    #print(cmeans/sum(cmeans))
    gShapley <- data.frame(phi=cmeans, sd= apply(shapvals, 2, sd))
    rownames(gShapley) <- colnames(data)
    return(gShapley)
}

globalShap <- function(model, data, predictor, ninst = 200, class.idx = 1) {
    ### Compute shap values for entire training set --> global overview mean(Shap values)
    # model - for shap, we need the model as one of the inputs
    # predictor - to use to compute the model probabilities (for the shap explanation)
    # data - training data to compute global shapley (without the label column)
    # ninst to include from data, by default 200 
    # class.idx (int) - index of the class we want to study (default 1 - works for regression) 
    # mode - classification or regressiom mode for tabular data
    ## Returns dataFrame gShap with mean phi for all features over ninst and corresponding std
    ninst <- min(ninst, nrow(data))
    nimps <- ncol(data)
    shapvals <- matrix(0, nrow = ninst, ncol=nimps)
    instinds <- round(runif(ninst, 1, nrow(data)))
    for ( inst in 1:ninst ) {
        s <- shapper::individual_variable_effect(model, data = data, predict_function = predictor, new_observation = data[instinds[inst],], nsamples=100)
        shapvals[inst,] <- s[(ncol(data)*(class.idx-1)+1):(ncol(data)*class.idx),]["_attribution_"][,1]    
        #do abs
        shapvals[inst,] <- abs(shapvals[inst,])
        #so that sum is 1 - I put + 0.00001 because the sum may be 0 for SHAP (influence values)
        shapvals[inst,] <- shapvals[inst,]/(sum(shapvals[inst,])+0.00001)
  }
  
  cmeans <- colMeans(shapvals)
  #print(cmeans)  
  gShap <- data.frame(phi=cmeans, sd=apply(shapvals, 2, sd))
  rownames(gShap) <- colnames(data)
  return(gShap)
}

plot_features <- function(globExp){
    # Order globExp dataframe in decreasing order - output of previous functions (without sd column)
    gOrder = order(unlist(globExp), decreasing=FALSE)
    gExp_ord = as.data.frame(globExp[gOrder,])
    rownames(gExp_ord) = rownames(globExp)[gOrder]
    colnames(gExp_ord) = 'Glob'
    
    barplot(gExp_ord[,1], names.arg = rownames(gExp_ord), xlab ="Importance", 
        col ="cadetblue", horiz=TRUE, main = "Global Importance", las=2, cex.names = 0.7)
}