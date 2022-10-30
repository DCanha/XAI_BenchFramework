library(caret)
set.seed(1313)

##### SELECTIVITY property ######

#FS methods - compacity (apply just this if global method); also apply for local when wanting to see for a particular instance
min_nb_features <- function(phi, mode = "regression", distance = 0.1, print=TRUE) {
    #### Determine the minimum number of features needed for the prediction of the explainability method to be *close enough* to the one obtained with all features.
    ###The closeness is defined via the following distances:
    ##For regression: distance = \\frac{|output_{allFeatures} - output_{currentFeatures}|}{|output_{allFeatures}|}
    ##For classification: distance = |output_{allFeatures} - output_{currentFeatures}|

    #phi (one col DataFrame) - Calculated contribution values for the instance / dataset if global with row names with feature names
    #mode (str) - "classification" or "regression" (default)
    #distance (float)-  How close we want to be from model with all features, by default 0.1 (10%)
    #print (bool) - print which ones are the crucial features?
    
    #Returns features_needed : int - minimum number of required features (for the instance / dataset if global) to be close enough to the prediction 
    features_needed = NULL
    
    phi_order = order(unlist(abs(phi)), decreasing=TRUE)
    phi_ord = abs(as.data.frame(phi[phi_order,]))
    rownames(phi_ord) = rownames(phi)[phi_order]
    #print(phi_ord)

    output_value = sum(phi_ord[,1])
    #print(output_value)
    score = 0
    #add features until we get close enough
    for( i in c(1:nrow(phi_ord))) {
        if(print==TRUE)
            print(paste0(rownames(phi_ord)[i], " is a crucial feature."))
        score = score + phi_ord[i,1]
        #print(score)
        # CLOSE_ENOUGH
        if (mode == "regression"){
            if (abs(score - output_value) < distance * abs(output_value))
                break
        }
        else if (mode == "classification") {
            if (abs(score - output_value) < distance)
                break
        }
    }
    features_needed = i 
    return(features_needed)    
}

#calulcate the mean size over ninst data instances (global idea) - specific for the selected methods
mean_size <- function(explainer, data, ninst = 200, mode = "regression", distance = 0.1, print = FALSE, class.idx = 1, 
                      model = NULL, pred = NULL, plot=TRUE) {
    # explainer - xai method to evaluate. It can also be predictor, if this is needed instead of a explainer (to create the explanation).
    # data - training dataset (without label column)
    # ninst (int) to include from data, by default 200
    # mode, distance, print - to apply the function min_nb_features
    # class.idx (default is 1) - also to apply the function min_nb_features (feature selection may be different from class to class)
    # model - it is necessary for kernelSHAP and CFEs
    # pred (prediction function) - necessary for kernelSHAP CFEs
    # print (bool) - print bar plot showing the feature distrbution across ninst instances of the data?
    
    ninst <- min(ninst, nrow(data))
    instinds <- round(runif(ninst, 1, nrow(data)))
    size <- rep(0, ninst)
    if ( "data_frame_explainer" %in% class(explainer) ) {
        #Anchors - Rules
        if ("maxAnchors" %in% names(explainer)) {
            xlab <- "Number of conditions"
            for(inst in 1:ninst){
                explanation <- anchors::explain(data[instinds[inst],],explainer)
                #the number of conditions is the number of rows - 1 (we do not count the base)
                n = nrow(explanation) - 1
                size[inst] = n
            }            
        }
        #LIME - FS method
        else{
            xlab <- "Number of features"
            for(inst in 1:ninst){
                explanation <- lime::explain(data[instinds[inst],], explainer, n_labels = 1, n_features = ncol(data))
                phi = as.data.frame(explanation$feature_weight, row.names = colnames(data))
                #use function above
                lime_sel <- min_nb_features(phi, mode=mode, distance = distance, print = print)
                size[inst] = lime_sel
            }
        }
    }
    #Shapley
    else if ("Predictor" %in% class(explainer)) {
        xlab <- "Number of features"
        for(inst in 1:ninst){
            instance <- data[instinds[inst],]
            explanation <- Shapley$new(explainer, x.interest = instance)
            #this works for both classification and regression because class.idx is one as default
            phi = as.data.frame(explanation$results[(ncol(data)*(class.idx-1)+1):(ncol(instance)*class.idx),]$phi, 
                                row.names = colnames(data))
            #use function above
            shapley_sel <- min_nb_features(phi, mode=mode, distance = distance, print = print)
            size[inst] = shapley_sel    
        }  
    }
    #SHAP
    else if ("function" %in% class(explainer)) {
        xlab <- "Number of features"
        for(inst in 1:ninst) {
            instance <- data[instinds[inst],]
            explanation <- individual_variable_effect(model, data = data, predict_function = explainer,
                                                      new_observation = instance, nsamples=100)
            #this works for both classification and regression because class.idx is one as default
            phi = as.data.frame(explanation[(ncol(instance)*(class.idx-1)+1):(ncol(instance)*class.idx),]["_attribution_"], 
                                row.names = colnames(data))
            #use function above
            shap_sel <- min_nb_features(phi, mode=mode, distance = distance, print = print)
            size[inst] = shap_sel  
        }
    }
    #CIU
    else if ("CIU" %in% class(explainer) ) {
        xlab <- "Number of features"
        for(inst in 1:ninst){
            instance <- data[instinds[inst],]
            phi <- matrix(0, nrow = ninst, ncol=ncol(data))
            #doing for CI as default, because CI reflects the importance of features, which are the ones that should be displayed
            #it is possible to change
            explanation <- explainer$meta.explain(data[instinds[inst],])
            for (inp in 1:nimps ) 
                phi[inst,inp] <- ciu.list.to.frame(ciu.meta$ciuvals, out.ind = class.idx)[inp,]$CI
            #use function above
            ciu_sel <- min_nb_features(phi, mode=mode, distance = distance, print = print)
            size[inst] = ciu_sel
        }
            
    }
    else if ( "CounterfactualMethod" %in% class(explainer)) {
        xlab <- "Number of changed features"
        for(inst in 1:ninst) {
            prediction <- pred(model, data[instinds[inst],])
            #cfe for the class with the lowest prediction - change maybe
            idx <- which.min(prediction)
            class.name <- colnames(prediction)[idx]
            explanation <- explainer$find_counterfactuals(data[instinds[inst],], desired_class = class.name)
            evaluate <- explanation$evaluate()
            #mean number of changed features of the closes counterfactual
            size[inst] = evaluate$no_changed[1]
        }
    }
    mean_size = mean(size)
    if(plot==TRUE) {
        hist(size, 
        xlab=xlab, 
        ylab=paste0("Distribution over ", ninst, " instances"),
        col = "cadetblue")
        abline(v=mean_size,col="cadetblue2",lwd=5)}
    #print(size)
    return (mean_size)
}

##### FIDELITY property #####
#specific for the selected methods

#Only for LIME and Anchors, currently
fidelity <- function(explainer, data, n_features = 5, ninst = 200, mode = "regression", plot = TRUE) {
    # explainer - xai method to evaluate. It can also be predictor, if this is needed instead of a explainer (to create the explanation).
    # data - training dataset (without label column)
    # n_features - creating explanations for n_features (considers the most important) (for lime) (deafult is 5)
    # ninst - evaluating for ninst Default=200
    # mode - classification or regressiom mode for tabular data (anchors only works for classification)
    # plot - plot histogram with fidelity distribution?
    ### returns the mean fidelity associated over all instances
    ninst <- min(ninst, nrow(data))
    instinds <- round(runif(ninst, 1, nrow(data)))
    fids <- rep(0, ninst)
    covs <- rep(0, ninst)
    anchors = FALSE
    if ( "data_frame_explainer" %in% class(explainer) ) {
    # anchors - PC metric
        if ("maxAnchors" %in% names(explainer)) {
            anchors = TRUE
            explanation <- anchors::explain(data[instinds,],explainer)
            for ( inst in 1:ninst) {
                #to grab the precision
                exp_i = explanation[explanation$case==instinds[inst],]
                fids[inst] = tail(exp_i$precision, n=1)
                covs[inst] = tail(exp_i$coverage, n=1)
            }
        }
        #lime - SA metric
        else {
            n_features = min(n_features, ncol(data))
            for (inst in 1:ninst) {
                if (mode == "classification") {
                    #doing for predicted class always
                    explanation <- lime::explain(data[instinds[inst],], explainer, n_labels = 1, n_features = n_features)
                    label_prob = explanation$label_prob[1] #prediction bb model for that class label
                }
                else if (mode == "regression") {
                    explanation <- lime::explain(data[instinds[inst],], explainer, n_features = n_features)
                    label_prob = explanation$prediction[1] #prediction bb model
                }
                model_pred = explanation$model_prediction[1] #prediction surrogate model (equal for regression and classification)
                
                if(label_prob <= model_pred)
                    fids[inst] = label_prob/model_pred
                else
                    fids[inst] = model_pred/label_prob
            }
        }        
    }
    mean_fids = mean(fids)
    mean_covs = mean(covs)
    if(plot==TRUE) {
        hist(fids, 
        xlab= "(Local) Fidelity", 
        ylab=paste0("Distribution over ", ninst, " instances"),
        col = "cadetblue", xlim=c(0,1))
        if (anchors==TRUE) {
            mycol <- rgb(100, 200, 100, max = 255, alpha = 125, names = "mycol")
            hist(covs, col = mycol, add = TRUE) # Add 2nd histogram using different color
            #add legend
            legend('topright', c('precision', 'coverage'), fill=c("cadetblue",mycol))}
        abline(v=mean_fids,col="cadetblue2",lwd=5)
        abline(v=0.5,col="red",lwd=5)}
    
    if(anchors==TRUE)
        return(c(mean_fids,mean_covs))
    else
        return(mean_fids)
}

##### FAITHFULNESS PROPERTY #####

#ROAR metric to evaluate faithfulness - compute accs decrease
roar_accs <-  function(model, globExp, train, test, ind.output= 1, mode = "regression", percentage = 0.9){
    # model - model to be re-trained
    # globExp - dataframe with one column global explanaiTon values and rownames with feature names
    # train - training dataset (with labels column)
    # test - testing dataset (with labels column)
    # ind.output - column index of train and testing datasets thar corresponds to the predictive label output (default is 1, at the beginning);
    # mode - classification or regression mode for tabular data
    # percentage - percentage of features to remove (default is 0.9 - 90%)
    ## Returns dataFrame with two columns: global explanation values and correspondent effect on model accuracy/RMSE

    # choose 90% of the number of features; -1 here to remove the label column
    n_features = round(percentage*(ncol(train)-1))
    # Order globExp dataframe in decreasing order
    gOrder = order(unlist(globExp), decreasing=TRUE)
    gExp_ord = as.data.frame(globExp[gOrder,])
    rownames(gExp_ord) = rownames(globExp)[gOrder]
    colnames(gExp_ord) = 'Glob'
    
    #only asked number of features
    gOrder = gOrder[1:n_features]
    roar <- subset(gExp_ord, row(gExp_ord) < n_features+1)
    #Initial acc also include to plot
    initial_acc <- data.frame("Glob" = NA, row.names=c("Initial"))
    roar <- rbind(initial_acc,roar)
    #Ass acc column to fill with accuracy effect
    roar$acc <- rep(0,n_features+1)
    
    
    #original model accuracy
    prediction <-predict(model, test)
    if(mode == "classification") {
        if("glm" %in% class(model) )
            prediction <- factor(as.numeric(predict(model,test, type="response") > 0.5))
        acc0=confusionMatrix(prediction, test[,ind.output])$overall[1]
        }
    else if(mode=="regression")
        acc0=sqrt(mean((prediction - test[,ind.output])^2))
    #print(paste0("Inicial accuracy is ", acc0))
    roar[1,]$acc = acc0
    #initialize new_training
    new_training <- train
    new_testing <- test
    index=2
    #incrementally remoave features in decreasing order
    for(feat in rownames(roar[-1,])) {
        #print(feat)
        #Replace both datasets by shuffling the data
        new_training[feat] = train[feat][sample(1:nrow(train)), ]
        new_testing[feat] = test[feat][sample(1:nrow(test)), ]
        #retrain 5 times
        acc_feat = rep(0,5)
        for(i in c(1:5)){
            #Retrain the model: update() refits the model with one or more of the model arguments updated.
            retrain <- update(model, . ~ ., data = new_training)
            #Compute new test accuracy with the retrained model
            prediction <- predict(retrain, new_testing)
            if(mode=="classification") {#Acc 
                if("glm" %in% class(model) )
                    prediction <- factor(as.numeric(predict(retrain,new_testing, type="response") > 0.5))
                acc_feat[i] = confusionMatrix(prediction, new_testing[,ind.output])$overall[1]
            }
            else if (mode =="regression") #RMSE
                acc_feat[i] = sqrt(mean((prediction - new_testing[,ind.output])^2))
            }
        #mean over 5 retrains
        acc_mean = mean(acc_feat)
        #print(acc_mean)
        #add acc after removing feat
        roar[index,]$acc = acc_mean
        index = index+1
    }
    return(roar)  
}

library(zoo)
compare_auc <- function(accs, plot=TRUE, mode = "regression", rand.idx = 1, ylim = c(0,1), ID=FALSE) {
     #### ROAR and ID metric to evaluate faithfulness - plot decrease accs/preds and compute aucs
    #accs - data frame where each column corresponds to the second column of the output of either incrDel or roar_accs 
    #rand.idx - where the random explanation accs/preds is located (default is 1, first column)
    #ID - either we are computing for ID metric or ROAR metric (default is False, so for ROAR)
    nfeatures <- nrow(accs)-1
    x <- c(0:nfeatures)
    aucs <- rep(0, ncol(accs))
    pos <- "bottomleft"
    if(ID==TRUE) {
        ylab = "Model prediction"
        pos <- "topright"
    }
    else {
        if(mode=="regression")
            ylab = "Model RMSE"
        else
            ylab = "Model Accuracy"
        }
    if(plot==TRUE){
        plot(-1, type = "b", xlim = c(0,nfeatures), ylim = ylim,  xlab= "Number of features removed", ylab=ylab)
        for(i in 1:ncol(accs)) {
            lines(c(0:nfeatures), accs[,i] , type="b", col=i)
            aucs[i] <- sum(diff(x)*rollmean(accs[,i],2))
        }
        legend( x = pos,
        legend = colnames(accs),
        col = c(1:ncol(accs)), lwd = 1, lty = c(1,1),
        pch = c(1,1))
    }
    
    auc_compare <- data.frame("AUC" = aucs[-rand.idx], row.names = colnames(accs[,-rand.idx]))
    index=1
    for( i in auc_compare[,]) {
        #how much the explainer is better than the random explainer in percentage
        if(mode=="classification" || ID == TRUE) 
            auc_compare[index,] <- (1-i/aucs[rand.idx]) * 100 #the lower the auc, the better
        else
            auc_compare[index,] <- (1-aucs[rand.idx]/i) * 100 #the higher the auc, the better (roar + regression case)
        index=index+1
    }
    return(auc_compare)
    
}

#ID metric to evaluate faithfulness - compute preds decrease
incrDel <-  function(model, pred, loc, instance, base, mode = "regression", percentage = 0.9){
    # model and pred (predictor function) - model to compute predictions
    # loc - dataframe with one column local explanaiTon values and rownames with feature names
    # base - base instance - with optimal for the "opposite" output
    # mode - classification or regression mode for tabular data
    # percentage - percentage of features to remove (default is 0.9 - 90%)
    ## Returns dataFrame with two columns: local explanation values and correspondent effect on model prediction

    # choose 90% of the number of features
    n_features = round(percentage*(ncol(instance)))
    # Order locExp dataframe in decreasing order
    lOrder = order(unlist(loc), decreasing=TRUE)
    l_ord = as.data.frame(loc[lOrder,])
    rownames(l_ord) = rownames(loc)[lOrder]
    colnames(l_ord) = 'Attr'
    #only asked number of features
    lOrder = lOrder[1:n_features]
    faith <- subset(l_ord, row(l_ord) < n_features+1)
    #Initial pred also include to plot
    initial_pred <- data.frame("Attr" = NA, row.names=c("Initial"))
    faith <- rbind(initial_pred,faith)
    faith$pred <- rep(0,n_features+1)    
    #initial prediction
    if(mode=="classification") {
        preds <- pred(model, instance)
        idx <- which.max(preds)
        pred0 <- preds[,idx]
    }
    else
        pred0 <- pred(model, instance)
    faith[1,]$pred = pred0    
    #initialize new instance
    new_instance = instance
    index=2
    for (feat in lOrder) {
        new_instance[,feat] = base[,feat]
        if(mode == "classification") {
            preds = pred(model, new_instance)
            new_pred = preds[,idx]          
        }
        else
            new_pred = predict(model, new_instance)
        faith[index,]$pred = new_pred
        index = index + 1 
    }
    return(faith)
}



##### STABILITY PROPERTY #####

#specific for the selected methods
identity <- function(instance, explainer, model = NULL, data = NULL, n.sens=50, 
                         ciu_variable="influence", mode="regression", class.idx = 1) {
    #instance - instance of interest; to see if the explanation changes when we check again
    #explainer - XAI method explainer (or predictor, when it's the case)
    #model and training data (without label column) - necessary for SHAP
    #n.sens - number of times to recompute the explanation
    #ciu_variable - for CIU, we have 3 possible values (default is influence because influence values are computed from importance and utility values)
    #mode - either classification or regression mode for tabular data
    #class.idx - when it is a classifcation problem, provide output class index (default is 1)
    xlab_plot <- expression(phi)
    sens_values <- matrix(0, nrow=n.sens, ncol=ncol(instance))
    for ( i in 1:n.sens){
        #kernelSHAP
        if("function" %in% class(explainer)) {
            #print("SHAP")
            s <- individual_variable_effect(model, data = data, predict_function = explainer, 
                                                new_observation = instance, nsamples=100)
            shap_loc = s[(ncol(instance)*(class.idx-1)+1):(ncol(instance)*class.idx),]["_attribution_"][,1]
            sens_values[i,] <- shap_loc
            }
        #Shapley
        if ( "Predictor" %in% class(explainer) ) {
            s <- Shapley$new(explainer, x.interest = instance)
            shapley_loc = s$results[(ncol(instance)*(class.idx-1)+1):(ncol(instance)*class.idx),]$phi
            sens_values[i,] <- shapley_loc
    }
        
        # Anchors and LIME
        else if ( "data_frame_explainer" %in% class(explainer)) {
             #not really doing for anchors - takes too long and it anchors is very selective so
            if ("maxAnchors" %in% names(explainer) ) {
                a <- anchors::explain(instance,explainer)
                for(inp in c(2:nrow(a))) {
                    feat = a[inp,]$feature
                    match = match(feat,names(instance))
                    sens_values[i,match] <- a[inp,]$feature_weight
                }
            }
            else {
                if(mode=="classification") 
                    l <- lime::explain(instance, explainer, n_labels = 1, n_features=ncol(instance))
                else
                    l <- lime::explain(instance, explainer, n_features=ncol(instance))
                sens_values[i,] <- l$feature_weight
            }
        }
        
        else if ( "CIU" %in% class(explainer) ) {
            ciu.meta <- explainer$meta.explain(instance)
            ciu.outp <- ciu.list.to.frame(ciu.meta$ciuvals,class.idx)
            for ( inp in 1:ncol(instance) ) {
                if ( ciu_variable == "influence" )
                    sens_values[i,inp] <- explainer$influence(ciu.outp[inp,])
                else if ( ciu_variable == "CI" ) {
                    sens_values[i,inp] <- ciu.outp[inp,]$CI
                    xlab_plot <- "CI"
                }
                else if ( ciu_variable == "CU" ) {
                    sens_values[i,inp] <- ciu.outp[inp,]$CU
                    xlab_plot <- "CU"
                }
            }
        }

    }
    sens_values <- data.frame(sens_values)
    colnames(sens_values) <- colnames(instance)
    return(sens_values)
}

#identity/similarity result
sens_result <- function(data) {
    m <- apply(data,2,mean)
    sd <- apply(data,2,sd) + 0.00001
    #AVERAGE VIEW OF SD (VARIANCE)
    avg_sd <- sum(sd)/sum(m)
    return(avg_sd)
}

# Sensitivity violin plot
sens_plot <- function(data, ylab = expression(phi)) {
    p <- ggplot(reshape2::melt(data), aes(x=variable, y=value)) + geom_violin(position=position_dodge(1)) +
        stat_summary(fun.data=data_summary, geom="pointrange", color="red", position=position_dodge(1)) +
        coord_flip() + labs(y = ylab) + sens_violin_theme
    return(p)
    }

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# Common text sizes everywhere by own theme.
sens_violin_theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_text(size = 18),
  axis.text = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  strip.text = element_text(size=16),
  legend.title = element_text(size=16),
  legend.text = element_text(size=14),
  axis.title.y.left = element_blank()
)


### For similarity metric ###
library(gower)

radius <- function(data, n.neighbors, sample_size=200, percentile=0.95) { 
    #### Calculate the maximum allowed distance between points to be considered as neighbors
    # data : (DataFrame) Pool to sample from and calculate a radius
    #n.neighbors : (int) Mximum number of neighbors considered per instance
    #sample_size : (int) Number of data points to sample from dataset, by default 200
    #percentile : (int)  Percentile used to calculate the distance threshold, by default 95
    # Returns radius : (float) istance threshold

    #Select 200 points max to sample
    size = min(sample_size, nrow(data))
     # Select instances from dataset
    instinds <- round(runif(size, 1, nrow(data)))
    instances = data[instinds,]
    # Initialize the similarity matrix
    similarity_distance = matrix(0,size, size)
    # Calculate pairwise distance between instances
    for(i in c(1:size)) {
        similarities <- gower_dist(instances[i,], data)
        for (j in c(i:size)) {
            dist = similarities[j]
            similarity_distance[i,j] = dist
            similarity_distance[j,i] = dist
        }
    }
    #print(similarity_distance)
    # Select top n.neighbors
    #from index 2 because the fisrt is itself
    ord_X = t(apply(similarity_distance,1,sort))[,2:(n.neighbors+1)]
    # Select the value of the distance that captures XX% of all distances (percentile)
    #return np.percentile(ordered_X.flatten(), percentile)
    return(as.numeric(quantile(as.vector(t(ord_X)), percentile)))
    
}

find_neighbors <- function(instance, data, model, pred.func = NULL, mode = "classification",n.neighbors=10, class.idx = 1) {
    ####  For each instance, select neighbors based on 3 criteria:
    #### 1. First pick top N closest neighbors (L1 Norm + st. dev normalization)
    #### 2. Filter neighbors whose model output is too different from instance (see condition below)
    #### 3. Filter neighbors whose distance is too big compared to a certain threshold
    
    #instance: find neighors with respect to this instance (without label column)
    # data: (dataframe) to identify neighbors (without label column)
    # model and pred.func: bb model and predictor funtion (needed when it's different than default) to compute predictions
    # mode: (str) "classification" or "regression", by default classification
    # n.neighbors: Top N neighbors initially allowed, by default 10
    # class.idx:
    
    #Returns neighbors: (dataframe)  Wrap instance with corresponding neighbors in a list.
    #The df has shape (#neighbors, #features) where #neighbors includes the instance itself.

    #to have the same columns type as the data (Idk other way to do this)
    neighbors = data[1:(n.neighbors+1),]
    #include instance itself
    neighbors[1,] = instance
    #print(neighbors)
    
    #### Filter 1 : Pick top N closest neighbors
    
    #compute similarities: compute distances bewteen data points using Gower distance
    c = gower_dist(instance, data)
    # Pick indices of the closest neighbors 
    neighbors_indices = order(c)[1:n.neighbors]
    # Return instance with its neighbors
    neighbors[2:(n.neighbors+1),] = data[neighbors_indices,]
    # Add distance column
    neighbors$dist <- c(0,c[neighbors_indices])
    #print(neighbors)
    
    #### Filter 2: neighbors with similar blackbox output
    # Calculate predictions for all instances and corresponding neighbors
    if (mode == "regression") 
        neighbors$pred = predict(model, neighbors)        
    else if (mode == "classification"){
        predictions <- pred.func(model, neighbors)
        # Add prediction column - this has the prediction of class.idx label
        neighbors$pred <- predictions[,class.idx]
        }
    # Remove points if prediction is far away from instance prediction
    if (mode == "regression")
        neighbors = neighbors[abs(neighbors$pred - neighbors$pred[1]) < 0.1 * neighbors$pred[1],]
    else if (mode == "classification") 
        neighbors = neighbors[abs(neighbors$pred - neighbors$pred[1]) < 0.1,]
    
    #### Filter 3 : neighbors below a distance threshold
    # Remove points if distance is bigger than radius - guarantees that it is not an out of distribution instance
    radius = radius(data, n.neighbors)
    neighbors = neighbors[neighbors$dist < radius,]
    
    return(neighbors)
}

#specific for the selected methods
similarity <- function(neighbors, explainer, class.idx = 1, ciu_variable = "influence" , data = NULL, model=NULL) {
    print("Similarity analysis...")
    xlab_plot <- expression(phi)
    n.inst = nrow(neighbors)
    #-2 to remove dist and pred indices
    n.feat = ncol(neighbors)-2
    sens_values <- matrix(0, nrow=n.inst, ncol=ncol(neighbors)-2)
    #including instance itself
    neighbors_data = neighbors[,1:(ncol(neighbors)-2)]
    for (i in 1:n.inst){
        instance <-  neighbors_data[i,]
        #kernelSHAP
        if("function" %in% class(explainer)) {
            s <- individual_variable_effect(model, data = data, predict_function = explainer, 
                                            new_observation = instance, nsamples=100)
            shap_loc = s[(ncol(instance)*(class.idx-1)+1):(ncol(instance)*class.idx),]["_attribution_"][,1]
            sens_values[i,] <- shap_loc
            }
        #Shapley
        else if ( "Predictor" %in% class(explainer) ) {
            s <- Shapley$new(explainer, x.interest = instance)
            shapley_loc = s$results[(ncol(instance)*(class.idx-1)+1):(ncol(instance)*class.idx),]$phi
            sens_values[i,] <- shapley_loc
        }
        # Anchors and LIME
        else if ( "data_frame_explainer" %in% class(explainer)) {
            #not really doing for anchors - takes too long and it anchors is very selective so
            if ("maxAnchors" %in% names(explainer) ) {
                a <- anchors::explain(neighbors_data[i,],explainer)
                for(inp in c(2:nrow(a))) {
                    feat = a[inp,]$feature
                    match = match(feat,names(neighbors_data[i,]))
                    sens_values[i,match] <- a[inp,]$feature_weight
                }
            }
            else {
                l <- lime::explain(instance, explainer, n_labels = 1, n_features=n.feat)
                sens_values[i,] <- l$feature_weight
            }
        }
        
        else if ( "CIU" %in% class(explainer) ) {
            ciu.meta <- explainer$meta.explain(instance)
            ciu.outp <- ciu.list.to.frame(ciu.meta$ciuvals,class.idx)
            for ( inp in 1:n.feat ) {
                if(ciu_variable == "influence")
                    sens_values[i,inp] <- explainer$influence(ciu.outp[inp,])
                    
                else if (ciu_variable == "CI") {
                    sens_values[i,inp] <- ciu.outp[inp,]$CI
                    xlab_plot <- "CI"
                }
                else if (ciu_variable == "CU") {
                    sens_values[i,inp] <- ciu.outp[inp,]$CU
                    xlab_plot <- "CU"
                }
            }
        }
        
    }
    sens_values <- data.frame(sens_values)
    colnames(sens_values) <- colnames(neighbors_data)
    return(sens_values)
}