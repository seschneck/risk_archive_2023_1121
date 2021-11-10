# fit_model <- function(d, ...) -----------------------)

#fits a variety of statistical model types to GPS data
#currently limited to single loop but easy to adapt
fit_model <- function(d, iters, i_iter_outer = NA, i_iter_inner, 
                      model_type = model_type, features = features) {
  
  #NOTE: features argument not currently used
  
  held_in <- iters$inner_in_id[[i_iter_inner]]
  held_out <- iters$inner_out_id[[i_iter_inner]]
  iter_outer <- ""
  iter_inner <- names(iters$inner_in_id)[[i_iter_inner]]
  
  #for nested
  # held_in <- iters$inner_in_id[[i_iter_outer]][[i_iter_inner]]
  # held_out <- iters$inner_out_id[[i_iter_outer]][[i_iter_inner]]
  # iter_outer <- names(iters$inner_in_id)[i_iter_outer]
  # iter_inner <- names(iters$inner_in_id[[i_iter_outer]])[[i_iter_inner]]
  
  
  trn_ctrl <- trainControl(
    method = "cv", number = 1,
    index = list(held_in), indexOut = list(held_out),
    allowParallel = FALSE,
    classProbs = TRUE, savePredictions = TRUE,
    summaryFunction = twoClassSummary 
  )
  
  y <- factor(d$y, levels = c(0,1), labels = c("NoLapse",  "Lapse"))
  x <- as.data.frame(select(d, -y))
  
  if (model_type == "glmnet") {
    m <- train(y = y, x = x,
               method = "glmnet", family = "binomial",
               preProcess = c("nzv", "medianImpute","center","scale"),
               trControl = trn_ctrl,
               tuneLength = 20,
               metric="ROC")
    
    hp1 <- m$results$alpha
    hp2 <- m$results$lambda
    hp3 <- NA
  }

  if (model_type == "ranger") {   
    m <- train(y = y, x = x,
               method = "ranger", num.trees = 500,
               preProcess = c("nzv", "medianImpute"),
               trControl = trn_ctrl,
               tuneLength = 50,
               metric = "ROC"
    )
    
    hp1 <- m$results$mtry
    hp2 <- m$results$min.node.size  
    hp3 <- m$results$splitrule
  }
  
  results <- tibble(i_iter_outer = i_iter_outer,
                    i_iter_inner = i_iter_inner,
                    model_type = model_type,
                    features = features,
                    hp1 = hp1, 
                    hp2 = hp2,
                    hp3 = hp3,
                    auc_inner = m$results$ROC)
  
  return(results)
}