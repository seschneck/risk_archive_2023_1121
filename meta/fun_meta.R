# rope_min and rope_max are the lower and the upper bound of the rope
# if not NULL, plot_min and plot_max will return pdf for this range of metric diffs for plots
# cv_full and cv_compact should be a vector of accuracies/balanced accuracys
bayesian_correlated_t_test <- function(cv_full, cv_compact = NULL, rope_min, rope_max, k = 10, 
                                       plot_min = NULL, plot_max = NULL, plot_n = 1000){
  if (rope_max < rope_min){
    stop("rope_max should be larger than rope_min")
  }
  
  diffs <- if(!is.null(cv_compact)) {
    cv_full - cv_compact
  } else cv_full
  
  delta <- mean(diffs)
  n <- length(diffs)
  df <- n - 1
  stdX <- sd(diffs)
  rho = 1 / k
  sp <- sd(diffs)*sqrt(1/n + rho/(1-rho))
  p.left <- pt((rope_min - delta)/sp, df)
  p.rope <- pt((rope_max - delta)/sp, df)-p.left
  
  results <- list('left'=p.left,'rope'=p.rope,'right'=1-p.left-p.rope)
  
  if (!is.null(plot_min) & !is.null(plot_max)) {
    plot_diffs <- seq(plot_min, plot_max, length.out = plot_n)
    ts <- (plot_diffs - delta) / sp
    pdf <- dt(ts, df)
    results$plot_diffs <- plot_diffs
    results$pdf <- pdf
  }
  
  return(results)
}
