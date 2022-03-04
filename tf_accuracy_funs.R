Mode <- function(x) {
  ux <- unique(x)
  uxt <- tabulate(match(x, ux))
  return(ux[which.max(uxt)])
}

get_conf_mat <- function(mapped, reference) {
  table(mapped, reference)
}

dim_check <- function(x, len = 2) { # DANGER - hard coded to 2 classes, use len = if error matrix is different size
  dim(x)[1] != len | dim(x)[2] != len
}

percentage_agreement <- function(conf_mat) {
  # sum(as.character(data[true_id[[reps]], "veg_class"]) == as.character(pred_class[[reps]])) / length(pred_class[[reps]])
  if(dim_check(conf_mat)) {return(NA)}
  sum(diag(conf_mat)) / sum(conf_mat) # xtab method quicker?
}

cohens_kappa <- function(conf_mat) {
  if(dim_check(conf_mat)) {return(NA)}
  # props <- conf_mat / sum(conf_mat)
  # cor_prob <- sum(diag(props))
  # chance_prob <- sum( apply(props, 1, sum) * apply(props, 2, sum) )
  # below seems to be a touch quicker...
  cor_prob <- sum(diag(conf_mat)) / sum(conf_mat)
  chance_prob <- crossprod(colSums(conf_mat) / sum(conf_mat), rowSums(conf_mat) / sum(conf_mat))[1]
  (cor_prob - chance_prob)/(1 - chance_prob)
}

# disagreemetns kind of stolen from {diffeR} package
disagreement <- function(conf_mat) {
  if(dim_check(conf_mat)) {return(NA)}
  1 - (sum(diag(conf_mat)) / sum(conf_mat))
}

quantity_disagreement <- function(conf_mat) {
  if(dim_check(conf_mat)) {return(NA)}
  sum(abs(apply(conf_mat, 1, sum) - apply(conf_mat, 2, sum))) / 2 / sum(conf_mat)
}

allocation_disagreement <- function(conf_mat) {
  if(dim_check(conf_mat)) {return(NA)}
  disagreement(conf_mat) - quantity_disagreement(conf_mat)
}

producer_accuracy <- function(conf_mat) {
  if(dim_check(conf_mat)) {return(data.frame(NA,NA,NA,NA))} # DANGER - hard coded to 4 classes
  ret <- diag(conf_mat) / apply(conf_mat, 1, sum)
  names(ret) <- paste0(names(ret),"_prod")
  data.frame(as.list(ret))
}

user_accuracy <- function(conf_mat) {
  if(dim_check(conf_mat)) {return(data.frame(NA,NA,NA,NA))} # DANGER - hard coded to 4 classes
  ret <- diag(conf_mat) / apply(conf_mat, 2, sum)
  names(ret) <- paste0(names(ret),"_user")
  data.frame(as.list(ret))
}

sampled_accuracies <- function(mapped, reference) {
  data <- data.frame(mapped, reference, strat = factor(mapped)) %>%
    group_by(strat) %>%
    sample_frac(1, replace = T) # this is the bootstrap
  conf_mat <- get_conf_mat(data$mapped, data$reference)
  data.frame(perc_agr = percentage_agreement(conf_mat),
             non_tidal_agr = conf_mat[1,1] / sum(conf_mat[1,]),
             tidal_agr = conf_mat[2,2] / sum(conf_mat[2,]))
}

accuracy_sensitivity_sample <- function(mapped, reference, fraction) {
  data <- data.frame(mapped, reference, strat = factor(mapped)) %>%
    group_by(strat) %>%
    sample_frac(fraction, replace = F) # this is the bootstrap
  conf_mat <- get_conf_mat(data$mapped, data$reference)
  data.frame(perc_agr = percentage_agreement(conf_mat),
             non_tidal_agr = conf_mat[1,1] / sum(conf_mat[1,]),
             tidal_agr = conf_mat[2,2] / sum(conf_mat[2,]),
             fraction = fraction)
}

sample_at_fractions <- function(fraction, mapped, reference, nboot) {
  rbindlist(replicate(n = nboot, 
                      expr = {accuracy_sensitivity_sample(mapped, reference, fraction)}, 
                      simplify = F))
}