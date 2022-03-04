library(ggplot2)
library(data.table)
library(dplyr)

setwd ("C:\\Murray_Git\\tidalFlat_v1_2_R")
source("tf_accuracy_funs.R")


# New v1.2 samples done here.
# Sampling script done here: 
# https://code.earthengine.google.com/?scriptPath=users%2Fmurrnick%2FtidalFlat_v1_2%3AtidalFlat_accuracyAnalysis_2019
# Actual final run: https://code.earthengine.google.com/c7192845d8fd38ffffd36e1c617a0f7f
# Note we run the v1.2 validation on 2016 data also, and it yielded a 4% increase in overall accuracy.

# load data ---------------------------------------------------------------
# v1_2 validation set
v1_2_allocations <- read.csv("data/gic_accuracy_2016_v1_2.csv", stringsAsFactors = F)
v1_2_allocations$descriptio <- NULL
v1_2_allocations$.geo <- NULL
# v1_2_allocations$joinField  <- v1_2_allocations$lat * v1_2_allocations$lon
head(v1_2_allocations)
colnames(v1_2_allocations)
nrow(v1_2_allocations)

# join to v1_1 to enable filtering for "include" column
raw_allocations <- v1_2_allocations
nrow(raw_allocations)
colnames(raw_allocations)

observers <- raw_allocations[,c(2,3,4)] # these are columns
reference <- apply(observers, 1, Mode)
mapped <- raw_allocations$v1_2_Class # remapped to 0 == not tidal flat, 2 == tidal flat
str(observers)
str(reference)
str(mapped)

# check out agreement between observers
overallAgreement <- sum(apply(X = observers, MARGIN = 1, FUN = function(x){length(unique(x))==1})) / nrow(observers)
overallAgreement # reported as consensus in the paper 76.9
sum(apply(X = observers[,c(1,2)], MARGIN = 1, FUN = function(x){length(unique(x))==1})) / nrow(observers)
sum(apply(X = observers[,c(2,3)], MARGIN = 1, FUN = function(x){length(unique(x))==1})) / nrow(observers)
sum(apply(X = observers[,c(3,1)], MARGIN = 1, FUN = function(x){length(unique(x))==1})) / nrow(observers) #

get_conf_mat(mapped, observers$aClass) # reported in new tables in supp material
get_conf_mat(mapped, observers$dClass)
get_conf_mat(mapped, observers$eClass)


# standard accuracy assessment --------------------------------------------

alldat_conf_mat <- get_conf_mat(mapped, reference) # this is the mode dataset
alldat_conf_mat # reported in supp material

percentage_agreement(alldat_conf_mat) # reported in paper
cohens_kappa(alldat_conf_mat)
allocation_disagreement(alldat_conf_mat)
quantity_disagreement(alldat_conf_mat)
user_accuracy(alldat_conf_mat)
producer_accuracy(alldat_conf_mat)



# accuracy variance -------------------------------------------------------

resampled_accuracy <- rbindlist(replicate(n = 1000, 
                                          expr = {sampled_accuracies(mapped, reference)}, 
                                          simplify = F))
# mean
lapply(resampled_accuracy, mean) # reported in ppaper 82.2
# 95% interval values
lapply(resampled_accuracy, quantile, c(0.025,0.975))
# empirical + 95%
unlist(lapply(resampled_accuracy, quantile, c(0.975))) - 
  unlist(lapply(resampled_accuracy, mean))
# empirical - 95%
unlist(lapply(resampled_accuracy, mean)) -
unlist(lapply(resampled_accuracy, quantile, c(0.025)))
# approx 95% symetric interval via standard error
unlist(lapply(resampled_accuracy, sd)) * 1.96

# area bounds
## Reported in final paper
## occupy at least 127,921 square kilometres (95 per cent confidence interval: 124,286-131,821 km2).
# 127921 - (127921 * 0.02841532) # v10b published 2.5% - from line 66
# 127921 + (127921 * 0.03049485) # v10b published 97.5% - from line 63


## Check against caret
library (caret)
head(raw_allocations)
ref <- as.factor(reference)
map <- as.factor(mapped)
str(map)

confMx <- confusionMatrix(map,ref, positive = '2')
confMx

confMx_a <- confusionMatrix(map,as.factor(observers$aClass), positive = '2')
confMx_b <- confusionMatrix(map,ref, positive = '2')
confMx_c <- confusionMatrix(map,ref, positive = '2')






# sensitivity analysis ----------------------------------------------------

resampled_accuracy_sensitivity <- rbindlist(lapply(seq(0.05, 0.95, 0.05), 
                                                   sample_at_fractions, mapped, reference, 500))

accuracy_sensitivity_stats <- resampled_accuracy_sensitivity %>%
  group_by(fraction) %>%
  summarise(mean_oa = mean(perc_agr),
            lwr_oa = quantile(perc_agr, 0.025),
            upr_oa = quantile(perc_agr, 0.975),
            # could to min/max here if you really wanted to be harsh (FYI it is contained at 95% at ~75% fraction) 
            # lwr_oa = min(perc_agr),
            # upr_oa = max(perc_agr),
            mean_tidal = mean(tidal_agr),
            mean_nontidal = mean(non_tidal_agr))

traditional_oa_estimate <- percentage_agreement(alldat_conf_mat)

sensitivity_plt <- ggplot(accuracy_sensitivity_stats, aes(x = fraction)) +
  scale_x_continuous(breaks = seq(0.1, 1, 0.1)) +
  geom_ribbon(aes(ymin = lwr_oa, ymax = upr_oa), fill = 'gray') +
  geom_point(aes(y = mean_oa)) +
  geom_hline(aes(yintercept = traditional_oa_estimate), colour = 'red') +
  geom_hline(aes(yintercept = traditional_oa_estimate + traditional_oa_estimate*0.025), colour = 'red', linetype = 2) +
  geom_hline(aes(yintercept = traditional_oa_estimate - traditional_oa_estimate*0.025), colour = 'red', linetype = 2) +
  theme_bw() +
  ggtitle("Mean and 95% CI for overall accuracy sensitivity analysis") +
  xlab("Percentage of validation data sampled") +
  ylab("Overall accuracy (%)")
sensitivity_plt





