# get average predictions across algorithms
# start with fits_best_rf and fits_best_glmnet from ana_best_model

# Mean probability
preds_best_rf <- fits_best_rf[[2]] %>% 
  select(truth = y,
         estimate = .pred_class,
         prob = .pred_yes) %>% 
  mutate(truth = if_else(truth == "no", "no_lapse", "lapse"),
         truth = factor(truth, levels = c("no_lapse", "lapse")),
         estimate = if_else(estimate == "no", "no_lapse", "lapse"),
         estimate = factor(estimate, levels = c("no_lapse", "lapse")))

preds_best_glmnet <- fits_best_glmnet[[2]] %>% 
  select(truth = y,
         estimate = .pred_class,
         prob = .pred_yes) %>% 
  mutate(truth = if_else(truth == "no", "no_lapse", "lapse"),
         truth = factor(truth, levels = c("no_lapse", "lapse")),
         estimate = if_else(estimate == "no", "no_lapse", "lapse"),
         estimate = factor(estimate, levels = c("no_lapse", "lapse")))
View(head(preds_best_glmnet))

preds_best <- preds_best_glmnet %>% 
  rename(prob_glmnet = prob) %>% 
  bind_cols(preds_best_rf %>% select(prob)) %>% 
  rename(prob_rf = prob) %>% 
  mutate(prob_mean = (prob_rf + prob_glmnet) / 2,
         estimate_mean = factor(if_else(prob_mean < .50, "no_lapse", "lapse"), levels = c("no_lapse", "lapse")))

# Mean
(cm_mean <- preds_best %>% 
  conf_mat(truth, estimate_mean))

cm_mean %>% 
  autoplot()


cm_mean %>% summary(event_level = "second")

preds_best %>% 
  ggplot(data = ., aes(x = prob_mean)) + 
  geom_histogram(bins = 15, fill = "white", col = "black") +
  facet_wrap(~truth, nrow = 2, scales = "free_y") +
  xlab("Pr(Lapse)")

preds_best %>%
  roc_auc(prob_mean, truth = truth, event_level = "second")


# Extreme

preds_best <- preds_best %>% 
  rowwise() %>% 
  mutate(prob_min = min(prob_glmnet, prob_rf),
         prob_max = max(prob_glmnet, prob_rf)) %>% 
  ungroup() %>% 
  mutate(prob_ex = if_else(prob_mean < .50, prob_min, prob_max))

preds_best %>% 
  ggplot(data = ., aes(x = prob_ex)) + 
  geom_histogram(bins = 15, fill = "white", col = "black") +
  facet_wrap(~truth, nrow = 2, scales = "free_y") +
  xlab("Pr(Lapse)")


preds_best %>%
  roc_auc(prob_ex, truth = truth, event_level = "second")
