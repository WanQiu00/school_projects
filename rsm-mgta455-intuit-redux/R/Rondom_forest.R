library(radiant)

intuit75k <- readr::read_rds("data/intuit75k.rds")
intuit75k <- mutate(intuit75k, zip801 = zip == '00801', zip804 = zip == '00804')
intuit75k <- mutate_at(intuit75k, .vars = vars(zip_bins), .funs = as_factor)

result <- nn(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded", "zip801",
    "zip804"
  ),
  lev = "Yes",
  size = 4,
  decay = 0.35,
  seed = 1234,
  data_filter = "training == 1"
)
# prediction
intuit75k$nn_pred <- predict(result, intuit75k)$Prediction

# data frames
eval_dat <- tibble::tibble(
  id = test$id,
  res1 = test$res1
)
eval_dat$nn <- predict(result, test)$Prediction
bootstrap_dat <- tibble::tibble(
  id = test$id,
)
bootstrap_dat <- tibble::tibble(
  id = test$id,
)

# Bootstrap 100 times

for (i in 1:100){
  train_temp <- sample_n(train, nrow(train), replace = TRUE)
  result <- nn(
    intuit75k,
    rvar = "res1",
    evar = c(
      "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
      "sincepurch", "version1", "owntaxprod", "upgraded", "zip801",
      "zip804"
    ),
    lev = "Yes",
    size = 4,
    decay = 0.35,
    seed = 1234,
  )
  bootstrap_dat[i] <- predict(result, test)$Prediction
  print(i)
}

# 5th percentile
eval_dat$nn_boot <- apply(bootstrap_dat,1,quantile,probs=c(.75))

# breakeven
breakeven <- 1.41 / 60

# append predictions
test$nn_pred <- eval_dat$nn > breakeven
test$nn_pred_boot <- eval_dat$nn_boot > breakeven

# true positive rates
table(test$res1, test$nn_pred)
937/(937+11927)
table(test$res1, test$nn_pred_boot)
951/(951+12016)
# profit and ROME for nn
target_nn <- test %>%
  filter(nn_pred == "TRUE")
resp_nn <- test %>%
  filter(nn_pred == "TRUE" & res1 == 'Yes')

profit_nn  <- 60*nrow(resp_nn) - 1.41*nrow(target_nn)
profit_nn
ROME_nn <- profit_nn/(1.41*nrow(target_nn))
ROME_nn

# profit and ROME for nn_boot
target_nn_boot <- test %>%
  filter(nn_pred_boot == "TRUE")
resp_nn_boot <- test %>%
  filter(nn_pred_boot == "TRUE" & res1 == 'Yes')

profit_nn_boot  <-  60*nrow(resp_nn_boot) - 1.41*nrow(target_nn_boot)
profit_nn_boot
ROME_nn_boot <- profit_nn_boot/(1.41*nrow(target_nn_boot))
ROME_nn_boot



#Decision Tree
result <- crtree(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded", "zip801",
    "zip804"
  ),
  type = "classification",
  lev = "Yes",
  cost = 1.41,
  margin = 60,
  data_filter = "training == 1"
)
summary(result, prn = TRUE)
pred <- predict(result, pred_data = intuit75k)
print(pred, n = 10)
intuit75k <- store(intuit75k, pred, name = "pred_crtree")

intuit75k$mail2_tree <- intuit75k$pred_crtree > breakeven

test <- intuit75k %>%
  filter(training == 0)


target_crtree <- test %>%
  filter(mail2_tree == TRUE)
resp_crtree <- test %>%
  filter(mail2_tree == TRUE & res1 == 'Yes')

profit_crtree <-  60*nrow(resp_crtree) - 1.41*nrow(target_crtree)


# Rondom Forest

result <- rforest(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded", "zip801",
    "zip804"
  ),
  lev = "Yes",
  mtry = 1,
  seed = 1234
)
summary(result)


cv.rforest(result,K = 5, mtry = 1:3, num.trees = c(100, 200), min.node.size = seq(1, 10, 5), sample.fraction = 1, trace = TRUE, seed = 1234, fun = profit, cost = 1.41, margin = 60)



