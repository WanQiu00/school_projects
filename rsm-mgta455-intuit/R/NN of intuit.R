library(radiant)

intuit75k <- readr::read_rds("data/intuit75k.rds")
register("intuit75k")

## change variable type
intuit75k <- mutate_at(intuit75k, .vars = vars(zip_bins), .funs = as_factor)
## change variable type
intuit75k <- mutate_at(intuit75k, .vars = vars(bizflag), .funs = as_factor)
## change variable type
intuit75k <- mutate_at(intuit75k, .vars = vars(version1), .funs = as_factor)
## change variable type
intuit75k <- mutate_at(intuit75k, .vars = vars(owntaxprod), .funs = as_factor)
## change variable type
intuit75k <- mutate_at(intuit75k, .vars = vars(upgraded), .funs = as_factor)

intuit75k <- mutate_ext(intuit75k, .vars = vars(dollars), .funs = log, .ext = "_ln")

intuit75k <- mutate_ext(intuit75k, .vars = vars(sincepurch), .funs = log, .ext = "_ln")

train <- intuit75k %>% filter(training == 1)
test <- intuit75k %>% filter(training == 0)

result <- nn(
  intuit75k,
  rvar = "res1",
  evar = c("zip_bins","sex", "bizflag", "numords", "dollars_ln", "last", "sincepurch_ln", "version1", "owntaxprod", "upgraded"),
  lev = "Yes",
  size = 3,
  decay = 0.15,
  seed = 1234,
  data_filter = "training == 1"
)

cv.nn(result, K = 5, size = 1:3, decay = seq(0.05, 0.15, 0.05), seed = 1234, fun = profit, cost = 1.41, margin = 60)

result <- nn(
  intuit75k,
  rvar = "res1",
  evar = c("zip_bins","sex", "bizflag", "numords", "dollars_ln", "last", "sincepurch_ln", "version1", "owntaxprod", "upgraded"),
  lev = "Yes",
  size = 2,
  decay = 0.15,
  seed = 1234,
  data_filter = "training == 1"
)
intuit75k$nn_pred <- predict(result, intuit75k)$Prediction
eval_dat <- tibble::tibble(
  id = test$id,
  res1 = test$res1
)
eval_dat$nn <- predict(result, test)$Prediction


bootstrap_dat <- tibble::tibble(
  id = test$id,
)


for (i in 1:100){
  train_temp <- sample_n(train, nrow(train), replace = TRUE)
  result <- nn(
    train_temp,
    rvar = "res1",
    evar = c("zip_bins","sex", "bizflag", "numords", "dollars_ln", "last", "sincepurch_ln", "version1", "owntaxprod", "upgraded"),
    lev = "Yes",
    size = 2,
    decay = 0.15,
    seed = 1234
  )
  bootstrap_dat[i] <- predict(result, test)$Prediction
  print(i)
}


eval_dat$nn_boot <- apply(bootstrap_dat,1,quantile,probs=c(.05))
sum(eval_dat$nn != eval_dat$nn_boot)

breakeven <- 1.41/60
test$nn_pred <- eval_dat$nn > breakeven
test$nn_pred_boot <- eval_dat$nn_boot > breakeven

table(test$res1, test$nn_pred)
accuracy1 <- (938+9261)/(938+9261+165+12136)
accuracy1
table(test$res1, test$nn_pred_boot)
accuracy2 <- (863+12185)/(863+12185+240+9212)
accuracy2


target_nn <- test %>%
  filter(nn_pred == "TRUE")

resp_nn <- test %>%
  filter(nn_pred == "TRUE" & res1 == 'Yes')

profit_nn  <- 60*nrow(resp_nn) - 1.41*nrow(target_nn)
profit_nn


target_nn_boot <- test %>%
  filter(nn_pred_boot == "TRUE")

resp_nn_boot <- test %>%
  filter(nn_pred_boot == "TRUE" & res1 == 'Yes')

profit_nn_boot  <-  60*nrow(resp_nn_boot) - 1.41*nrow(target_nn_boot)
profit_nn_boot
