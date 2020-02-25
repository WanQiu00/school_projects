library(radiant)

breakeven = 1.41/60

intuit75k <- readr::read_rds("data/intuit75k.rds")
intuit75k <- mutate(intuit75k, zip801 = zip == '00801', zip804 = zip == '00804')
intuit75k <- mutate_at(intuit75k, .vars = vars(zip_bins), .funs = as_factor)



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
  seed = 1234,
  data_filter = "training == 1"
)
summary(result)


cv.rforest(result,K = 5, mtry = 1:3, num.trees = c(100, 200), min.node.size = seq(5, 10, 1), sample.fraction = 1, trace = TRUE, seed = 1234, fun = profit, cost = 1.41, margin = 60)


result <- rforest(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "dollars", "last",
    "sincepurch", "version1", "owntaxprod", "upgraded", "zip801",
    "zip804"
  ),
  lev = "Yes",
  seed = 1234,
  data_filter = "training == 1"
)
summary(result)

