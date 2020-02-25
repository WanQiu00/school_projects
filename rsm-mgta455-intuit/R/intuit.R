### RFM Model
library(radiant)
intuit75k <- readr::read_rds("data/intuit75k.rds")
register("intuit75k")
## create recency variable(s)
intuit75k <- mutate(intuit75k, rec_sq = xtile(last, 5))
## create frequency variable(s)
intuit75k<- group_by(intuit75k, rec_sq) %>%
  mutate(freq_sq = xtile(numords, 5, rev = TRUE)) %>%
  ungroup()

## create monetary variable(s)
intuit75k <- group_by(intuit75k, rec_sq, freq_sq) %>%
  mutate(mon_sq = xtile(dollars, 5, rev = TRUE)) %>%
  ungroup()

intuit75k <- mutate(intuit75k, rfm_sq = paste0(rec_sq, freq_sq, mon_sq))

breakeven <- 1.41/60

target <- intuit75k %>%
  filter(training == 1) %>%
  group_by(rfm_sq) %>%
  mutate(mailto_sq = mean(res1 == "Yes") > breakeven)%>%
  filter(mailto_sq == "TRUE")

test <- intuit75k %>%
  filter(training == 0 & rfm_sq %in% target$rfm_sq)


resp_rate_rfm <- mean(test$res1 == "Yes")


profit_rfm <- 60*nrow(test)*resp_rate_rfm - 1.41*nrow(test)



### Logistic Regression
intuit75k <- mutate_ext(intuit75k, .vars = vars(dollars), .funs = log, .ext = "_ln")
intuit75k <- mutate_ext(intuit75k, .vars = vars(sincepurch), .funs = log, .ext = "_ln")
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

# Stepwise
result <- logistic(
  intuit75k,
  rvar = "res1",
  evar = c(
    "zip_bins", "sex", "bizflag", "numords", "last", "version1",
    "owntaxprod", "upgraded", "dollars_ln", "sincepurch_ln"
  ),
  lev = "Yes",
  check = "stepwise-backward",
  data_filter = "training == 1"
)
summary(result)
pred <- predict(result, pred_data = intuit75k, conf_lev = 0.9, se = TRUE)
print(pred, n = 10)
intuit75k <- store(intuit75k, pred, name = c("click_logit", "click_logit_lb", "click_logit_ub"))

test1 <- intuit75k %>%
  filter(training == 0 )

table(test1$res1, test1$click_logit > breakeven)
accuracy1 <- (980+8028)/(980+8028+123+13369)
accuracy1

table(test1$res1, test1$click_logit_lb > breakeven)
accuracy2 <- (913+10009)/(913+10009+190+11388)
accuracy2


target2 <- intuit75k %>%
  filter(training == 0) %>%
  mutate(mailto_logist = click_logit > breakeven)%>%
  filter(mailto_logist == "TRUE")

resp_rate_lg <- mean(target2$res1 == "Yes")

mail2 <- intuit75k %>%
  filter(training == 0 & click_logit > breakeven)
profit_lg  <- 60*nrow(mail2)*resp_rate_lg - 1.41*nrow(mail2)
profit_lg

# lower_bound
target_lower <- intuit75k %>%
  filter(training == 0) %>%
  mutate(mailto_logist = click_logit_lb > breakeven)%>%
  filter(mailto_logist == "TRUE")

resp_rate_lower <- mean(target_lower$res1 == "Yes")

mail_lower <- intuit75k %>%
  filter(training == 0 & click_logit_lb > breakeven)
profit_lower  <- 60*nrow(mail_lower)*resp_rate_lower - 1.41*nrow(mail_lower)
profit_lower
