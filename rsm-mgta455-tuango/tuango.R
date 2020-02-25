
library(tidyverse)
library(radiant)

tuango <- readr::read_rds("data/tuango.rds")
#1
ifbuy <- tuango %>% count(buyer)
percentage = ifbuy[1,2]/nrow(tuango)

#2
amount <- tuango %>%
  filter(buyer == 'yes') %>%
  summarise(average = mean(ordersize))

#3
# create new variable(s)
tuango <- mutate(tuango, rec_iq = xtile(recency, 5))
## create new variable(s)
tuango <- mutate(tuango, freq_iq = xtile(frequency, 5, rev = TRUE))
## create new variable(s)
tuango <- mutate(tuango, mon_iq = xtile(monetary, 5, rev = TRUE))#

#4
visualize(
  tuango,
  xvar = c("rec_iq", "freq_iq", "mon_iq"),
  yvar = "buyer",
  type = "bar",
  custom = FALSE
)

#5
visualize(
  tuango,
  xvar = c("rec_iq", "freq_iq", "mon_iq"),
  yvar = "ordersize",
  type = "bar",
  data_filter = "buyer == 'yes'",
  custom = FALSE
)

#6
#The above bar plot reveals that the likelihood of response is positively related to RFM.The more recency, frequently and monetary value, the higher likelihood of response.
#For the size of the order across all recency, frequency and monetary quantiles, the trend of the graph is moderate that it’s almost a horizontal line among different quantiles.

#7

## create new variable(s)
tuango <- mutate(tuango, rfm_iq = paste0(rec_iq, freq_iq, mon_iq))

## create new variable(s)
tuango <- group_by(tuango, rec_iq) %>%
  mutate(freq_sq = xtile( frequency, 5, rev = TRUE)) %>%
  ungroup()
## create new variable(s)
tuango <- group_by(tuango, rec_iq, freq_sq) %>%
  mutate(mon_sq = xtile(monetary, 5, rev= TRUE)) %>%
  ungroup()
## create new variable(s)
tuango <- mutate(tuango, rfm_sq = paste0(rec_iq, freq_sq, mon_sq))


margin <- amount*0.5
cost <- 2.5
breakeven <- cost/margin


# 8
n <- 250902
response <- n * percentage
profit <- margin*response - cost*n
ROME <- profit/(cost*n)

#9
perf_calc <- function(x) {
  temp <- tuango %>%
    group_by(x) %>%
    summarize(resp_rate = mean(buyer == "yes"))%>%
    left_join(tuango) %>%
    filter(resp_rate > 0.02473671) %>%
    summarise(perc = n()/nrow(tuango), resp = mean(resp_rate), order = sum(ordersize)/sum(buyer == "yes"),
              profit = perc*order*resp*n*0.5 - cost*n*perc, ROME = profit/(cost*n*perc))
  return(c(temp[4],temp[5]))
}

perf_calc(rfm_iq)

tuango_iq <- tuango %>%
  group_by(rfm_iq) %>%
  summarize(resp_rate = mean(buyer == "yes"))%>%
  left_join(tuango) %>%
  filter(resp_rate > 0.02473671) %>%
  summarise(perc = n()/nrow(tuango), resp = mean(resp_rate),
            profit = perc*amount*resp*n*0.5 - cost*n*perc, ROME = profit/(cost*n*perc))

profit_iq <-tuango_iq$profit
ROME_iq<-tuango_iq$ROME

tuango_sq <- tuango %>%
  group_by(rfm_sq) %>%
  summarise(resp_rate = mean(buyer == "yes")) %>%
  left_join(tuango) %>%
  filter(resp_rate > 0.02473671) %>%
  summarise(perc = n()/nrow(tuango), resp = mean(buyer == 'yes'),
            profit = perc*amount*resp*n*0.5 - cost*n*perc, ROME = profit/(cost*n*perc))
profit_sq <-tuango_sq$profit
ROME_sq<-tuango_sq$ROME
print(profit_sq)
print(ROME_sq)

# 10
tuango %>% filter(rfm_iq != rfm_sq)
#The two approaches generally doesn't yield the same RFM index for any given customer.
#Independent quantiles create 5 independent groups and each of them is analyzed independently.
#This method is simpler than sequential quantiles which create 125 groups.
#However, sequential quantiles is a superior method, since for each recency quantile, we create frequency quantiles which is then divided into smaller monetary quantiles.
#This method is more accuate to predict the performance.

# 11
cell <- tuango %>%
  group_by(rfm_sq) %>%
  summarize(resp_rate = mean(buyer == "yes"), order = sum(ordersize)/sum(buyer == 'yes'),
            break_even = cost/(order*0.5)) %>%
  left_join(tuango) %>%
  filter(resp_rate > break_even) %>%
  summarise(perc = n()/nrow(tuango), resp = mean(buyer == 'yes'),
            profit = perc*amount*resp*n*0.5 - cost*n*perc, ROME = profit/(cost*n*perc))
profit_sq_cell <- cell$profit
ROME_sq_cell<-cell$ROME
print(profit_sq_cell)
print(profit_sq_cel)
#12
tuango1 <- group_by(tuango, rfm_iq) %>%
  mutate(mailto_lbiq = (mean(buyer == "yes") - 1.64 * seprop(buyer == "yes")) > breakeven) %>%
  ungroup()
perc_mail <- mean(tuango1$mailto_lbiq)
nr_mail <- n * perc_mail
dat <- filter(tuango1, mailto_lbiq == TRUE)
rep_rate <- mean(dat$buyer == "yes")
nr_resp <- nr_mail * rep_rate
mail_cost_lbiq <- cost * nr_mail
profit_lbiq <- 0.5 * amount * nr_resp - mail_cost_lbiq
ROME_lbiq <- profit_lbiq / mail_cost_lbiq
print(profit_lbiq)
print(ROME_lbiq)

tuango2 <- group_by(tuango, rfm_sq) %>%
  mutate(mailto_lbsq = (mean(buyer == "yes") - 1.64 * seprop(buyer == "yes")) > breakeven) %>%
  ungroup()
perc_mail <- mean(tuango2$mailto_lbsq)
nr_mail <- n * perc_mail
dat <- filter(tuango2, mailto_lbsq == TRUE)
rep_rate <- mean(dat$buyer == "yes")
nr_resp <- nr_mail * rep_rate
mail_cost_lbsq <- cost * nr_mail
profit_lbsq <- 0.5 * amount * nr_resp - mail_cost_lbsq
ROME_lbsq <- profit_lbsq / mail_cost_lbsq
print(profit_lbsq)
print(ROME_lbsq)

#13
results <- tibble::tibble(
  name = c("Indep. RFM", "Sequen. RFM", "Cell Sequen. RFM", "Indep. lb RFM", "Seq. lb RFM"),
  Profit = c(profit_iq, profit_sq, profit_sq_cell, profit_lbiq, profit_lbsq),
  ROME = c(ROME_iq, ROME_sq, ROME_sq_cell, ROME_lbiq, ROME_lbsq)
) %>%
  mutate(name = factor(name, levels = name))
results$Profit <- as.numeric(results$Profit)
results$ROME <- as.numeric(results$ROME)

results %>%
  ggplot(aes(x = name, y = Profit, fill = name)) + geom_bar(stat = "identity") + geom_text(aes(label = round(Profit, 2)), size = 3)

results %>%
  ggplot(aes(x = name, y = ROME, fill = name)) + geom_bar(stat = "identity") + geom_text(aes(label = round(ROME, 2)), size = 3)

#14
library(radiant)
tuango_post <- readr::read_rds("data/tuango_post.rds")

register("tuango_post")
## create new variable(s)
tuango_post <- mutate(tuango_post, rec_iq = xtile(recency, 5))
## create new variable(s)
tuango_post <- mutate(tuango_post, freq_iq = xtile(frequency, 5, rev = TRUE))
## create new variable(s)
tuango_post <- mutate(tuango_post, mon_iq = xtile(monetary, 5, rev = TRUE))
## create new variable(s)
tuango_post <- mutate(tuango_post, rfm_iq = paste0(rec_iq, freq_iq, mon_iq))
## create new variable(s)
tuango_post <- group_by(tuango_post, rec_iq) %>%
  mutate(freq_sq = xtile(frequency, 5, rev = TRUE)) %>%
  ungroup()
## create new variable(s)
tuango_post <- group_by(tuango_post, rec_iq, freq_sq) %>%
  mutate(mon_sq = xtile(monetary, 5, rev = TRUE)) %>%
  ungroup()
tuango_post <- mutate(tuango_post, rfm_sq = paste0(rec_iq, freq_sq, mon_sq))

rev <- mean(data.matrix(tuango_post[tuango_post$buyer == 'yes' & tuango_post$training == 1, "ordersize"]))
breakeven <- 2.5/(rev*0.5)

temp <- tuango_post %>%
  filter(training == 1) %>%
  group_by(rfm_iq) %>%
  summarise(mailto_iq = mean(buyer == "yes") > breakeven) %>%
  ungroup()
tuango_post <- tuango_post %>%
  left_join(temp, by = c("rfm_iq" = "rfm_iq"))
## create new variable(s)
temp <- tuango_post %>%
  filter(training == 1) %>%
  group_by(rfm_sq) %>%
  summarise(mailto_sq = mean(buyer == "yes") > breakeven) %>%
  ungroup()
tuango_post <- tuango_post %>%
  left_join(temp, by = c("rfm_sq" = "rfm_sq"))

perf_calc <- function(x){
  tuango_post %>%
    filter(training == 0, tuango_post[, x] == TRUE) %>%
    summarise(
      population = n(),
      expenditure = population * 2.5,
      rep_rate = mean(buyer == "yes"),
      #nr_resp = nr_mail * rep_rate,
      # profit = mean(ordersize) * nr_resp - mail_cost,
      profit = 0.5 * sum(ordersize) - expenditure,
      ROME = profit / expenditure,
    )
}

profit_iq <- perf_calc('mailto_iq')[4]
print(profit_iq)
ROME_iq <- perf_calc('mailto_iq')[5]
print(ROME_iq)
profit_sq <- perf_calc('mailto_sq')[4]
print(profit_sq)
ROME_sq <- perf_calc('mailto_sq')[5]
print(ROME_sq)

Breakeven <- tuango_post %>%
  filter(buyer == "yes" & training == 1) %>%
  group_by(rfm_sq) %>%
  summarise(breakeven_each = 2.5 / (mean(ordersize)*0.5)) %>%
  ungroup()

Breakeven <- tuango_post %>%
  distinct(rfm_sq) %>%
  left_join(Breakeven, by=c("rfm_sq"="rfm_sq"))

tuango_post <- tuango_post %>%
  left_join(Breakeven, by = "rfm_sq")

tuango_post$breakeven_each <- tuango_post$breakeven_each %>% replace_na(1)

temp <- tuango_post %>%
  filter(training == 1) %>%
  group_by(rfm_sq) %>%
  summarise(mailto10_sq= mean(buyer == "yes") > mean(breakeven_each)) %>%
  ungroup()
tuango_post <- tuango_post %>%
  left_join(temp, by = c("rfm_sq" = "rfm_sq"))

profit_sq_cell <- perf_calc('mailto10_sq')[4]
ROME_sq_cell <- perf_calc('mailto10_sq')[5]


temp <- tuango_post %>%
  filter(training == 1) %>%
  group_by(rfm_iq) %>%
  summarise(mailto_lbiq = (mean(buyer == "yes") - 1.64 * seprop(buyer == "yes")) > breakeven) %>%
  ungroup()
tuango_post <- tuango_post %>%
  left_join(temp, by = c("rfm_iq" = "rfm_iq"))
## create new variable(s)
temp <- tuango_post %>%
  filter(training == 1) %>%
  group_by(rfm_sq) %>%
  summarise(mailto_lbsq = (mean(buyer == "yes") - 1.64 * seprop(buyer == "yes")) > breakeven) %>%
  ungroup()
tuango_post <- tuango_post %>%
  left_join(temp, by = c("rfm_sq" = "rfm_sq"))

profit_lbiq <- perf_calc('mailto_lbiq')[4]
ROME_lbiq <- perf_calc('mailto_lbiq')[5]
profit_lbsq <- perf_calc('mailto_lbsq')[4]
ROME_lbsq <- perf_calc('mailto_lbsq')[5]

results <- tibble::tibble(
  name = c("Indep. RFM", "Seq. RFM", "Seq. RFM with each BE", "Indep. lb RFM", "Seq. lb RFM"),
  Profit = c(as.numeric(profit_iq), as.numeric(profit_sq), as.numeric(profit_sq_cell), as.numeric(profit_lbiq), as.numeric(profit_lbsq)),
  ROME = c(as.numeric(ROME_iq), as.numeric(ROME_sq), as.numeric(ROME_sq), as.numeric(ROME_lbiq), as.numeric(ROME_lbsq))) %>%
  mutate(name = factor(name, levels = name))

results %>%
  ggplot(aes(x = name, y = Profit, fill = name)) + geom_bar(stat = "identity") + geom_text(aes(label = round(Profit, 2)), size = 3)

results %>%
  ggplot(aes(x = name, y = ROME, fill = name)) + geom_bar(stat = "identity") + geom_text(aes(label = round(ROME, 2)), size = 3)

