
# Prepare work environment #####################################################

# clean everything done before
rm(list = ls())

# delete plots
graphics.off()

# install pacman if it is not installed
if (!require("pacman")) install.packages("pacman")

# install packages if they are not installed
# and load them
p_load(
  readxl,
  tidyverse,
  scales,
  quantmod,
  ggpubr,
  here,
  janitor,
  broom,
  lubridate,
  tidyquant,
  corrr,
  Hmisc
)

# Get data #####################################################################

# load discounts data
discount <- read_excel(here("discount.xlsx"))

# load ITSA4 stock prices
itsa <-
  getSymbols(
    "ITSA4.SA",
    auto.assign = FALSE,
    src = "yahoo",
    from = "2005-12-01",
    to = Sys.Date()
  ) |>
  tidy() |>
  pivot_wider(names_from = "series", values_from = "value") |>
  clean_names() |>
  rename(date = index)

# load ITUB4 stock prices
itub <-
  getSymbols(
    "ITUB4.SA",
    auto.assign = FALSE,
    src = "yahoo",
    from = "2005-12-01",
    to = Sys.Date()
  ) |>
  tidy() |>
  pivot_wider(names_from = "series", values_from = "value") |>
  clean_names() |>
  rename(date = index)

# get last ITSA4 stock price
itsa_today <-
  itsa |>
  filter(date == last(date)) |>
  pull(itsa4_sa_adjusted)

# get last ITUB4 stock price
itub_today <-
  itub |>
  filter(date == last(date)) |>
  pull(itub4_sa_adjusted)

# Manipulate data ##############################################################

# merge stocks
merge <-
  itsa |>
  select(date, itsa = itsa4_sa_adjusted) |>
  left_join(itub |> select(date, itub = itub4_sa_adjusted))

# Explore data #################################################################

## Discounts ###################################################################

# summary statistics
summary(discount$discount)

# plot distribution
ggplot(data = discount, aes(x = discount)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .15, fill = "#FF6666") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Discount",
       y = "Count",
       title = "Discount's Distribution") +
  geom_vline(
    xintercept = mean(discount$discount),
    linetype = "dotted",
    size = 1,
    color = "royalblue4"
  ) +
  geom_vline(
    xintercept = quantile(discount$discount, .25),
    linetype = "dotted",
    size = 1,
    color = "royalblue4"
  ) +
  geom_vline(
    xintercept = quantile(discount$discount, .75),
    linetype = "dotted",
    size = 1,
    color = "royalblue4"
  ) +
  geom_vline(
    xintercept = last(discount$discount),
    linetype = "solid",
    size = 1,
    color = "red"
  ) +
  annotate(
    "text",
    label = "Mean",
    y = 16,
    x = mean(discount$discount) + .005
  ) +
  annotate(
    "text",
    label = "1st Quantile",
    y = 16,
    x = quantile(discount$discount, .25) - .010
  ) +
  annotate(
    "text",
    label = "3rd Quantile",
    y = 16,
    x = quantile(discount$discount, .75) + .010
  ) +
  annotate(
    "text",
    label = "Actual",
    y = 1,
    x = last(discount$discount) - .005,
    fontface = "bold",
    color = "royalblue4"
  )

# plot time series
discount |>
  mutate(color = case_when(
    discount > mean(discount) + sd(discount) ~ "Green",
    discount < mean(discount) - sd(discount) ~ "Red",
    TRUE                                     ~ "Orange"
  )) |>
  ggplot(aes(
    x = date,
    y = discount,
    color = color,
    group = 1
  )) +
  geom_line(size = 1) +
  scale_color_manual(values = c("green3", "orange", "red")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = mean(discount$discount),
             linetype = "dashed",) +
  geom_hline(
    yintercept = mean(discount$discount) + sd(discount$discount),
    linetype = "dashed",
  ) +
  geom_hline(
    yintercept = mean(discount$discount) - sd(discount$discount),
    linetype = "dashed",
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Date",
       y = "Discount",
       title = "Holding's discount in relation to shares \nheld in investee companies")

# view high discounts
discount |>
  filter(discount >= mean(discount) + sd(discount)) |>
  arrange(date) |>
  print(n = Inf)

# dates that I have bought ITSA4
buy_date <-
  c(
    "2020-07-08",
    "2020-08-24",
    "2020-09-22",
    "2020-10-21",
    "2022-01-06",
    "2022-05-02",
    "2022_05-09",
    "2022_06-30"
  ) |>
  as_date()

# months that I have bought ITSA4
buy_month <- ceiling_date(buy_date, "month") - days(1)

# check my stock purchases in relation to discounts
discount |>
  mutate(
    lower = mean(discount) - sd(discount),
    mean = mean(discount),
    median = median(discount),
    upper = mean(discount) + sd(discount),
    color = case_when(
      discount > mean(discount) + sd(discount) ~ "Green",
      discount < mean(discount) - sd(discount) ~ "Red",
      TRUE                                     ~ "Orange"
    ),
    date = as_date(date)
  ) |>
  filter(date %in% buy_month)

## Correlation ITUB4 and ITSA4 #################################################

### All time ###################################################################

merge |>
  tq_mutate(
    select = itsa,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "itsa_return"
  ) |>
  tq_mutate(
    select = itub,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "itub_return"
  ) |>
  select(itsa = itsa_return,
         itub = itub_return) |>
  ggscatter(
    x = "itsa",
    y = "itub",
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE,
    alpha = .1,
  ) +
  stat_cor(method = "pearson",
           label.x = 0,
           label.y = .25) +
  labs(x = "ITSA4",
       y = "ITUB4",
       title = "Scatterplot ITSA4 versus ITUB4 daily returns")

### Rolling correlation ########################################################

# build function
mycor <- function(x) cor(x[, 1], x[, 2])

# apply function
correlation <-
  merge |>
  tq_mutate(
    select = itsa,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "itsa_return"
  ) |>
  tq_mutate(
    select = itub,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "itub_return"
  ) |>
  select(date,
         itsa = itsa_return,
         itub = itub_return) |>
  mutate(cor = rollapplyr(
    cbind(itsa, itub),
    252,
    mycor,
    by.column = FALSE,
    fill = NA
  )) |>
  drop_na() |>
  filter(date >= "2010-01-01")

# plot rolling correlation
ggplot(data = correlation, aes(x = date, y = cor)) +
  geom_line(color = "royalblue4") +
  labs(x = "Date",
       y = "Correlation",
       title = "Rolling Correlation between ITSA4 and ITUB4 daily returns")

## Compare ITSA4 discounts with returns from ITSA4 against ITUB4 ###############

# join discounts and stock prices data sets
# calculate cumulative returns
# compare cumulative returns from ITSA4 and ITUB4
complete <-
  merge |>
  group_by(year = year(date), month = month(date)) |>
  filter(date == max(date)) |>
  mutate(date = as.yearmon(date)) |>
  ungroup() |>
  select(-c(year, month)) |>
  left_join(discount |> mutate(date = as.yearmon(date))) |>
  mutate(
    itsa_cum_return = (itsa_today / itsa) - 1,
    itub_cum_return = (itub_today / itub) - 1,
    itsa_higher = case_when(
      itsa_cum_return > itub_cum_return ~ "yes",
      TRUE                              ~ "no")
  ) |>
  drop_na()
complete

# get summary statistics by variable "itsa_higher"
complete |>
  group_by(itsa_higher) |>
  summarise(
    n = n(),
    min = min(discount),
    mean = mean(discount),
    median = median(discount),
    max = max(discount)
  ) |>
  mutate(pct = n / sum(n)) |>
  relocate(last_col(), .after = n)

# box plot by variable "itsa_higher"
ggplot(data = complete,
       aes(x = itsa_higher, y = discount, fill = itsa_higher)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "",
    y = "Discount",
    title = "Are ITSA4 returns higher than ITUB4 returns?",
    subtitle = "Boxplot when yes or no"
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red3", "green4"))

# density of difference on returns
complete |>
  mutate(itsa_higher = str_to_title(itsa_higher)) |>
  ggplot(aes(x = discount, fill = itsa_higher)) +
  geom_density(alpha = .3) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Discount",
    y = "Density",
    title = "Are ITSA4 returns higher than ITUB4 returns?",
    subtitle = "Distribution when yes or no",
    fill = "ITSA4 higher?"
  ) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("red", "green3"))

# histogram of difference on returns
complete |>
  mutate(itsa_higher = str_to_title(itsa_higher)) |>
  ggplot(aes(x = discount, fill = itsa_higher)) +
  geom_histogram(alpha = .4, color = "black") +
  facet_wrap(~ itsa_higher) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Discount",
       y = "Count",
       title = "Returns comparison's distribution") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red", "green3"))

# plot probability of ITSA4 returns be higher than ITUB4 returns
complete |>
  mutate(discount_group = cut2(discount, m = 30)) |>
  group_by(discount_group) |>
  summarise(prob = sum(itsa_higher == "yes") / length(itsa_higher)) |>
  ggplot(aes(y = prob, x = discount_group)) +
  geom_col(fill = "royalblue4", width = .5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Probability",
       y = "Discount range",
       title = "Probability of ITSA4 > ITUB4 returns") +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  geom_text(aes(label = percent_format(accuracy = 1)(prob)),
            hjust = 1.2,
            color = "white")

# plot time series
complete |>
  mutate(itsa_higher = str_to_title(itsa_higher)) |>
  ggplot(aes(
    x = date,
    y = discount,
    color = itsa_higher,
    group = 1
  ),
  size = 1) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Discount",
    title = "Discounts time series",
    subtitle = "when ITSA4 > ITUB4 returns (or not)"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c("red", "green3"))
