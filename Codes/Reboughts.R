
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
  rvest,
  tidyverse,
  janitor,
  scales,
  broom,
  quantmod,
  lubridate
)

# Get data #####################################################################

# store url
url <- "https://www.itausa.com.br/Recompra-de-Acoes-Proprias"

# read table - on stocks volume
on_raw  <- 
  read_html(url) |>
  html_table() |>
  pluck(1) |>
  clean_names()
on_raw

# read table - pn stocks volume
pn_raw  <- 
  read_html(url) |>
  html_table() |>
  pluck(2) |>
  clean_names()
pn_raw

# load ITSA4 stock prices
itsa <-
  getSymbols(
    "ITSA4.SA",
    auto.assign = FALSE,
    src = "yahoo",
    from = "2005-01-01",
    to = Sys.Date()
  ) |>
  tidy() |>
  pivot_wider(names_from = "series", values_from = "value") |>
  clean_names() |>
  rename(date = index)
itsa

# Manipulate data ##############################################################

# join on and pn volume stocks data
total <- 
  on_raw |>
  select(year = periodo, on = volume_negociado) |>
  filter(!year == "2006 a 2013") |>
  mutate(year = as.integer(year)) |>
  bind_rows(tibble(
    year = seq(from = 2006, to = 2013),
    on = NA
    )) |>
  left_join(pn_raw |> select(year = periodo, pn = volume_negociado)) |>
  mutate(across(on:pn, ~str_remove_all(., "\\."))) |>
  mutate(across(on:pn, as.numeric)) |>
  mutate(across(on:pn, ~replace_na(., 0))) |>
  mutate(total = on + pn) |>
  arrange(year)

# get mean stocks values per year
price <-
  itsa |>
  select(date, price = itsa4_sa_adjusted) |>
  group_by(year = year(date)) |>
  summarise(price = mean(price))

# join buybacks volume and stock prices
volume_price <- 
  total |>
  select(year, value = total) |>
  mutate(variable = "Volume") |>
  bind_rows(
    price |>
      rename(value = price) |>
      mutate(variable = "Price")
  )

# Visualize data ###############################################################

# plot buybacks
total |>
  ggplot(aes(y = total, x = factor(year))) +
  geom_col(width = .5, fill = "royalblue4") +
  labs(x = "Ano",
       y = "Volume negociado",
       title = "Itaúsa: recompra anual de ações") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  scale_y_continuous(label = label_number(scale_cut = cut_long_scale()))

# plot stock prices
price |>
  ggplot(aes(x = factor(year), y = price, group = 1)) +
  geom_point(color = "red", size = 2) +
  geom_line(color = "royalblue4") +
  labs(x = "Ano",
       y = "Cotação média ajustada",
       title = "ITSA4: cotação média anual ajustada") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

# plot volume buybacks and annual average stock prices
ggplot() +
  geom_col(data = volume_price |> filter(variable == "Volume"),
           aes(x = factor(year), y = value),
           width = .3, fill = "royalblue4") +
  geom_line(data = volume_price |> filter(variable == "Price"),
            aes(x = factor(year), y = value, group = 1),
            color = "green4") +
  geom_point(data = volume_price |> filter(variable == "Price"),
             aes(x = factor(year), y = value),
             color = "red") +
  facet_wrap( ~ variable, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = label_number()) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        strip.background = element_rect(fill = "turquoise"),
        strip.text = element_text(face = "bold")) +
  labs(x = "Ano",
       y = "",
       title = "Average annual stock prices (ITSA4) and buybacks volume")
  
  