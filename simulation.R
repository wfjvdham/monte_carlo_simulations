library(tidyverse)

business <- tibble(
  id = c(1, 2, 3, 4),
  name = c("savings", "apple", "computers", "cars"),
  cost = c(0, 5, 50, 500),
  income = c(1, 1, 10, 100)
)

money <- tibble(
  name = c("A", "B"),
  amount = c(25, 50),
  round = c(0, 0)
)

properties <- tibble(
  name = "A",
  id = 1
)

current_round <- 1
current_player <- "A"
coin = "norm"

set_current_player <- function(cp) {
  if(cp == "A") {
    "B"
  } else {
    "A"
  }
} 

update_state <- function(cp, cr) {
  current_money <- money %>%
    filter(name == cp, round == cr - 1) %>%
    select(amount) %>%
    pull(1)
  business_to_buy <- business %>%
    filter(cost <= current_money) %>%
    select(id) %>%
    tail(1) %>%
    pull(1)
  cost <- 0
  if (length(business_to_buy) != 0) {
    cost <- business %>%
      filter(id == business_to_buy) %>%
      select(cost) %>%
      pull(1)
    new_property <- list(cp, business_to_buy)
      properties <<- properties %>%
        rbind(new_property)
  }
  if (coin == "normal") {
    total_income <- properties %>%
      filter(name == cp) %>%
      left_join(business, by = "id") %>%
      summarize(sum(income)) %>%
      pull(1)
  } else {
    
  }
  #new_money <- list(cp, current_money - cost + total_income, cr)
  if (coin == "normal") {
    new_money <- list(cp, current_money * 1.04, cr)
  } else {
    potatial_gain <- current_money * 0.04
    #distribution is mu = 100 sd = 10
    if (potatial_gain > 100) {
      factor <- max((potatial_gain - 100) / 10, 1)
      gain <- potatial_gain / factor
    } else {
      factor <- max((100 - potatial_gain) / 10, 1)
      gain <- potatial_gain * factor
    }
    new_money <- list(cp, current_money + gain, cr)
  }
  money <<- money %>%
    rbind(new_money)
}

show_plots <- function() {
  ggplot(money) +
    geom_line(aes(round, amount, color = name))
}

do_turn <- function(x) {
  update_state(current_player, current_round)
  if (current_player == "B") {
    current_round <<- current_round + 1
  }
  current_player <<- set_current_player(current_player)
}

1:2000 %>%
  walk(do_turn)
show_plots()
