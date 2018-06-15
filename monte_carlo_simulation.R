library(tidyverse)

# --- user input
n_iterations <- 2000
n_paths <- 200
tax <- T
tax_limit <- Inf
tax_percentage <- 0.03
income_percentage <- 0.031
ubi <- F
give_back_percentage <- 0.1
ethicoin <- F
# ---

data <- 1:n_paths %>%
  map(list(1)) %>%
  setNames(1:n_paths)

x <- list(data, 0)

next_step <- function(data) {
  x <- data[[1]]
  prev_total_tax <- data[[2]]
  total_tax <- 0
  for (i in 1:length(x)) {
    old <- x[[i]][[length(x[[i]])]]
    new <- old * (1 + rnorm(1, income_percentage, 0.05))
    if (tax) {
      tax_amount <- old * tax_percentage
      if (old >= tax_limit) {
        tax_amount <- tax_limit * tax_percentage
      }
      total_tax <- total_tax + tax_amount
      new <- new - tax_amount 
      if (ubi) {
        share <- prev_total_tax[[length(prev_total_tax)]] / n_paths
        new <- new + (give_back_percentage * share)
      }
    }
    if (ethicoin) {
      income <- old * rnorm(1, income_percentage, 0.05)
      if (income > 0) {
        tax_amount <- tax_percentage * income
        total_tax <- total_tax + tax_amount
        new <- old + income - tax_amount
      } else {
        new <- old + income
      }
    }
    x[[i]] <- append(x[[i]], new)
  }
  list(x, append(prev_total_tax, total_tax))
}

for (i in 1:n_iterations) {
  x <- next_step(x)
}

tax <- tibble(
  i = 1:(n_iterations + 1),
  total_tax = x[[2]]
) %>%
  slice(2:n_iterations)

df <- x[[1]] %>%
  stack() %>%
  mutate(i = rep(1:(n_iterations + 1), length(x[[1]])))

df %>%
  filter(ind %in% sample(1:n_paths, min(10, n_paths))) %>%
  ggplot() +
  geom_line(aes(i, values, colour = ind)) +
  geom_hline(yintercept = tax_limit) +
  geom_line(data = tax, aes(i, total_tax))

final <- df %>%
  filter(i == n_iterations + 1)

ggplot(final) +
  geom_histogram(aes(values), bins = 40) +
  geom_vline(xintercept = tax_limit) +
  geom_vline(xintercept = mean(final$values), linetype = 3) +
  geom_vline(xintercept = median(final$values), linetype = 4)

min(final$values)
max(final$values)
