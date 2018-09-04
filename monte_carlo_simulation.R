library(tidyverse)
library(config)

Sys.setenv(R_CONFIG_ACTIVE = "default")
#no tax no ubi leads to inequality
Sys.setenv(R_CONFIG_ACTIVE = "with_tax")
#equal tax for everybody results in equality
Sys.setenv(R_CONFIG_ACTIVE = "with_tax_limit")
#tax with a max results in inequality
Sys.setenv(R_CONFIG_ACTIVE = "ubi")
#introducing ubi does not solve this
Sys.setenv(R_CONFIG_ACTIVE = "ubi_no_tax_limit")
#ubi without tax limit works but it is the Inf tax limit that solves the problem
config <- get()

data <- 1:config$n_paths %>%
  map(list(1)) %>%
  setNames(1:config$n_paths)

x <- list(data, 0)

next_step <- function(data) {
  x <- data[[1]]
  prev_total_tax <- data[[2]]
  total_tax <- 0
  for (i in 1:length(x)) {
    old <- x[[i]][[length(x[[i]])]]
    new <- old * (1 + rnorm(1, config$income_percentage, 0.05))
    if (config$tax) {
      tax_amount <- old * config$tax_percentage
      if (old >= config$tax_limit) {
        tax_amount <- config$tax_limit * config$tax_percentage
      }
      total_tax <- total_tax + tax_amount
      new <- new - tax_amount 
      if (config$ubi) {
        share <- prev_total_tax[[length(prev_total_tax)]] / config$n_paths
        new <- new + (config$give_back_percentage * share)
      }
    }
    x[[i]] <- append(x[[i]], new)
  }
  list(x, append(prev_total_tax, total_tax))
}

for (i in 1:config$n_iterations) {
  x <- next_step(x)
}

tax_values <- tibble(
  i = 1:(config$n_iterations + 1),
  total_tax = x[[2]]
) %>%
  slice(2:config$n_iterations)

df <- x[[1]] %>%
  stack() %>%
  mutate(i = rep(1:(config$n_iterations + 1), length(x[[1]])))

df %>%
  filter(ind %in% sample(1:config$n_paths, min(10, config$n_paths))) %>%
  ggplot() +
  geom_line(aes(i, values, colour = ind)) +
  geom_hline(yintercept = config$tax_limit) +
  geom_line(data = tax_values, aes(i, total_tax))

final <- df %>%
  filter(i == config$n_iterations + 1)

ggplot(final) +
  geom_histogram(aes(values), bins = 40) +
  geom_vline(xintercept = config$tax_limit) +
  geom_vline(xintercept = mean(final$values), linetype = 3) +
  geom_vline(xintercept = median(final$values), linetype = 4)
