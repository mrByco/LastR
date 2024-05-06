# Import necessary libraries
library(readr)
library(ggplot2)
library(ggpubr)
library(dplyr)

# Set the correct stock ticker and year
stock_ticker <- ar[ai+1]
year <- ev

# Generate URL for data download
url <- paste0("https://query1.finance.yahoo.com/v7/finance/download/", 
              stock_ticker, 
              "?period1=", as.integer(as.Date(paste0(year, "-01-01"))), 
              "&period2=", as.integer(as.Date(paste0(year, "-12-31"))), 
              "&interval=1d&events=history&includeAdjustedClose=true")

# Download the data
stock_data <- read.csv(url)

# Calculate log returns
stock_data <- stock_data %>% 
  mutate(log_return = log(Close / lag(Close)))

# Remove NA values that appear from the lag function
stock_data <- na.omit(stock_data)

# Plot density of log returns
ggdensity(stock_data$log_return, 
          xlab = "Log Returns", 
          title = paste("Density of Log Returns for", stock_ticker, "in", year))

# Statistical Test: Chi-square Goodness-of-Fit Test for Normal Distribution
mu <- mean(stock_data$log_return)
sigma <- sd(stock_data$log_return)
observed <- table(cut(stock_data$log_return, breaks = "Sturges"))
expected <- diff(pnorm(c(-Inf, breaks, Inf), mean = mu, sd = sigma)) * nrow(stock_data)
chi_sq_test <- chisq.test(x = observed, p = expected)

# Print test results
print(chi_sq_test)

# Show plot
ggsave("log_returns_density.png")
