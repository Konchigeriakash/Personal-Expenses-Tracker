# Personal Expenses Tracker Analysis

# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Read dataset
data <- read.csv("New Csv file with null values included.csv", stringsAsFactors = FALSE)

# Explore data
head(data)
str(data)
summary(data)

# Data cleaning
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")
colSums(is.na(data))
data <- na.omit(data)

# Category-wise spending
category_data <- data %>%
  group_by(Category) %>%
  summarise(Total = sum(Amount), .groups="drop")

ggplot(category_data, aes(Category, Total)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Category-wise Spending",
       x="Category",
       y="Total Expense")

# Daily trend
daily_data <- data %>%
  group_by(Date) %>%
  summarise(Total=sum(Amount), .groups="drop")

ggplot(daily_data, aes(Date, Total)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title="Daily Expense Trend")

# Distribution
ggplot(data, aes(Amount)) +
  geom_histogram(bins=20) +
  theme_minimal()

# Summary statistics
total_expense <- sum(data$Amount)
average_expense <- mean(data$Amount)
min_expense <- min(data$Amount)
max_expense <- max(data$Amount)

print(total_expense)
print(average_expense)
print(min_expense)
print(max_expense)

# Category summary
category_summary <- data %>%
  group_by(Category) %>%
  summarise(Total=sum(Amount), .groups="drop") %>%
  arrange(desc(Total))

print(category_summary)

# Payment mode summary
payment_summary <- data %>%
  group_by(Payment_Mode) %>%
  summarise(Total=sum(Amount), Count=n(), .groups="drop") %>%
  arrange(desc(Total))

print(payment_summary)

# Highest spending day
daily_summary <- data %>%
  group_by(Date) %>%
  summarise(Total=sum(Amount), .groups="drop") %>%
  arrange(desc(Total))

print(daily_summary[1,])

# Linear regression prediction
data$Date_numeric <- as.numeric(data$Date)
model <- lm(Amount ~ Date_numeric, data=data)

future_dates <- data.frame(
  Date_numeric = seq(max(data$Date_numeric)+1, by=1, length.out=5)
)

predictions <- predict(model, future_dates)
print(predictions)
