install.packages("lubridate")
install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)

data <- read.csv("C:\\Users\\mohan\\Downloads\\2.ecommerce_transactions.csv")

head(data)

ggplot(data, aes(x = Transaction_Amount)) +
  geom_histogram(
    binwidth = 500,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Histogram of Transaction Amounts",
    x = "Transaction Amount",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(data, aes(y = TransactionAmount)) +
  geom_boxplot(
    fill = "orange",
    outlier.color = "red",
    outlier.size = 2
  ) +
  labs(
    title = "Boxplot of Transaction Amounts",
    y = "Transaction Amount"
  ) +
  theme_minimal()

data$Transaction_Date <- as.Date(data$Transaction_Date)

data$Month <- format(data$Transaction_Date, "%b")
data$Year <- format(data$Transaction_Date, "%Y")

monthly_sales <- data %>%
  group_by(Month, Year) %>%
  summarise(
    TotalSales = sum(Transaction_Amount),
    .groups = "drop"
  )

ggplot(monthly_sales, aes(x = Year, y = Month, fill = TotalSales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Heatmap of Monthly Sales Intensity",
    x = "Year",
    y = "Month"
  ) +
  theme_minimal()
