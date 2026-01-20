library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/mohan/Downloads/1.student_performance.csv")

head(data)
str(data)

colSums(is.na(data))

data$Subject <- as.factor(data$Subject)
data$Department <- as.factor(data$Department)
data$Semester <- as.factor(data$Semester)
data$Final_Grade <- as.factor(data$Final_Grade)

subject_avg <- data %>%
  mutate(Average_Marks = (Internal_Test1 + Internal_Test2) / 2) %>%
  group_by(Subject) %>%
  summarise(Average_Marks = mean(Average_Marks))

ggplot(subject_avg, aes(x = Subject, y = Average_Marks, fill = Subject)) +
  geom_bar(stat = "identity") +
  labs(title = "Subject-wise Average Marks",
       x = "Subject",
       y = "Average Marks") +
  theme_minimal()

test_trend <- data %>%
  summarise(
    Test1_Avg = mean(Internal_Test1),
    Test2_Avg = mean(Internal_Test2)
  )

test_trend_long <- data.frame(
  Test = c("Internal Test 1", "Internal Test 2"),
  Average_Marks = c(test_trend$Test1_Avg, test_trend$Test2_Avg)
)

ggplot(test_trend_long, aes(x = Test, y = Average_Marks, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance Trend Across Internal Tests",
       x = "Test",
       y = "Average Marks") +
  theme_minimal()

grade_dist <- data %>%
  count(Final_Grade)

ggplot(grade_dist, aes(x = "", y = n, fill = Final_Grade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Final Grade Distribution") +
  theme_void()
