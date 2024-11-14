# Load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Read the CSV data
df <- read.csv('C:/Users/Administrator/Downloads/stud.csv')

# Show Top 5 records
head(df)

# Check the shape of the dataset
dim(df)

# Check for missing values
colSums(is.na(df))

# Check for duplicates
sum(duplicated(df))

# Check data types
str(df)

# Check the number of unique values for each column
sapply(df, function(x) length(unique(x)))

# Summary statistics for numerical columns
summary(df[, c('math_score', 'reading_score', 'writing_score')])

# Exploring Data: Categorical values
cat("Categories in 'gender' variable: ", unique(df$gender), "\n")
cat("Categories in 'race_ethnicity' variable: ", unique(df$race_ethnicity), "\n")
cat("Categories in 'parental_level_of_education' variable: ", unique(df$parental_level_of_education), "\n")
cat("Categories in 'lunch' variable: ", unique(df$lunch), "\n")
cat("Categories in 'test_preparation_course' variable: ", unique(df$test_preparation_course), "\n")

# Define numerical & categorical columns
numeric_features <- names(df)[sapply(df, is.numeric)]
categorical_features <- names(df)[sapply(df, is.factor)]

cat("We have", length(numeric_features), "numerical features: ", numeric_features, "\n")
cat("We have", length(categorical_features), "categorical features: ", categorical_features, "\n")

# Add columns for Total Score and Average
df$total_score <- df$math_score + df$reading_score + df$writing_score
df$average <- df$total_score / 3
head(df)

# Calculate number of students with full marks
math_full <- sum(df$math_score == 100)
writing_full <- sum(df$writing_score == 100)
reading_full <- sum(df$reading_score == 100)

cat("Number of students with full marks in Maths:", math_full, "\n")
cat("Number of students with full marks in Writing:", writing_full, "\n")
cat("Number of students with full marks in Reading:", reading_full, "\n")

# Calculate number of students with less than 20 marks
reading_less_20 <- sum(df$reading_score <= 20)
writing_less_20 <- sum(df$writing_score <= 20)
math_less_20 <- sum(df$math_score <= 20)

cat("Number of students with less than 20 marks in Maths:", math_less_20, "\n")
cat("Number of students with less than 20 marks in Writing:", writing_less_20, "\n")
cat("Number of students with less than 20 marks in Reading:", reading_less_20, "\n")

# Visualize the data
# Histogram and KDE for average score distribution
par(mfrow = c(1, 2))
ggplot(df, aes(x = average)) + 
  geom_histogram(bins = 30, fill = 'green', color = 'black', alpha = 0.7) +
  geom_density(color = "blue") +
  ggtitle("Average Score Distribution") +
  theme_minimal()

ggplot(df, aes(x = average, color = gender)) +
  geom_density() +
  ggtitle("Average Score by Gender") +
  theme_minimal()

# Distribution of total scores
par(mfrow = c(1, 2))
ggplot(df, aes(x = total_score)) +
  geom_histogram(bins = 30, fill = 'green', color = 'black', alpha = 0.7) +
  geom_density(color = "blue") +
  ggtitle("Total Score Distribution") +
  theme_minimal()

ggplot(df, aes(x = total_score, color = gender)) +
  geom_density() +
  ggtitle("Total Score by Gender") +
  theme_minimal()

# Explore lunch type with average score distribution
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
ggplot(df, aes(x = average, color = lunch)) +
  geom_density() +
  ggtitle("Average Score by Lunch Type")

ggplot(df[df$gender == "female", ], aes(x = average, color = lunch)) +
  geom_density() +
  ggtitle("Average Score by Lunch Type (Female)")

ggplot(df[df$gender == "male", ], aes(x = average, color = lunch)) +
  geom_density() +
  ggtitle("Average Score by Lunch Type (Male)")

# Explore parental education level with average score distribution
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
ggplot(df, aes(x = average, color = parental_level_of_education)) +
  geom_density() +
  ggtitle("Average Score by Parental Education")

ggplot(df[df$gender == "male", ], aes(x = average, color = parental_level_of_education)) +
  geom_density() +
  ggtitle("Average Score by Parental Education (Male)")

ggplot(df[df$gender == "female", ], aes(x = average, color = parental_level_of_education)) +
  geom_density() +
  ggtitle("Average Score by Parental Education (Female)")

# Explore ethnicity with average score distribution
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2))
ggplot(df, aes(x = average, color = race_ethnicity)) +
  geom_density() +
  ggtitle("Average Score by Ethnicity")

ggplot(df[df$gender == "female", ], aes(x = average, color = race_ethnicity)) +
  geom_density() +
  ggtitle("Average Score by Ethnicity (Female)")

ggplot(df[df$gender == "male", ], aes(x = average, color = race_ethnicity)) +
  geom_density() +
  ggtitle("Average Score by Ethnicity (Male)")

# Violin plots for subject scores
# Set up the plotting layout
par(mfrow = c(1, 4), mar = c(5, 4, 4, 2))

# Violin plot for math scores by gender
ggplot(df, aes(x = gender, y = math_score)) + 
  geom_violin(fill = 'red') +
  ggtitle("Math Scores")

# Violin plot for reading scores by gender
ggplot(df, aes(x = gender, y = reading_score)) + 
  geom_violin(fill = 'blue') +
  ggtitle("Reading Scores")

# Violin plot for writing scores by gender
ggplot(df, aes(x = gender, y = writing_score)) + 
  geom_violin(fill = 'purple') +
  ggtitle("Writing Scores")

