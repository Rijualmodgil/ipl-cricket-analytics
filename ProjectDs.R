# DATA VISUALISATION OF PLAYERS PLAYING IN IPL 2025

# Loading libraries
library(readr)
library(dplyr)
library(ggplot2)

# Loading the data
iplData <- read_csv("C:/Users/rijua/OneDrive/Documents/cricket_data.csv", col_names = TRUE, skip = 0)

# Viewing First Few Rows
head(iplData)

# Data Pre-processing

# Check Datatypes of all columns
sapply(iplData, class)

# Remove rows where any column has "No stats" 
iplData <- iplData %>%
  filter(if_all(everything(), ~ . != "No stats"))

# Now safe to convert columns to numeric
iplData$Matches_Batted <- as.numeric(iplData$Matches_Batted)
iplData$Runs_Scored <- as.numeric(iplData$Runs_Scored)
iplData$Balls_Faced <- as.numeric(iplData$Balls_Faced)
iplData$Batting_Strike_Rate <- as.numeric(iplData$Batting_Strike_Rate)
iplData$Wickets_Taken <- as.numeric(iplData$Wickets_Taken)


head(iplData)

# Data Visualization

# Top 10 Run Scorers in IPL
Highest_scorers <- iplData %>%
  group_by(Player_Name) %>%
  summarise(Runs_Scored = sum(Runs_Scored, na.rm = TRUE)) %>%
  arrange(desc(Runs_Scored)) %>%
  slice_head(n = 10) 

Highest_scorers

# Bar representation of Top Run Scorers
ggplot(Highest_scorers, aes(x = reorder(Player_Name, Runs_Scored), y = Runs_Scored, fill = Player_Name)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Top 10 Run Scorers in IPL", x = "Player", y = "Runs Scored") +
  theme_minimal() 

# Batting Strike Rate Distribution
ggplot(iplData, aes(x = Batting_Strike_Rate)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Batting Strike Rate Distribution", x = "Strike Rate", y = "Count") +
  theme_minimal()

# Runs vs Balls Faced (Top 20 Players)
top_players <- iplData %>%
  group_by(Player_Name) %>%
  summarise(Runs_Scored = sum(Runs_Scored, na.rm = TRUE),
            Balls_Faced = sum(Balls_Faced, na.rm = TRUE)) %>%
  arrange(desc(Runs_Scored)) %>%
  slice_head(n = 20)

top_players

# Point representation of these 20 Players
ggplot(top_players, aes(x = Balls_Faced, y = Runs_Scored, color = Player_Name)) +
  geom_point(size = 3) +
  labs(title = "Runs Scored vs Balls Faced (Top 20 Players)", x = "Balls Faced", y = "Runs Scored") +
  theme_minimal() 

# Wicket Taking Performance
top_wicket_takers <- iplData %>%
  group_by(Player_Name) %>%
  summarise(Wickets_Taken = sum(Wickets_Taken, na.rm = TRUE)) %>%
  arrange(desc(Wickets_Taken)) %>%
  slice_head(n = 10)

top_wicket_takers

# Bar representation of Top Wicket Takers
ggplot(top_wicket_takers, aes(x = reorder(Player_Name, Wickets_Taken), y = Wickets_Taken, fill = Player_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Wicket Takers in IPL", x = "Player", y = "Wickets Taken") +
  theme_minimal() 

# Measures of Central Tendency and Dispersion

# Create a simple Mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Summary for Runs Scored
mean(iplData$Runs_Scored, na.rm = TRUE)
median(iplData$Runs_Scored, na.rm = TRUE)
getmode(iplData$Runs_Scored)
var(iplData$Runs_Scored, na.rm = TRUE)
sd(iplData$Runs_Scored, na.rm = TRUE)
range(iplData$Runs_Scored, na.rm = TRUE)
IQR(iplData$Runs_Scored, na.rm = TRUE)



# ANOVA - Strike Rate differences among Top 10 Players

top10_players <- iplData %>%
  filter(Player_Name %in% Highest_scorers$Player_Name)

anova_result <- aov(Batting_Strike_Rate ~ Player_Name, data = top10_players)
summary(anova_result)

# Interpretation:
# If Pr(>F) < 0.05 --> significant difference among players' strike rates.


# Regression Analysis - Predict Runs based on Balls Faced

regression_model <- lm(Runs_Scored ~ Balls_Faced, data = iplData)
summary(regression_model)

# Plot Regression Line
ggplot(iplData, aes(x = Balls_Faced, y = Runs_Scored)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression: Runs Scored vs Balls Faced", x = "Balls Faced", y = "Runs Scored") +
  theme_minimal()


# Correlation Analysis

# Selecting numeric columns
ipl_numeric <- iplData %>%
  select(Matches_Batted, Runs_Scored, Balls_Faced, Batting_Strike_Rate, Wickets_Taken)

# Correlation Matrix
correlation_matrix <- cor(ipl_numeric, use = "complete.obs")

# Print Correlation Matrix
print(correlation_matrix)

# Visualize Correlation Matrix
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)



        
