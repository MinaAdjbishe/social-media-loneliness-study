# =====================================================================
# Title: Social Media and Loneliness Study
# Description: Cleaning, scoring, and mediation analysis
# Author: Mina Adjbishe
# Date: 28/10/25
# =====================================================================

# ---------------------------------------------------------------------
# Install packages
# ---------------------------------------------------------------------
install.packages(c(
  "tidyverse",       # for data wrangling and visualisation
  "psyntur",           # for reliability analysis and descriptives
  "lavaan",          # for SEM, CFA, mediation models
  "janitor"          # for cleaning variable names and data
))


# ---------------------------------------------------------------------   
# Load packages packages for use in the script
# ---------------------------------------------------------------------         
library(tidyverse)            
library(psyntur)
library(janitor)
library(lavaan)

# ---------------------------------------------------------------------   
# Load datasets
# ---------------------------------------------------------------------   
scales_data <- read_csv("scales_data.csv")  #Contains survey responses for scales
demo_data <- read_csv("demo_data.csv")      #Contains participant demographics

# Inspect the structure of the datasets to understand variables
glimpse(scales_data)
glimpse(demo_data)

# ---------------------------------------------------------------------   
# Clean column names for easier handling
# ---------------------------------------------------------------------   
df_scales <- scales_data %>%
  clean_names()   # Makes all column names lowercase and removes spaces/special characters

# Select only relevant columns for analysis (participant ID + quantised scale responses)
df_selected <- df_scales %>%
  select(participant_private_id,
         matches("quantised"))

glimpse(df_selected)

# ---------------------------------------------------------------------
# Rename scale columns for clarity
# ---------------------------------------------------------------------
# Scale 1: Social Media Use Integration Scale
scale_1_columns <- grepl("scale_1", 
                         names(df_selected))
scale_1_names <- paste0 ("smuis_", 1:10)
names(df_selected)[scale_1_columns] <- scale_1_names

# Scale 2: Ruminative Response Scale
scale_2_columns <- grepl("scale_2",
                         names(df_selected))
scale_2_names <- paste0("rrs_", 1:10)
names(df_selected)[scale_2_columns] <- scale_2_names

# Scale 3: UCLA Loneliness Scale
scale_3_columns <- grepl("scale_3",
                         names(df_selected))
scale_3_names <- paste0("ucla_", 1:5)
names(df_selected)[scale_3_columns] <- scale_3_names

# ---------------------------------------------------------------------
# Reverse negatively keyed items
# ---------------------------------------------------------------------
# smuis_8 is negatively worded, so reverse code

df_selected <- df_selected %>%
  mutate(smuis_8 = re_code(smuis_8, 1:5, 5:1))  # Reverses scale from 1-5

# ---------------------------------------------------------------------
# Check correlations between items to ensure consistency
# ---------------------------------------------------------------------
smuis_items <- df_selected %>% select(starts_with("smuis_"))
round(cor(smuis_items, use = "pairwise.complete.obs"), 2)  # Correlation matrix
summary(df_selected$smuis_8)
table(df_selected$smuis_8)

# ---------------------------------------------------------------------
# Calculate reliability (Cronbach's alpha) for all scales
# ---------------------------------------------------------------------
cronbach(df_selected,
         social_media = starts_with("smuis_"),
         rumination = starts_with("rrs_"),
         loneliness = starts_with("ucla_"))
cronbach(df_selected,
         smuis = starts_with("smuis_"))

# ---------------------------------------------------------------------
# Calculate total scores for each participant for each scale
# ---------------------------------------------------------------------
df_total <- total_scores(df_selected,
                         social_media = starts_with("smuis_"),
                         rumination = starts_with("rrs_"),
                         loneliness = starts_with("ucla_"))

glimpse(df_total)

# ---------------------------------------------------------------------
# Descriptive statistics for total scores
# ---------------------------------------------------------------------
describe_across(data = df_total,
                variables = c(social_media, rumination, loneliness),
                functions = list (avg = ~mean(.x, na.rm = TRUE),
                                  median = ~median(.x, na.rm = TRUE),
                                  stdev = ~sd(.x, na.rm = TRUE),
                                  iqr = ~IQR(.x, na.rm = TRUE),
                                  min = ~min(.x, na.rm = TRUE),
                                  max = ~max(.x, na.rm = TRUE)),
                pivot = TRUE)

# ---------------------------------------------------------------------
# Clean and prepare demographics data
# ---------------------------------------------------------------------
df_demo <- demo_data %>%
  clean_names()  # Standardise column names

# Select variables of interest: age and gender
demo_selected <- df_demo %>%
  select(participant_private_id, age_object_2_value, gender_object_3_quantised)
colnames(df_demo)

# Rename for clarity
demo_selected <- demo_selected %>%
  rename(
    age = age_object_2_value,
    gender = gender_object_3_quantised
  )

# Descriptive statistics for age
describe_across(
  data = demo_selected,
  variables = age,
  functions = list(
    avg = ~mean(.x, na.rm = TRUE),
    stdev = ~sd(.x, na.rm = TRUE)
  ),
  pivot = TRUE
)

# Count participants by gender
demo_selected %>%
  count(gender)

# Label gender categories
demo_selected <- demo_selected %>%
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Non-binary",
    gender == 4 ~ "Prefer not to say",
    TRUE ~ NA_character_
  ))

# ---------------------------------------------------------------------
# Define mediation model
# ---------------------------------------------------------------------
# Model: Social Media Use -> Rumination -> Loneliness
Model <- '
  # Regressions
  loneliness ~ b*rumination + cp*social_media
  rumination ~ a*social_media

  # Effects
  indirect := a*b
  direct := cp
  total := direct + indirect'

# ---------------------------------------------------------------------
# Fit the model with bootstrapping
# ---------------------------------------------------------------------
fit <- sem(
  Model,
  data = df_total,
  se = "bootstrap",
  bootstrap = 1000  # as in your example
)

# ---------------------------------------------------------------------
# 5. Get parameter estimates with bootstrapped CIs
# ---------------------------------------------------------------------
est <- parameterEstimates(
  fit,
  boot.ci.type = "bca.simple",
  standardized = TRUE)
glimpse(est)

# ---------------------------------------------------------------------
# Summarise means and SD for each total score
# ---------------------------------------------------------------------
descriptives <- df_total %>%
  summarise(
    social_media_mean = mean(social_media, na.rm = TRUE),
    social_media_sd = sd(social_media, na.rm = TRUE),
    rumination_mean = mean(rumination, na.rm = TRUE),
    rumination_sd = sd(rumination, na.rm = TRUE),
    loneliness_mean = mean(loneliness, na.rm = TRUE),
    loneliness_sd = sd(loneliness, na.rm = TRUE)
  )

descriptives

# ---------------------------------------------------------------------
# Mediation diagram summary 
# ---------------------------------------------------------------------
cat("
Social Media Use --> Rumination --> Loneliness
   a = 0.63          b = 0.77
Social Media Use --> Loneliness
   cp = 0.04 (ns)
Indirect effect = 0.49 (p = 0.002)
Total effect = 0.53 (p < 0.001)
")

# ---------------------------------------------------------------------
# Prepare long-format data for plotting distributions
# ---------------------------------------------------------------------
df_long <- df_total %>%
  pivot_longer(
    cols = c(social_media, rumination, loneliness),
    names_to = "Scale",
    values_to = "Score"
  )

# Boxplot of scale distributions
ggplot(df_long, aes(x = Scale, y = Score, fill = Scale)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Distribution of Scale Scores",
       y = "Total Score",
       x = "") +
  theme(legend.position = "none")

# ---------------------------------------------------------------------
# Bar plot with mean and SD for each scale
# ---------------------------------------------------------------------
scale_summary <- df_total %>%
  summarise(
    social_media_mean = mean(social_media, na.rm = TRUE),
    social_media_sd   = sd(social_media, na.rm = TRUE),
    rumination_mean   = mean(rumination, na.rm = TRUE),
    rumination_sd     = sd(rumination, na.rm = TRUE),
    loneliness_mean   = mean(loneliness, na.rm = TRUE),
    loneliness_sd     = sd(loneliness, na.rm = TRUE)
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Scale", ".value"),
    names_pattern = "(.*)_(mean|sd)"
  )

ggplot(scale_summary, aes(x = Scale, y = mean, fill = Scale)) +
  geom_col(alpha = 0.7) +                # Bars
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) + # SD error bars
  theme_minimal() +
  labs(title = "Mean Scores for Each Scale",
       x = "",
       y = "Total Score") +
  theme(legend.position = "none")

# Create a new dataframe for age analysis
df_age_analysis <- df_total %>%
  mutate(participant_private_id = demo_selected$participant_private_id,
         age = demo_selected$age)

# Check it
glimpse(df_age_analysis)

# ---------------------------------------------------------------------
# Create dataframe for age analysis
# ---------------------------------------------------------------------
df_age_analysis <- df_total %>%
  mutate(
    participant_private_id = demo_selected$participant_private_id,
    age = demo_selected$age
  )

glimpse(df_age_analysis)

# ---------------------------------------------------------------------
# Reshape for plotting against age
# ---------------------------------------------------------------------
df_long <- df_age_analysis %>%
  pivot_longer(
    cols = c(social_media, rumination, loneliness),
    names_to = "Scale",
    values_to = "Score"
  )


#  Scatter plots with regression lines
ggplot(df_long, aes(x = age, y = Score, color = Scale)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Relationship Between Age and Scale Scores",
       x = "Age",
       y = "Total Score")

# ---------------------------------------------------------------------
# Pearson correlations between age and scales
# ---------------------------------------------------------------------
cor_results <- df_age_analysis %>%
  summarise(
    cor_social_media = cor(social_media, age, use = "complete.obs"),
    cor_rumination   = cor(rumination, age, use = "complete.obs"),
    cor_loneliness   = cor(loneliness, age, use = "complete.obs")
  )
print(cor_results)

# ---------------------------------------------------------------------
# Linear regressions predicting scales from age
# ---------------------------------------------------------------------
lm_social <- lm(social_media ~ age, data = df_age_analysis)
lm_rumination <- lm(rumination ~ age, data = df_age_analysis)
lm_loneliness <- lm(loneliness ~ age, data = df_age_analysis)

summary(lm_social)
summary(lm_rumination)
summary(lm_loneliness)



