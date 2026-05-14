############################################################
# MATH 167R — Statistical Programming with R
# Final Project — Data Cleaning & Exploratory Data Analysis
#
# Student Name: Lila Nguyen
# Group C
# Date: May 6, 2026
# Instructor: Andrea Gottlieb
############################################################


############################
# 0) SETUP
############################

# Start clean
rm(list = ls())

# Make printing easier to read
options(stringsAsFactors = FALSE)

# Load packages used in analysis
library(tidyverse)
library(skimr)
library(GGally)
library(scales)


############################
# PLOT EXPORT SETUP
############################

# Define plots directory (relative to scripts folder)
plot_dir <- "bank-marketing/plots"

# Helper function to save plots consistently
save_plot <- function(plot, name, width = 6, height = 4) {
  ggsave(
    filename = file.path(plot_dir, paste0(name, ".png")),
    plot = plot,
    width = width,
    height = height
  )
}


############################
# 1) IMPORT DATA
############################

# Import Bank Marketing dataset
bank <- read.csv(
  "data/bank-additional-full.csv", 
  sep = ";"
  )

# Inspect dimensions
dim(bank)

# Dataset contains: 
# 41,188 observations and 21 variables

# View first rows
head(bank)

# Check variable structure
str(bank)

# Initially, we see:
# Many categorical vars are stored as character.
# The response variable y indicates subscription outcome.


############################
# 2) DATA CLEANING
############################

# Convert categorical variables to factors
bank$job <- as.factor(bank$job)

bank$marital <- as.factor(bank$marital)

bank$education <- as.factor(bank$education)

bank$default <- as.factor(bank$default)

bank$housing <- as.factor(bank$housing)

bank$loan <- as.factor(bank$loan)

bank$contact <- as.factor(bank$contact)

bank$month <- as.factor(bank$month)

bank$day_of_week <- as.factor(bank$day_of_week)

bank$poutcome <- as.factor(bank$poutcome)

bank$y <- as.factor(bank$y)

# Confirm no missing values
colSums(is.na(bank))

# No explicit missing values were found, but
# some variables contain "unknown" categories.

# Create indicator for prev contact
bank$previous_contact <- ifelse(bank$pdays == 999, 
                                "No", 
                                "Yes")

bank$previous_contact <- as.factor(bank$previous_contact)

# Most observations have pdays = 999, indicating that most of 
# the clients were not previously contacted.

# Remove duration for prediction modeling (duration known 
# after phone call ends, so potential data leakage)
bank_model <- subset(bank,
                     select = -duration)

# Verify cleaned structure
str(bank_model)


############################
# 3) DESCRIPTIVE STATISTICS
############################

# Compute summary statistics for quantitative variables
summary(
  bank[, c(
    "age",
    "campaign",
    "pdays",
    "previous",
    "emp.var.rate",
    "cons.price.idx",
    "cons.conf.idx",
    "euribor3m",
    "nr.employed"
  )]
)

# Key observations: 
# Average client age is ~40.
# "campaign" is right-skewed with extreme outliers.
# "pdays" is heavily concetrated at 999.
# "euribor3m" varies across the observations.

# More detailed summary statistics
skim(
  bank[, c(
    "age",
    "campaign",
    "pdays",
    "previous",
    "emp.var.rate",
    "cons.price.idx",
    "cons.conf.idx",
    "euribor3m",
    "nr.employed"
  )]
)

# The histograms verify:
# "age" is moderately right-skewed.
# "campaign" is strongly right skew with outliers.
# "previous" is concentrated around 0.
# "euribor3m" has clustered distributions, as indicated by
# the large jump from Q1 (1.344) to the Median (4.857) and how
# Q3 (4.961) and Max (5.045) are near 5.

# Frequency tables for categorical variables
table(bank$job)

table(bank$education)

table(bank$marital)

table(bank$y)

# Proportion of subscription outcomes
prop.table(table(bank$y))

# Key observations:
# Subscription outcomes are highly imbalanced, as
# most clients did not subscript to the term deposit.

# Subscription rates by job
prop.table(
  table(bank$job, bank$y),
  1
)

# Key observations:
# Clients who are students and retired show high
# subscription rates, while blue-collar occupations
# show the lowest rates

# Subscription rates by education
prop.table(
  table(bank$education, bank$y),
  1
)

# Key observations:
# Clients with university degrees generally show
# higher subscription rates than several lower
# education categories.


############################
# 4) EXPLORATORY DATA ANALYSIS
############################

############################
# (a) HISTOGRAMS
############################

# Distribution of client age
p_age <- ggplot(bank, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Client Age", x = "Age", y = "Count") +
  theme_minimal()

save_plot(p_age, "hist_age")

# Distribution of Euribor 3-Month Rate
p_euribor <- ggplot(bank, aes(x = euribor3m)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Euribor 3-Month Rate",
       x = "Euribor 3-Month Rate", y = "Count") +
  theme_minimal()

save_plot(p_euribor, "hist_euribor")

# Distribution of campaign contacts
p_campaign <- ggplot(bank, aes(x = campaign)) +
  geom_histogram(bins = 30, fill = "purple", color = "white") +
  labs(title = "Distribution of Number of Campaign Contacts",
       x = "Campaign Contacts", y = "Count") +
  theme_minimal()

save_plot(p_campaign, "hist_campaign")

# These distributions confirm earlier findings. 


############################
# (b) BOXPLOTS
############################

# Age by subscription outcome
p_age_box <- ggplot(bank, aes(x = y, y = age, fill = y)) +
  geom_boxplot() +
  labs(title = "Age by Subscription Outcome",
       x = "Subscribed", y = "Age") +
  theme_minimal()

save_plot(p_age_box, "box_age")

# Euribor rate by subscription outcome
p_euribor_box <- ggplot(bank, aes(x = y, y = euribor3m, fill = y)) +
  geom_boxplot() +
  labs(title = "Euribor Rate by Subscription Outcome",
       x = "Subscribed", y = "Euribor 3-Month Rate") +
  theme_minimal()

save_plot(p_euribor_box, "box_euribor")

# Campaign contacts by subscription outcome
p_campaign_box <- ggplot(bank, aes(x = y, y = campaign, fill = y)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 15)) +
  labs(title = "Campaign Contacts by Subscription Outcome",
       x = "Subscribed", y = "Number of Contacts") +
  theme_minimal()

save_plot(p_campaign_box, "box_campaign")


############################
# (c) BAR CHARTS
############################

# Distribution of subscription outcomes
p_y <- ggplot(bank, aes(x = y, fill = y)) +
  geom_bar() +
  labs(title = "Distribution of Subscription Outcomes",
       x = "Subscribed", y = "Count") +
  theme_minimal()

save_plot(p_y, "bar_y")

# Subscription rates by job type
p_job <- ggplot(bank, aes(x = job, fill = y)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Subscription Rates by Job Type",
       x = "Job Type", y = "Proportion") +
  theme_minimal()

save_plot(p_job, "bar_job")

# Subscription rates by education level
p_education <- ggplot(bank, aes(x = education, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rates by Education Level",
       x = "Education", y = "Proportion") +
  theme_minimal()

save_plot(p_education, "bar_education")

# Subscription rates vary across education groups.
# The illiterate category shows the highest observed
# subscription proportion, although this group has
# a very small sample size.
#
# Among larger education groups, clients with
# university degrees appear to have relatively
# higher subscription rates.

# Subscription rates by month
p_month <- ggplot(bank, aes(x = month, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rates by Month",
       x = "Month", y = "Proportion") +
  theme_minimal()

save_plot(p_month, "bar_month")

# Subscription rates appear highest in December, March,
# October, and September.

# Subscription rates by previous campaign outcome
p_poutcome <- ggplot(bank, aes(x = poutcome, fill = y)) +
  geom_bar(position = "fill") +
  labs(title = "Subscription Rates by Previous Campaign Outcome",
       x = "Previous Campaign Outcome", y = "Proportion") +
  theme_minimal()

save_plot(p_poutcome, "bar_poutcome")

# Clients with successful previous campaign
# outcomes appear substantially more likely
# to subscribe in the current campaign.
#
# In contrast, clients with failed or
# nonexistent previous outcomes show
# much lower subscription rates.


############################
# (d) SCATTERPLOTS
############################

# Age versus campaign contacts
p_age_campaign <- ggplot(bank, aes(x = age, y = campaign)) +
  geom_point(alpha = 0.4) +
  labs(title = "Age versus Campaign Contacts",
       x = "Age", y = "Campaign Contacts") +
  theme_minimal()

save_plot(p_age_campaign, "scatter_age_campaign")

# Euribor rate versus number employed
p_euribor_emp <- ggplot(bank, aes(x = euribor3m, y = nr.employed)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Euribor Rate versus Number Employed",
       x = "Euribor 3-Month Rate", y = "Number Employed") +
  theme_minimal()

save_plot(p_euribor_emp, "scatter_euribor_employed")


############################
# (e) PAIR PLOTS
############################

# Pairwise relationships among quantitative variables
png(filename = file.path(plot_dir, "pairplot.png"),
    width = 900,
    height = 700)

ggpairs(
  bank[, c(
    "age",
    "campaign",
    "pdays",
    "previous",
    "euribor3m",
    "nr.employed"
  )]
)

dev.off()

# "euribor3m" (interest rate) and "nr.employed" (number of 
# employees) appear to have the strongest positive correlation
# (r = 0.945). This means that as the Euribor 3-month 
# interest rate increases, employment levels also tend to 
# increase, suggesting that these macoeconomic indicators
# move together.

# "pdays" and "previous" show a moderately strong negative
# relationship (r = -0.588). This means that clients who
# were contacted more times previously generally have smaller
# "pday" values.

# Looking at "previous" against both macroeconomic variables,
# "euribor3m" and "nr.employed", it is observed that when
# interest rates and employement were higher, the number of
# previous contacts tended to be lower. 