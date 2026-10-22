# ---------------------------------------------------------
# 23.3.4 Clinical trial: gait data
# ---------------------------------------------------------

# Load the packages I need
library(tidyverse)

# Have a look at the data
head(gait_data)
dim(gait_data)   # should be 16 rows and 704 columns

# Check what the treatment names actually are
table(gait_data$treatment)

# Check how many patients are in each treatment/exercise group
table(gait_data$treatment, gait_data$exercise)


# ---------------------------------------------------------
# Question 1: Plot each participant's gait + descriptive stats
# ---------------------------------------------------------

# The data is in "wide" format (one column per second).
# ggplot works better with "long" format (one row per measurement),
# so I will convert it with pivot_longer.
gait_long <- pivot_longer(
  gait_data,
  cols = ends_with("_secs"),   # all the columns that hold gait measurements
  names_to = "time",           # the old column names go into a new column called time
  values_to = "angle"          # the measurements go into a new column called angle
)

# The time column currently looks like "1_secs", so I remove the "_secs"
# part and turn it into a number
gait_long$time <- as.numeric(gsub("_secs", "", gait_long$time))

# Check that it worked
head(gait_long)

# Plot the gait of each participant over time (one panel per person)
ggplot(gait_long, aes(x = time, y = angle, colour = treatment)) +
  geom_line() +
  facet_wrap(~ donorid) +
  geom_hline(yintercept = 0, linetype = "dashed") +   # 0 = standing upright
  labs(x = "Time (seconds)", y = "Degrees off-axis") +
  theme_bw()

# Plot the distribution of angles for each participant
ggplot(gait_long, aes(x = angle, fill = treatment)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ donorid) +
  theme_bw()

# Now calculate descriptive statistics for each participant.
# I use group_by and summarise to get one row per participant.
gait_summary <- gait_long %>%
  group_by(donorid, treatment, age, exercise) %>%
  summarise(
    mean_angle = mean(angle),
    median_angle = median(angle),
    sd_angle = sd(angle),
    min_angle = min(angle),
    max_angle = max(angle),
    iqr_angle = IQR(angle)
  )

# Look at the table
gait_summary

# Which statistic is most useful?
# The mean is not very useful because positive and negative angles
# (leaning one way or the other) cancel each other out.
# The standard deviation (sd_angle) tells me how much the person wobbles
# around upright, so I will use it as my measure of gait smoothness/balance.
# A bigger SD means a less stable gait.


# ---------------------------------------------------------
# Question 2: Is there a difference between L-DOPA and the new compound?
# ---------------------------------------------------------

# I use one sd value per patient (16 values in total) because the
# measurements from one person over time are not independent of each other.

# Make a boxplot to compare the two treatments
ggplot(gait_summary, aes(x = treatment, y = sd_angle)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()

# --- Check the assumptions of a t-test ---

# 1. Are the data normally distributed in each group?
# Shapiro-Wilk test: a p-value > 0.05 means no evidence against normality
# I need to split the data into the two groups first
ldopa <- gait_summary[gait_summary$treatment == unique(gait_summary$treatment)[1], ]
newdrug <- gait_summary[gait_summary$treatment == unique(gait_summary$treatment)[2], ]

shapiro.test(ldopa$sd_angle)
shapiro.test(newdrug$sd_angle)

# QQ plots to check normality visually (points should follow the line)
qqnorm(ldopa$sd_angle)
qqline(ldopa$sd_angle)
qqnorm(newdrug$sd_angle)
qqline(newdrug$sd_angle)

# 2. Do the two groups have similar variances?
# A p-value > 0.05 means the variances are similar
var.test(sd_angle ~ treatment, data = gait_summary)

# --- Run the test ---

# If the assumptions are OK, use a t-test
t.test(sd_angle ~ treatment, data = gait_summary)

# If the data are NOT normal, use the Wilcoxon rank-sum test instead
wilcox.test(sd_angle ~ treatment, data = gait_summary)


# ---------------------------------------------------------
# Question 3: Do age or exercise affect gait?
# ---------------------------------------------------------

# Plot gait against age (colour = exercise group)
ggplot(gait_summary, aes(x = age, y = sd_angle, colour = exercise)) +
  geom_point(size = 3) +
  theme_bw()

# Plot gait for exercise vs no exercise
ggplot(gait_summary, aes(x = exercise, y = sd_angle)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()

# Is age similar between the two treatment groups?
# (If not, age could confuse the treatment comparison)
t.test(age ~ treatment, data = gait_summary)

# Linear model with treatment only (this is like the t-test)
model1 <- lm(sd_angle ~ treatment, data = gait_summary)
summary(model1)

# Linear model with treatment + age + exercise
# This tests the effect of treatment AFTER taking age and exercise into account
model2 <- lm(sd_angle ~ treatment + age + exercise, data = gait_summary)
summary(model2)

# Compare the two models. A small p-value means adding age and
# exercise made the model significantly better.
anova(model1, model2)

# Check the assumptions of the linear model with diagnostic plots
# (residuals should look random and roughly normal)
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# Shapiro test on the residuals
shapiro.test(residuals(model2))

# Put the donor IDs in order of treatment.
# order() sorts alphabetically by treatment first (L-DOPA, then New compound),
# and then by donor ID within each treatment.
ordered_ids <- gait_data$donorid[order(gait_data$treatment, gait_data$donorid)]

# Check the order looks right (the first 8 should be L-DOPA, the last 8 new compound)
ordered_ids

# Turn donorid into a factor with this order, so ggplot draws the panels in this order
gait_long$donorid <- factor(gait_long$donorid, levels = ordered_ids)

# Same plot as before, but now the panels are grouped by treatment
ggplot(gait_long, aes(x = time, y = angle, colour = treatment)) +
  geom_line() +
  facet_wrap(~ donorid, ncol = 4) +      # 4 panels per row
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time (seconds)", y = "Degrees off-axis") +
  theme_bw()