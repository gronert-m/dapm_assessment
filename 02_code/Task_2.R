
      ###==================###
      ### ---- TASK 2 ---- ###
      ###==================###

#--------------------------------------------------------------------------#
# Step 1 - Set libraries, prep environment --------------------------------
#--------------------------------------------------------------------------#

# Clear up environment at start
rm(list=ls())

# List packages that we need
pkgs <- c("lubridate", "dplyr", "tidyr", "ggplot2", "openxlsx", "data.table")

# Check they are installed
pkgs_2_install <- pkgs[!(pkgs %in% installed.packages())]

# Install if not on system
for (pkg in pkgs_2_install) {
  install.packages(pkg)
}

# Load packages
for (pkg in pkgs) {
  library(pkg,character.only=TRUE)
}

# R's table command does not include NAs by default, need to add
# "useNA = `ifany`" every time. NAs are important to know, especially
# in exploration. Make a function that does this
tab <- function(x, ...) {
  # Call table with the additional arguments if provided
  table(x, useNA = "ifany", ...)
}


#--------------------------------------------------------------------------#
# Step 2 - Read in survey data --------------------------------------------
#--------------------------------------------------------------------------#

# We can read in the data directly from the raw Github data
url <- "https://raw.githubusercontent.com/unicef-drp/UNICEF-P3-assessment-public/main/01_rawdata/Zimbabwe_children_under5_interview.csv"
data <- read.csv(url)


#--------------------------------------------------------------------------#
# Step 3 - Check data -----------------------------------------------------
#--------------------------------------------------------------------------#

# Simple review commands
summary(data)
lapply(data[,4:13], tab)

# Data has no NAs, but contrary to planned data dictionary, there are
# answer codes 9. Very few, assume they are 8s

# The plan is to replace the EC codes with 1 for yes (stays), 0 for no, NA for 8 or 9
# That way the mean of EC is also the pct of positive answers.

# Create a function to make this replacement
# Define the function to replace values in a vector
replace_values <- function(vec) {
  
  # Use case_when from dplyr for  replacement
  dplyr::case_when(
    vec == 1 ~ 1,
    vec == 2 ~ 0,
    vec %in% c(8, 9) ~ NA_real_)
  
}

# Apply function to EC columns
data[,4:13] <- lapply(data[,4:13], replace_values)

# Review again
summary(data)


#--------------------------------------------------------------------------#
# Step 4 - Calculate age of children in months ----------------------------
#--------------------------------------------------------------------------#

# If we are to compare the progress by months, need to know age in months, 
# info in years is not good enough, only 3 or 4
tab(data$child_age_years)

# Add age in months
data <- data |>
  mutate(
    
    # Convert character dates to Date objects
    child_birthday = as.Date(child_birthday, format = "%Y-%m-%d"),
    interview_date = as.Date(interview_date, format = "%Y-%m-%d"),
    
    # Calculate age in months 
    age_months = interval(child_birthday, interview_date) / months(1),
    # The above creates a number with decimal places, want only completed months
    age_floor = floor(age_months)
  )


#--------------------------------------------------------------------------#
# Step 5 - Create grouping averages ---------------------------------------
#--------------------------------------------------------------------------#

# We can cluster the ECs into four distinct groups:
# Literacy + Math: EC6, EC7, EC8
# Physical: EC9, EC10
# Learning: EC11, EC12
# Socio-emotional: EC13, EC14, EC15

# We add them up and divide by the number of questions in the group. That way
# each child has a score between 1 (said 1 to all) and 0 (answer 0 to all).
# For the purposes of this exercise, we treat NAs as 0s (i.e. they do not
# appear in the numerator but are counted in the denominator). It serves thus
# as a lower bound.

data$lit <- rowSums(data[,c("EC6", "EC7", "EC8")], na.rm = TRUE) / 3
data$phy <- rowSums(data[,c("EC9", "EC10")], na.rm = TRUE) / 2
data$lrn <- rowSums(data[,c("EC11", "EC12")], na.rm = TRUE) / 2
data$sne <- rowSums(data[,c("EC13", "EC14", "EC15")], na.rm = TRUE) / 3
data$all <- rowSums(data[,c("EC6", "EC7", "EC8", "EC9", "EC10", "EC11", "EC12", "EC13", "EC14", "EC15")], na.rm = TRUE) / 10


#--------------------------------------------------------------------------#
# Step 6 - Plot regression lines for the educational areas ----------------
#--------------------------------------------------------------------------#

# To plot this, we need to move the data to long format, as ggplot expects this format
df <- data |> 
  
  # Select columns we want
  select(age_months, all, lit, phy, lrn, sne) |> 
  
  # Convert the five EC columns to scores and their type
  pivot_longer(cols = c("all", "lit", "phy", "lrn", "sne"), names_to = "score_type", values_to = "score") |> 
  
  # Rename so they appear in the legend properly
  mutate(score_type = case_when(score_type == "all" ~ "All",
                                score_type == "lit" ~ "Lit",
                                score_type == "phy" ~ "Phy",
                                score_type == "lrn" ~ "Lrn",
                                score_type == "sne" ~ "SnE"))

# To not lose information, if data has 2517 kids, and we now have stacked 5 answers expect 5*2517 = 12585 rows
stopifnot(nrow(df) == nrow(data)*5)

t2_plot_1 <- 
  
  ggplot(df, aes(x = age_months, y = score, color = score_type)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +  # GLM regression lines with uncertainty shading
  labs(x = "Age (months)",
       y = "Score",
       color = "Dev Area",
       caption = "Source: Author's calculations based on Zimbabwe 2019 MICS6") +
  theme(
    legend.position = "bottom",      # Place legend at the bottom
    legend.box = "horizontal",       # Arrange legend items in a single row
    legend.box.margin = margin(0, 0, 0, 0),  # Remove extra margin around the legend box
    legend.title = element_text(size = 12),  # Optional: Adjust legend title size
    legend.text = element_text(size = 10) ,   # Optional: Adjust legend text size
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) 

#--------------------------------------------------------------------------#
# Step 7 - Plot questions of areas showing regression ---------------------
#--------------------------------------------------------------------------#

# Goal is to do the same things we have done above but with the underlying questions
# of the two areas where we see worsening results, split by developmental area

# Reshape physical questions so that we have them in long format
df_phy <- data |> 
  select(age_months, EC9, EC10) |> 
  pivot_longer(!age_months, names_to = "Q", values_to = "score") |> 
  mutate(type = "Physical")

# Reshape socio emotional questions so that we have them in long format
df_sne <- data |> 
  select(age_months, EC13, EC14, EC15) |> 
  pivot_longer(!age_months, names_to = "Q", values_to = "score") |> 
  mutate(type = "SocEmo")

df_falls <- bind_rows(df_phy, df_sne)
# We are stacking 5 questions, ensure we are not leaving out
stopifnot(nrow(df_falls) == nrow(data)*5)

# Order Questions so that they align with facets
desired_order <- c("EC9", "EC10", "EC13", "EC14", "EC15")
df_falls$Qf <- factor(df_falls$Q, levels = desired_order)

t2_plot_2 <- 
  
  ggplot(df_falls, aes(x = age_months, y = score, color = Qf)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +  # GLM regression lines with uncertainty shading
  labs(x = "Age (months)",
       y = "Score",
       color = "Question",
       caption = "Source: Author's calculations based on Zimbabwe 2019 MICS6") +
  theme(
    legend.position = "bottom",      # Place legend at the bottom
    legend.box = "horizontal",       # Arrange legend items in a single row
    legend.box.margin = margin(0, 0, 0, 0),  # Remove extra margin around the legend box
    legend.title = element_text(size = 12),  # Optional: Adjust legend title size
    legend.text = element_text(size = 10) ,   # Optional: Adjust legend text size
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) + 
  
  # Facet by Indic in a single row
  facet_wrap(~ type, nrow = 1) 

#--------------------------------------------------------------------------#
# Step 8 - Store plots ----------------------------------------------------
#--------------------------------------------------------------------------#

ggsave("03_output/02_graphs/task2_plot_1.png", plot = t2_plot_1,
       dpi=1200, width = 7, height = 5, units = "in")
ggsave("03_output/02_graphs/task2_plot_2.png", plot = t2_plot_2,
       dpi=1200, width = 7, height = 5, units = "in")
