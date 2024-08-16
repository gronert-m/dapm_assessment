

          ###==================###
          ### ---- TASK 1 ---- ###
          ###==================###

#--------------------------------------------------------------------------#
# Step 1 - Set libraries, prep environment --------------------------------
#--------------------------------------------------------------------------#

# Clear up environment at start
rm(list=ls())

# List packages that we need
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "openxlsx")

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
tab <- function(x) table(x, useNA = "ifany")

#--------------------------------------------------------------------------#
# Step 2 - Read in, prep on/off track info file ---------------------------
#--------------------------------------------------------------------------#

# Read in file
status <- read_excel("01_rawdata/On-track and off-track countries.xlsx")

# ISO Alpha 3 codes for countries, 200 unique entries, no macro regions (e.g., SSA)
tab(status$ISO3Code)
tab(tab(status$ISO3Code))

# Goal is to have list of contries and status as on/off track. 
# Var Status.U5MR has different options, rule to turn binary is:
# on-track if Status.U5MR is “achieved” or “on-track”; 
# off-track if status is “acceleration needed”

# There are no NA cases of status
tab(status$Status.U5MR)

# As no NAs, anything not "accelaration needed" (nicely diplomaticly put) is on track
status$Status <- ifelse(status$Status.U5MR == "Acceleration Needed", "Off Track", "On Track")

# Reduce data for later merging to file with only necessary info
r_status <- status[, c("ISO3Code", "Status")]


#--------------------------------------------------------------------------#
# Step 3 - Read in, prep indicator file -----------------------------------
#--------------------------------------------------------------------------#

# File downloaded from
# https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=.MNCH_ANC4+MNCH_SAB.&startPeriod=2018&endPeriod=2022#
# on 2024/8/15

# Read in downloaded data
indic <- read.csv("01_rawdata/unicef_downloaded_data.csv")

# Gain an overview, see which vars are all NAs
summary(indic)

# Information is for women between 15 and 49 years of age
tab(indic$SEX.Sex)
tab(indic$AGE.Current.age)

# Indicator is [MNCH]_[Indicator shorthand]: [Description]
tab(indic$INDICATOR.Indicator)
# Generate a variable indicator that is shorthand only
indic$Case <- gsub("^MNCH_(.*?):.*$", "\\1", indic$INDICATOR.Indicator)

# Geographic area is [Area code]: [Name]
tab(indic$REF_AREA.Geographic.area)

# Keep only area code
indic$ISO3Code   <- gsub("^(.*?):\\s*.*$", "\\1", indic$REF_AREA.Geographic.area)

# Copy other vars we want to keep, shorten names
indic$Year  <- indic$TIME_PERIOD.Time.period
indic$Value <- indic$OBS_VALUE.Observation.Value

# Reduce to set of variables we need
r_indic <- indic[,c("ISO3Code", "Year", "Case", "Value")]

# ISO3Code includes regions (e.g., WH_AFRO), to match countries track info, 
# keep only actual 3 letter countries
true_3 <- grepl("^[A-Z]{3}$", r_indic$ISO3Code)
tab(r_indic$ISO3Code[!true_3])
r_indic <- r_indic[true_3, ]

# Need to further trim the data in that we:
# 1) Keep only the most recent info for each indicator
# 2) Reshape to have a single row per country as the other data has to merge

# Keep most recent
r_indic <- r_indic |> 
  group_by(ISO3Code, Case) |> 
  mutate(max_year = max(Year),
         recent   = max_year == Year) |> 
  ungroup() |> 
  filter(recent) |> 
  select(ISO3Code, Year, Case, Value)

# Reshape
r_indic <- r_indic |>
  pivot_wider(names_from = Case,
              values_from = Value)

# This output still can have more than one line per country if the latest measurement
# is form different years
r_indic[r_indic$ISO3Code == "GEO",]

# Take latest of any year (there can be only 2 at most, other is NA)
r_indic <- r_indic |>
  group_by(ISO3Code) |>
  summarise(
    ANC4 = last(ANC4[!is.na(ANC4)]),
    SAB  = last(SAB[!is.na(SAB)])) |>
  ungroup()


#--------------------------------------------------------------------------#
# Step 4 - Read in, prep population data file -----------------------------
#--------------------------------------------------------------------------#

# We are to read in the projected births in 2022 (regardless of when the latest data is from)
# This information is in the "Projections" sheet.

# The ISO codes, as the first rows are empty are read as logical, which then throws errors.
# Define the type (note that R Studio project writes the preview, don't need to write myself the tedious col_types)
pop <- 
  read_excel("01_rawdata/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx", 
             sheet = "Projections", range = "A17:BM22615", 
             col_types = 
               c("numeric", "text", "text", "text", "numeric", "text", "text", "numeric", "text", "numeric", 
                 "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                 "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                 "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                 "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                 "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

# Want reduced information only. Create ew vars that match naming convention in this file
pop$ISO3Code   <- pop$`ISO3 Alpha-code`
# Convert births, read as text into number
pop$Birth <- as.numeric(pop$`Births (thousands)`)

# We want to reduce this to (a) only countries (i.e., ISO 3 Code exists) and
# (b) year 2022
keeper <- !is.na(pop$ISO3Code) & pop$Year == 2022
r_pop <- pop[keeper, c("ISO3Code", "Birth")]


#--------------------------------------------------------------------------#
# Step 5 - Unite all three files ------------------------------------------
#--------------------------------------------------------------------------#

# Before uniting, let us look first at the country overlap

# From the row numbers, the largets is r_pop, then status, least is r_indic

# Are all of r_indic in r_status?
tab(unique(r_indic$ISO3Code) %in% unique(r_status$ISO3Code))
unique(r_indic$ISO3Code)[!(unique(r_indic$ISO3Code) %in% unique(r_status$ISO3Code))]
# BMU (Bermuda) honestly not in Status, XKX is Kosovo, as RKS in status, update
r_status$ISO3Code[r_status$ISO3Code == "RKS"] <- "XKX"
tab(unique(r_indic$ISO3Code) %in% unique(r_status$ISO3Code))

# Are all of r_indic in r_pop?
tab(unique(r_indic$ISO3Code) %in% unique(r_pop$ISO3Code))
# Works

# Are all of r_status in r_pop? 
tab(unique(r_status$ISO3Code) %in% unique(r_pop$ISO3Code))

# Based on the status, check what we may be missing
tab(r_status$Status)
tab(unique(r_status$ISO3Code[r_status$Status == "Off Track"]) %in% unique(r_indic$ISO3Code))
tab(unique(r_status$ISO3Code[r_status$Status == "On Track"]) %in% unique(r_indic$ISO3Code))
# From the above, we have data for 41 of the 59 Off Track countries (69.5%)
# and 101 of the 141 On track countries (71.6%)

unique(r_status$ISO3Code[r_status$Status == "Off Track"])[!(unique(r_status$ISO3Code[r_status$Status == "Off Track"]) %in% unique(r_indic$ISO3Code))]
# Largest missing countries TZA, ZAF, AGO
unique(r_status$ISO3Code[r_status$Status == "On Track"])[!(unique(r_status$ISO3Code[r_status$Status == "On Track"]) %in% unique(r_indic$ISO3Code))]
# Largest missing of the on track is MEX (GBR, KOR)

# Start by uniting indicator with status (expect 142 rows - r_indic minus BMU)
t1_df <- inner_join(r_indic, r_status, by = "ISO3Code")
stopifnot(nrow(t1_df) == 142)

# Then merge with birth information from 2022
t1_df <- left_join(t1_df, r_pop, by = "ISO3Code")
head(t1_df)

# Evaluate when we have info from one, the other, or both
both <- sum(!is.na(t1_df$ANC4) & !is.na(t1_df$SAB))
only_ANC4 <- sum(!is.na(t1_df$ANC4) & is.na(t1_df$SAB))
only_SAB <- sum(is.na(t1_df$ANC4) & !is.na(t1_df$SAB))

# Print results
cat("Rows with both ANC4 and SAB:", both, "\n")
cat("Rows with only ANC4:", onlSAB_ANC4, "\n")
cat("Rows with only SAB:", onlSAB_SAB, "\n")

unique(t1_df$ISO3Code[!is.na(t1_df$ANC4) & is.na(t1_df$SAB)])
unique(t1_df$ISO3Code[is.na(t1_df$ANC4) & !is.na(t1_df$SAB)])

#--------------------------------------------------------------------------#
# Step 6 - Calculate weighted coverage ------------------------------------
#--------------------------------------------------------------------------#

# Weighted coverage is calculated by group (off or on track) as
# (sum of value * weight in group) / (sum of weight in group)

track_values <- 
  
  t1_df |> 
  
  # Group by status
  group_by(Status) |> 
  
  # Calculate weighted mean
  summarise(ANC4 = weighted.mean(ANC4, Birth, na.rm = T),
            SAB  = weighted.mean(SAB,  Birth, na.rm = T))


#--------------------------------------------------------------------------#
# Step 7 - Plot -----------------------------------------------------------
#--------------------------------------------------------------------------#

# For plotting, we need to bring the data back to long format, as
# wish to use ggplot and that is what ggplot expects
data_country_grouped <- 
  track_values |> 
  pivot_longer(!Status, names_to = "Indic", values_to = "Value")

data_country_list <-
  t1_df |> 
  pivot_longer(!c(ISO3Code, Status, Birth), names_to = "Indic", values_to = "Value")

# Add text to add that in ANC4 India drags the average down
ann_text <- data.frame(Status = c("On Track", "On Track"), 
                       Indic = c("ANC4", "SAB"),
                       Value = c(58.5, 89.4))

# Create plot
t1_plot <-
  
  ggplot(data_country_list, aes(x = Status, y = Value, colour = Status)) +
  # Add individual country data points with jitter
  geom_jitter(aes(size = Birth), width = 0.2, alpha = 0.3) +
  
  # Add grouped mean points
  geom_point(data = data_country_grouped, aes(x = Status, y = Value, colour = Status, fill = Status), shape = 22, size = 6) +
  
  # Facet by Indic in a single row
  facet_wrap(~ Indic, nrow = 1) + 
  
  # Add indication for India (because of the jitter it may not always be placed perfectly, needs manual attention to look nice)
  geom_text(data = ann_text,label = "IND", hjust = 1.4) +
  
  # Add notes to graph
  labs(x = "U5 MR Status",
       y = "Coverage (in %)") +
  theme(legend.position = "none") 
t1_plot

#--------------------------------------------------------------------------#
# Step 8 - Save output ----------------------------------------------------
#--------------------------------------------------------------------------#

# Save plot
ggsave("03_output/02_graphs/task1_plot.png", plot = t1_plot)

# Save data

# Create a new Excel workbook
wb <- createWorkbook()
addWorksheet(wb, sheetName = "T1 - Wide")
addWorksheet(wb, sheetName = "T1 - Long")

# Write wide format data
writeDataTable(wb, sheet = "T1 - Wide",  t1_df,         startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
writeDataTable(wb, sheet = "T1 - Wide",  track_values,  startRow = 1, startCol = 8, tableStyle = "TableStyleLight9")

# Write wide format data
writeDataTable(wb, sheet = "T1 - Long",  data_country_list,    startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
writeDataTable(wb, sheet = "T1 - Long",  data_country_grouped, startRow = 1, startCol = 8, tableStyle = "TableStyleLight9")

# Save
saveWorkbook(wb, "03_output/01_data/T1_Data.xlsx", overwrite = TRUE)

