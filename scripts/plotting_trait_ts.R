# Plotting Functional Traits by Abundance through Time
# EKB; WG second session (Sept 2021)

# PACKAGES and DATA 
library(tidyverse)

data <- readRDS("data/fielddata/combined_intertidal_abundance.RDS")

# SUMMARIZE

cover <- data %>% 
  select(SITE, YEAR, INTERTIDAL_TRANSECT, LEVEL, REPLICATE, DATA_TAKEN, ORGANISM,
         VALUE, MEASURE, VALID_NAME)
