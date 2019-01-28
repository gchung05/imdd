# Proprietary. All Rights Reserved
# Author: Gary Chung
# DEVELOPMENT SCRIPT
# Project: ICIJ IMDD
# Description: Load ICIJ IMDD Database
# ==============================================================================

# ==============================================================================
# Required Packages
# ==============================================================================

UsePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
UsePackage("dplyr")


# ==============================================================================
# Required Sources
# ==============================================================================

# STANDARD Sources
source("src/Utility.R"); Message("load_icij.R")
folder <- "data/external/ICIJ IMDD 20181219/"
files <- list.files(folder)
imdd <- list(device=read.csv(paste0(folder, files[grepl("device", files)])),
             manuf=read.csv(paste0(folder, files[grepl("manuf", files)])),
             event=read.csv(paste0(folder, files[grepl("event", files)])))

# ==============================================================================
# Defined Functions
# ==============================================================================

# N/A


# ==============================================================================
# Script
# ==============================================================================
StartTimer()

# ------------------------------------------------------------------------------
# Basic Processing
# ------------------------------------------------------------------------------

save(imdd, file="data/interim/imdd.Rdata")

rm(list=ls(pattern="^s\\."))


# ==============================================================================
# END OF FILE
# ==============================================================================

# STANDARD Cleanup
# Save diagnostics and clear environment
StopTimer("script")
rm(list=ls()); gc()