# Private and Confidential.
# Author: <AUTHOR>
# <DEVELOPMENT/PRODUCTION> SCRIPT
# Project: <PROJECT>
# Description: <DESCRIPTION>
# ==============================================================================

# ==============================================================================
# Required Packages
# ==============================================================================

UsePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
# UsePackage("yourPackage")


# ==============================================================================
# Required Sources
# ==============================================================================

# STANDARD Sources
source("src/Utility.R"); Message("Template.R")
# INSTRUCTIONS: Initialize the info object in the first logical script, such as ImportData.R
# INSTRUCTIONS: Subsequent dependencies will just load info.Rdata
info <- list() # Initialize diagnostic information list
load(file="data/interim/info.Rdata")


# ==============================================================================
# Defined Functions
# ==============================================================================

# N/A


# ==============================================================================
# Script
# ==============================================================================
StartTimer()

# ------------------------------------------------------------------------------
# Header
# ------------------------------------------------------------------------------

# INSTRUCTIONS: If desired, using object naming conventions such as
# INSTRUCTIONS: s.* for source
# INSTRUCTIONS: w.* for working
# INSTRUCTIONS: t.* for temporary
# INSTRUCTIONS: Then objects can be saved or removed by pattern as follows
save(list=ls(pattern="^s\\."), file="myFile.Rdata")
rm(list=ls(pattern="^s\\."))


# ==============================================================================
# END OF FILE
# ==============================================================================

# STANDARD Cleanup
# Save diagnostics and clear environment
save(info, file="data/interim/info.Rdata")
StopTimer("script")
rm(list=ls()); gc()