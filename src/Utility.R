# Private and Confidential. Property of J&J
# Author: Gary Chung
# Project: General Non-Specific
# Description: Utility Functions
# ==============================================================================
options(stringsAsFactors=F)

# ==============================================================================
# Required Packages
# ==============================================================================

UsePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
UsePackage("lubridate")
UsePackage("xlsx")
UsePackage("stringr")


# ==============================================================================
# Required Sources
# ==============================================================================

# N/A


# ==============================================================================
# Defined Functions
# ==============================================================================

# Write to Excel file, resize, filter, freeze
WriteToXL <- function(inset, shName, filePath, existing=T, colResize=T){
  xlsx::write.xlsx(inset, file=filePath, sheetName=shName,
                   row.names=F, showNA=F, append=existing)
  # Size columns, freeze panes, add filters
  wb <- xlsx::loadWorkbook(filePath)
  addAutoFilter(getSheets(wb)[[shName]], paste0("A1:", LETTERS[ncol(inset)], "1"))
  if (colResize) autoSizeColumn(getSheets(wb)[[shName]], colIndex=1:ncol(inset))
  createFreezePane(getSheets(wb)[[shName]], 2, 1, 2, 1)
  saveWorkbook(wb, filePath)
}

# Start/Stop timer
StartTimer <- function(){ assign("ptm", proc.time(), envir = .GlobalEnv) }
StopTimer <- function(what) {
  time <- round(proc.time() - ptm)[3]
  cat(paste0("\nFinished ", what, " in ",
             ifelse(time < 60, paste0(time, " seconds"),
                    paste0(time %/% 60, ":", sprintf("%02d", time %% 60))), "\n"))
  rm(ptm, pos=".GlobalEnv")
}

# Message Update
Message <- function(x) cat("\n", x, format(Sys.time(), "%H:%M:%S"))

# Convert date to month-year (first of the month)
ToYearMon <- function(x) {
  as.Date(paste0(format(x, "%Y"), "-", format(x, "%m"), "-01"))
}

# Remove all punctuation, upcase, and trim whitespace from a string
RemovePunct <- function(input){
  str_trim(toupper(gsub("\\s+", " ", gsub("[[:punct:]]", " ", input))))
}

# Add to info file
AddInfo <- function(grp, inf, label, inlist=info){
  inlist[[grp]][[label]] <- inf
  return(inlist)
}

# Transform info into a 3-column format for stat output to Excel
StatsToXL <- function(grp, find){
  toget=which(grepl(find, names(info[[grp]])))
  outdf <- data.frame()
  for(i in toget){
    this <- info[[grp]][[i]]
    if (class(this) %in% c("character", "Date", "numeric", "integer")){
      if (length(this) == 1){
        thisdf <- data.frame(Name=names(info[[grp]])[i],
                             Group="N/A",
                             Value=as.character(this))
      } else{
        thisdf <- data.frame(Name=names(info[[grp]])[i],
                             Group=seq(1:length(this)),
                             Value=as.character(this))
      }
    } else if (class(this) == "data.frame" & ncol(this) == 2){
      thisdf <- data.frame(Name=names(info[[grp]])[i],
                           Group=this[[1]],
                           Value=as.character(this[[2]]))
    } else cat(paste("\nInvalid Info:", names(info[[grp]])[i]))
    outdf <- rbind(outdf, thisdf)
  }
  outdf$Group <- with(outdf, ifelse(is.na(Group), "N/A", Group))
  return(outdf)
}

# Conversion from string to factor - Any Input
# If no specified lvls, reference level is the max count
# If lvls is length one, that is the reference
# Otherwise lvls must be the vector of unique values in x
StrToFactor <- function(x,
                     lvls=NA,
                     na_levels="missing" # Vector of values to collapse into NA
){
  x <- as.character(x)
  xt <- ifelse(x %in% na_levels, NA, x)
  if (is.na(lvls[1])){
    relevel(factor(xt), ref=names(which.max(table(xt))))
  } else{
    if (length(lvls) == 1){
      relevel(factor(xt), ref=lvls)
    } else{
      factor(xt, levels=lvls)
    }
  }
}

# Conversion from string to factor - Binary Yes/No Type Input
BinaryYNtoFactor <- function(x){
  x <- as.character(x)
  xt <- ifelse(toupper(x) %in% c("YES", "Y", "1"), "Yes",
               ifelse(toupper(x) %in% c("NO", "N", "0"), "No", NA))
  relevel(factor(xt), ref="No")
}


# ==============================================================================
# Script
# ==============================================================================

# N/A
