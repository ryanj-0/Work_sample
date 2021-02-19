# This code is used to scale up when directories are shared and used under same name;
# Set working directory [setwd()] as needed

# clear environment
rm(list = ls())
# clears graphics devices to plot later
graphics.off()

if(getwd()==paste0("C:/Users/",Sys.info()[6],"/Documents/R")){
   dir <- paste0(getwd(), "/")
   message(paste0("Current working directory: ", getwd(), "/"))
} else{
   setwd(paste0("C:/Users/",Sys.info()[6],"/Documents/R", "/"))
   dir <- paste0(getwd(), "/")
   message(paste0("Directory changed, set to: ", getwd(), "/"))
}

# Loading Needed packages
# Checks for needed package [pacman] and then loads other packages [pkgs]
# future development: create automated check to return missing packages using [pkgs %in% loadedNamespaces()] (01/24/2021; rzj1019)

require("pacman", character.only = TRUE)

pkgs <- c("assertable",
          "data.table",
          "ggrepel",
          "shiny",
          "svMisc",
          "tidyverse")

p_load(pkgs, character.only = TRUE)

# Set global variables
repo <- "Work_sample/work_sample/"

##### Import Data, Data Cleaning, and Data Prep #####
#----------------------------------------------------
# Import data
source(paste0(dir, repo, "Data_work/import_data.R"))
# Data cleaning, returns table, Yearall, with cleaned data
source(paste0(dir, repo, "Data_work/data_work_cleaning.R"))