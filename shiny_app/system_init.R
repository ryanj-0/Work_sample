# This code is used to scale up when directories are shared and used under same name;
# Set working directory [setwd()] as needed

# clear environment
rm(list = ls())
# clears graphics devices to plot later
graphics.off()

if(getwd()==paste0("C:/Users/",Sys.info()[6],"/Documents/R")){
   dir <- getwd()
   message(paste0("Current working directory: ", getwd()))
} else{
   setwd(paste0("C:/Users/",Sys.info()[6],"/Documents/R"))
   dir <- getwd()
   message(paste0("Directory changed, set to: ", getwd()))
}

# Loading Needed packages
# Checks for needed package [pacman] and then loads other packages [pkgs]
# future development: create automated check to return missing packages using [pkgs %in% loadedNamespaces()] (01/24/2021; rzj1019)

require("pacman", character.only = TRUE)

pkgs <- c("data.table",
          "tidyverse",
          'shiny')

p_load(pkgs, character.only = TRUE,)