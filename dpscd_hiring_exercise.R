############################
# Data Analyst Hiring Exercise
#
# The following code is an exercise for Detroit Public School Community District (DPSCD)
# where we are tasks to create two different reports:
# 1. Community/District Wide
# 2. School Specific
############################

#### Initialize System Requirements ####
# This code is used to scale up when directories are shared and used under same name;
# Set working directory [setwd()] as needed

# clear environment
rm(list = ls())
# clears graphics devices to plot later
graphics.off()

if(getwd()==paste0("C:/Users/",Sys.info()[6],"/Documents/R")){
  dir <- getwd()
  message(paste0("Working directory: ", getwd()))
} else{
  setwd(paste0("C:/Users/",Sys.info()[6],"/Documents/R"))
  message(paste0("Directory changed, set to: ", getwd()))
}

# Set variables
repo <- "/Work_sample/work_sample"


#### Loading Needed packages ####
# Checks for needed package [pacman] and then loads other packages [pkgs]
# future development: create automated check to return missing packages using [pkgs %in% loadedNamespaces()] (01/24/2021; rzj1019)

require("pacman", character.only = TRUE)

pkgs <- c("data.table",
          "tidyverse",
          "ggthemes",
          "ggrepel",
          "svMisc")

p_load(pkgs, character.only = TRUE)



##### Import Data and Data Cleaning #####
#----------------------------------------

# Variable Key
key <- fread(file = paste0(dir, repo, "/Data/MI Statewide Student Growth (File Layout Key).csv"))

# 2015-2016 School Year
year15 <- fread(file = paste0(dir, repo, "/Data/MI Statewide Student Growth 2015-16.csv"))

# 2016 - 2017 School Year
year16 <- fread(file = paste0(dir, repo,  "/Data/MI Statewide Student Growth 2016-17.csv"))

# All Years Table: 2015-2017
yearall <- rbind(year15,year16,use.names = T)

# data cleaning
source(paste0(dir, repo,"/Data_work/data_work_cleaning.R"))



##### Section 1 #####
#--------------------
# Looking for Testing Group Disparities by ISD
# Year over Year (YoY) changes by MeanSGP
source(paste0(dir, repo, "/Data_work/data_work_section1.R"))


#--- Plotting Yoy MeanSGP change by ISD ---
source(paste0(dir, repo,"/Functions/isd_testinggroup_plot.R"))
# open pdf to populate with graphs
pdf(file = paste0(dir, repo, "/Plots/YoY Percent Changes in MeanSGP by ISD and Testing Group.pdf"), width = 11, height = 8.5)

# for loop to plot YoY change in MeanSGP by testinggroup and subject
for (isd in 1:(length(isd.msgp.delta[,unique(TestingGroup)])+1)) {

  # sets TestingGroup for iteration
  testgroup <- isd.msgp.delta[,unique(TestingGroup)][isd]

  # Dynamic tables for plotting
  isd.temp.math <- isd.msgp.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  isd.temp.ela <- isd.msgp.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  isd.temp.sci <- isd.msgp.delta[TestingGroup==testgroup & Subject=="Science"]

  # if statement to stop look and close graphics
  if(isd==length(isd.msgp.delta[,unique(TestingGroup)])+1){
    graphics.off()
    message("Done plotting, PDF ready for viewing.")
  }
  # Plots each subject for current testing group
  else{
    message("Plotting subjects for ", testgroup)
    # Math Plot
    isd_testinggroup_plot(isd.temp.math, testgroup, isd.temp.math[,unique(Subject)])
    # ELA Plot
    isd_testinggroup_plot(isd.temp.ela, testgroup, isd.temp.ela[,unique(Subject)])
    # Science Plot
    isd_testinggroup_plot(isd.temp.sci, testgroup, isd.temp.sci[,unique(Subject)])
  }
}



#--- Plotting YoY MeanSGP changes by Grade ---
# Data for Science is only for Grades 0,11
source(paste0(dir, repo,"/Functions/isd_grade_plot.R"))
# open pdf to populate with graphs
pdf(file = paste0(dir, repo, "/Plots/YoY Percent Changes in MeanSGP by ISD and Grade.pdf"), width = 11, height = 8.5)

for (grd in 1:(length(isd.msgp.grade.delta[,unique(Grade)])+1)) {

  # if statement to stop look and c
  grade <- isd.msgp.grade.delta[,unique(Grade)][grd]

  # Dynamic tables for plotting
  isd.temp.math <- isd.msgp.grade.delta[Grade==grade & Subject=="Mathematics"]
  isd.temp.ela <- isd.msgp.grade.delta[Grade==grade & Subject=="English Language Arts"]
  isd.temp.sci <- isd.msgp.grade.delta[Grade==grade & Subject=="Science"]

  # if statement to close pdf
  if(grd==length(isd.msgp.grade.delta[,unique(Grade)])+1){
    graphics.off()
    message("Done plotting, PDF ready for viewing.")
  }
  # Plots each subject for current grade
  else{
    message("Plotting subjects for ", grade, "th grade")
    # Math Plot
    isd_grade_plot(isd.temp.math, grade, isd.temp.math[,unique(Subject)])
    # ELA Plot
    isd_grade_plot(isd.temp.ela, grade, isd.temp.ela[,unique(Subject)])
    # Science Plot
    isd_grade_plot(isd.temp.sci, grade, isd.temp.sci[,unique(Subject)])
  }
}


##### Section 2 #####
#--------------------
# Line graph to show overall YoY changes
# By subjects, all students included; Social Science NA for 16/17 year
isd.subjects <- yearall[DistrictCode==0 & TestingGroup=="All Students" & Grade==0]
# soucrce plot functions
source(paste0(dir, repo, "/Functions/section2_plot.R"))



##### Section 3 #####
#--------------------
# Looking to show the top & bottom 5 ISD
# by each Subject (All Students, All Grades)
source(paste0(dir, repo, "/Data_work/data_work_section3.R"))
source(paste0(dir, repo, "/Functions/section3_plot.R"))



#-------- Plot Section 2 & 3 --------#
# open pdf to be able to populate with graphs
pdf(file = paste0(dir, repo, "/Plots/Overall & Comparison Plots.pdf"), width = 11, height = 8.5)

# Interested in the State of Michigan; set ISD Code = 0
dpscd_overall_plot(isd.subjects, 0)
# Interested in Hillsdale ISD; set ISD code = 30
dpscd_overall_plot(isd.subjects, 30)

# math comparison plot
compare_plots(isd.msgp.compare.math)
# ela comparison plot
compare_plots(isd.msgp.compare.ela)
# sci comparison plot
compare_plots(isd.msgp.compare.sci)

# close plots
graphics.off()
message("Done plotting, PDF ready for viewing.")




##### Section 4 ####
#-------------------
#look at changes in MeanSGP for Hillsdale ISD buildings
#by TestingGroup and Grade
source(paste0(dir, repo, "/Data_work/data_work_section4.R"))
source(paste0(dir, repo, "/Functions/hillsdale_testinggroup_plot.R"))

#--- Plotting Yoy MeanSGP for Hillsdale ISD by Testing Group ---
# All Subjects
pdf(file = paste0(dir, repo, "/Plots/Hillsdale - Buildings Percent Change in MeanSGP by Testing Group.pdf"), width = 11, height = 8.5)

for (bld in 1:(length(hillsdale.msgp.delta[,unique(TestingGroup)])+1)) {

  testgroup <- hillsdale.msgp.delta[,unique(TestingGroup)][bld]

  bld.temp.math <- hillsdale.msgp.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  bld.temp.ela <- hillsdale.msgp.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  bld.temp.sci <- hillsdale.msgp.delta[TestingGroup==testgroup & Subject=="Science"]

  # if statement to close pdf
  if(bld==length(hillsdale.msgp.delta[,unique(TestingGroup)])+1){
    graphics.off()
    message("Done plotting, PDF ready for viewing.")
  }
  else{
    message("Plotting subjects for ", testgroup)
    # Math Plot
    hillsdale_testinggroup_plot(bld.temp.math, testgroup, bld.temp.math[,unique(Subject)])
    # ELA Plot
    hillsdale_testinggroup_plot(bld.temp.ela, testgroup, bld.temp.ela[,unique(Subject)])
    # Science Plot
    hillsdale_testinggroup_plot(bld.temp.sci, testgroup, bld.temp.sci[,unique(Subject)])
  }
}


### Grade ###
source(paste0(dir, repo,"/Functions/hillsdale_grade_plot.R"))
# All Subjects
pdf(file = paste0(dir, repo, "/Plots/Hillsdale - Buildings, Percent Change in MeanSGP by Grade.pdf"), width = 11, height = 8.5)

for (grd in 1:(length(hillsdale.msgp.grade.delta[,unique(Grade)])+1)) {

  grade <- hillsdale.msgp.grade.delta[,unique(Grade)][grd]

  bld.temp.math <- hillsdale.msgp.grade.delta[Grade==grade & Subject=="Mathematics"]
  bld.temp.ela <- hillsdale.msgp.grade.delta[Grade==grade & Subject=="English Language Arts"]
  bld.temp.sci <- hillsdale.msgp.grade.delta[Grade==grade & Subject=="Science"]

  if(grd==length(hillsdale.msgp.grade.delta[,unique(Grade)])+1){
    graphics.off()
    message("Done plotting, PDF ready for viewing.")
  }
  else{
    message("Plotting subjects for ", grade, "th grade")
    # Math Plot
    hillsdale_grade_plot(bld.temp.math, grade, bld.temp.math[,unique(Subject)])
    # ELA Plot
    hillsdale_grade_plot(bld.temp.ela, grade, bld.temp.ela[,unique(Subject)])
    # Science Plot
    hillsdale_grade_plot(bld.temp.sci, grade, bld.temp.sci[,unique(Subject)])
  }
}