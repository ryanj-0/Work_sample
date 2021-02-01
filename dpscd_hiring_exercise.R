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

# System Initialization
source("~/R/Data_work/import_data.R")

# Set global variables
repo <- "/Work_sample/work_sample"


##### Import Data and Data Cleaning #####
#----------------------------------------
# Import data
source(paste(dir, repo,"/Data_work/import_data.R"))
# Data cleaning, returns table, Yearall, with cleaned data
source(paste(dir, repo,"/Data_work/data_work_cleaning.R"))



##### Section 1 #####
#--------------------
# Looking for Testing Group Disparities by ISD
# Year over Year (YoY) changes by MeanSGP

# Data prepping for
source(paste0(dir, repo, "/Data_work/data_work_section1.R"))

# Variable for level of admin we are interested in
adminlevel <- "IsdName"
adminname <- "ISD Name"

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


# Looking for Grade Disparities by ISD
# Year over Year (YoY) changes by MeanSGP
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
# Looking for Testing Group Disparities by buildings in Hillsdale ISD
# Year over Year (YoY) changes by MeanSGP
source(paste0(dir, repo, "/Data_work/data_work_section4.R"))
source(paste0(dir, repo, "/Functions/hillsdale_testinggroup_plot.R"))

# Variable for level of admin we are interested in
adminlevel <- "BuildingName"
adminname <- "Building Name"

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


# Looking for Grade Disparities by buildings in Hillsdale ISD
# Year over Year (YoY) changes by MeanSGP
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