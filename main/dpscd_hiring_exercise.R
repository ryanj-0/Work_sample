########################################################################################
# Data Analyst Hiring Exercise
#
# The following code is an exercise for Detroit Public School Community District (DPSCD)
# where we are tasks to create two different reports:
# 1. Community/District Wide
# 2. School Specific
########################################################################################

#### Initialize System Requirements ####
#---------------------------------------

# System Initialization
source("~/R/Work_sample/work_sample/main/system_init.R")

#---- Set global variables ---#
# main dir
repo <- "Work_sample/work_sample/"
# data
data.dir <- "data/"
archive.dir <- "archive/"
raw.dir <- "raw/"
rds.dir <- "working/"
# functions
function.dir <- "functions/"

# Global Functions
source(paste0(dir, repo, cleaning.dir, "data_qc.R"))
source(paste0(dir, repo, function.dir, "testinggroup_plot.R"))
source(paste0(dir, repo, function.dir, "grade_plot.R"))
source(paste0(dir, repo, function.dir, "dpscd_overall_plot.R"))
source(paste0(dir, repo, function.dir, "comparison_plot.R"))


##### Import Data, Data Cleaning, and Data Prep #####
#----------------------------------------------------
# Import data
source(paste0(dir, repo, "Data_work/import_data.R"))
# Data cleaning, returns table, Yearall, with cleaned data
source(paste0(dir, repo, "Data_work/data_work_cleaning.R"))

# Data Prep
source(paste0(dir, repo, "Data_work/change_meansgp_by_isd.R"))
source(paste0(dir, repo, "Data_work/change_meansgp_hillsdale.R"))
source(paste0(dir, repo, "Data_work/overall_and_comparison.R"))


##### Year over Year (YoY) changes in MeanSGP by ISD #####
#---------------------------------------------------------
# Looking for Testing Group and Grade disparities by ISD, respectively

# X-axis label and column we are interested in
xlabel <- "ISD Name"
col.interest = "IsdName"

#--- Testing Group ---

# open pdf to populate with graphs
pdf(file = paste0(dir, repo, "Plots/YoY Percent Changes in MeanSGP by ISD and Testing Group.pdf"), width = 11, height = 8.5)

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
    testinggroup_plot(plot_data = isd.temp.math,
                      testgroupname = testgroup,
                      subjectname = isd.temp.math[,unique(Subject)])
    # ELA Plot
    testinggroup_plot(plot_data = isd.temp.ela,
                      testgroupname = testgroup,
                      subjectname = isd.temp.ela[,unique(Subject)])
    # Science Plot
    testinggroup_plot(plot_data = isd.temp.sci,
                      testgroupname = testgroup,
                      subjectname = isd.temp.sci[,unique(Subject)])
  }
}

#--- Grade ---
# Data for Science is only for Grades 0,11

# open pdf to populate with graphs
pdf(file = paste0(dir, repo, "Plots/YoY Percent Changes in MeanSGP by ISD and Grade.pdf"), width = 11, height = 8.5)

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
    grade_plot(plot_data =isd.temp.math,
               gradename = grade,
               subjectname = isd.temp.math[,unique(Subject)])
    # ELA Plot
    grade_plot(plot_data = isd.temp.ela,
               gradename = grade,
               subjectname = isd.temp.ela[,unique(Subject)])
    # Science Plot
    grade_plot(plot_data = isd.temp.sci,
               gradename = grade,
               subjectname = isd.temp.sci[,unique(Subject)])
  }
}



##### Year over Year (YoY) changes in MeanSGP for Hillsdale ISD #####
#--------------------------------------------------------------------
# Looking for Testing Group and Grade disparities by buildings, respectively

# X-axis label and column we are interested in
xlabel <- "Building Name"
col.interest = "BuildingName"

#--- Testing Group ---

# open pdf to populate with graphs
pdf(file = paste0(dir, repo, "Plots/Hillsdale - Buildings Percent Change in MeanSGP by Testing Group.pdf"), width = 11, height = 8.5)

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
    testinggroup_plot(plot_data = bld.temp.math,
                      testgroupname = testgroup,
                      subjectname = bld.temp.math[,unique(Subject)])
    # ELA Plot
    testinggroup_plot(plot_data = bld.temp.ela,
                      testgroupname = testgroup,
                      subjectname = bld.temp.ela[,unique(Subject)])
    # Science Plot
    testinggroup_plot(plot_data = bld.temp.sci,
                      testgroupname = testgroup,
                      subjectname = bld.temp.sci[,unique(Subject)])
  }
}

#--- Grade ---
# Data for Science is only for Grades 0,11

# open pdf to populate with graphs
pdf(file = paste0(dir, repo, "Plots/Hillsdale - Buildings, Percent Change in MeanSGP by Grade.pdf"), width = 11, height = 8.5)

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
    grade_plot(plot_data = bld.temp.math,
               gradename = grade,
               subjectname = bld.temp.math[,unique(Subject)])
    # ELA Plot
    grade_plot(plot_data = bld.temp.ela,
               gradename = grade,
               subjectname = bld.temp.ela[,unique(Subject)])
    # Science Plot
    grade_plot(plot_data = bld.temp.sci,
               gradename = grade,
               subjectname = bld.temp.sci[,unique(Subject)])
  }
}



##### Overall Plot and Comparison Plots #####
#--------------------------------------------

# open pdf to be able to populate with graphs
pdf(file = paste0(dir, repo, "Plots/Overall & Comparison Plots.pdf"), width = 11, height = 8.5)

#--- Plot showing YoY change in MeanSGP; Overall by Subject ---

# State of Michigan; isdcode = 0
dpscd_overall_plot(plot_data = isd.subjects, isdcode = 0)

# Hillsdale ISD; isdcode = 30
dpscd_overall_plot(plot_data = isd.subjects, isdcode = 30)


#--- Comparison plot showing YoY change in MeanSGP for the top & bottom 5 ISDs compared with the State of Michigan ---
# Math
comparison_plots(plot_data = isd.msgp.compare.math)
# ELA
comparison_plots(plot_data = isd.msgp.compare.ela)
# Sci
comparison_plots(plot_data = isd.msgp.compare.sci)

# close plots
graphics.off()
message("Done plotting, PDF ready for viewing.")