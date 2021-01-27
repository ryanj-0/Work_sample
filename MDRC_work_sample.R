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

if(getwd()==paste0("C:/Users/",Sys.info()[6],"/Documents/R/MDRC")){
  dir <- getwd()
  message(paste0("Working directory: ", getwd()))
} else{
  setwd(paste0("C:/Users/",Sys.info()[6],"/Documents/R/MDRC"))
  message(paste0("Directory changed, set to: ", getwd()))
}

# Set variables and sources needed functions
repo <- "/MDRC_sample"
source(paste0(dir,repo,"/plot_function_section1.R"))

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



##### Import Data #####

# Variable Key
key <- fread(file =
               paste0(dir, "/Data/MI Statewide Student Growth (File Layout Key).csv"))

# 2015-2016 School Year
year15 <- fread(file =
                  paste0(dir, "/Data/MI Statewide Student Growth 2015-16.csv"))

# 2016 - 2017 School Year
year16 <- fread(file =
                  paste0(dir, "/Data/MI Statewide Student Growth 2016-17.csv"))

# All Years Table: 2015-2017
yearall <- rbind(year15,year16,use.names = T)



##### Data Cleaning #####
yearall[SchoolYear=="15 - 16 School Year", SchoolYear:="15.16"]
yearall[SchoolYear=="16 - 17 School Year", SchoolYear:="16.17"]

# Grade Column
yearall[Grade%like%"All Grades",Grade:=0] # All = 0

# NAAG Column
yearall[NumberAboveAverageGrowth=="< 10",NumberAboveAverageGrowth:=9] #NumberAboveAverageGrowth (NAAG): 9 = < 10

# NAVG Column
yearall[NumberAverageGrowth=="< 10",NumberAverageGrowth:=9] #NumberAverageGrowth (NAVG): 9 = < 10

# NBAG Column
yearall[NumberBelowAverageGrowth=="< 10",NumberBelowAverageGrowth:=9] #NumberBelowAverageGrowth (NBAG): 9 = < 10

# PAA
yearall[PercentAboveAverage=="< 10",PercentAboveAverage:=9] #PercentAboveAverage (PAA): 9 = < 10
yearall[PercentAboveAverage=="< 5",PercentAboveAverage:=5] #PercentAboveAverage (PAA): 5 = < 5
yearall[PercentAboveAverage=="> 95",PercentAboveAverage:=95] #PercentAboveAverage (PAA): 95 = > 95

#PAVG
yearall[PercentAverageGrowth=="< 10",PercentAverageGrowth:=9] #PercentAverageGrowth (PAVG): 9 = < 10
yearall[PercentAverageGrowth=="< 5",PercentAverageGrowth:=5] #PercentAverageGrowth (PAVG): 5 = < 5
yearall[PercentAverageGrowth=="> 95",PercentAverageGrowth:=95] #PercentAverageGrowth (PAVG): 95 = > 95

#PBA
yearall[PercentBelowAverage=="< 10",PercentBelowAverage:=9] #PercentBelowAverage (PBA): 9 = < 10
yearall[PercentBelowAverage=="< 5",PercentBelowAverage:=5] #PercentBelowAverage (PBA): 5 = < 5
yearall[PercentBelowAverage=="> 95",PercentBelowAverage:=95] #PercentBelowAverage (PBA): 95 = > 95

# Total Included
yearall[TotalIncluded=="< 10",TotalIncluded:=10] #TotalIncluded: 10 = < 10

# MeanSGP (Student Growth Percentile)
yearall[MeanSGP=="< 10",MeanSGP:=10] #MeanSGP: 10 = < 10

#### Set columns as numeric ###
yearall[,':=' (IsdCode=as.numeric(IsdCode),
               DistrictCode=as.numeric(DistrictCode),
               BuildingCode=as.numeric(BuildingCode),
               Grade=as.numeric(Grade),
               NumberAboveAverageGrowth=as.numeric(NumberAboveAverageGrowth),
               NumberAverageGrowth=as.numeric(NumberAverageGrowth),
               NumberBelowAverageGrowth=as.numeric(NumberBelowAverageGrowth),
               PercentAboveAverage=as.numeric(PercentAboveAverage),
               PercentAverageGrowth=as.numeric(PercentAverageGrowth),
               PercentBelowAverage=as.numeric(PercentBelowAverage),
               TotalIncluded=as.numeric(TotalIncluded),
               MeanSGP=as.numeric(MeanSGP))]

### Use to investigate data while cleaning (un-comment to run; highlight, ctrl+shift+c) ###
# View(unique(yearall[,1])) # School Year
# View(unique(yearall[,2])) # IsdCode
# View(unique(yearall[,3])) # IsdName
# View(unique(yearall[,4])) # DistrictCode
# View(unique(yearall[,5])) # DistrictName
# View(unique(yearall[,6])) # BuildingName
# View(unique(yearall[,7])) # BuildingCode
# View(unique(yearall[,8])) # EntityType
# View(unique(yearall[,9])) # Grade
# View(unique(yearall[,10])) # Subject
# View(unique(yearall[,11])) # TestingGroup
# View(unique(yearall[,12])) # NAAG
# View(unique(yearall[,13])) # NAG
# View(unique(yearall[,14])) # NBAG
# View(unique(yearall[,15])) # PAA
# View(unique(yearall[,16])) # PAVG
# View(unique(yearall[,17])) # PBA
# View(unique(yearall[,18])) # TotalIncluded
# View(unique(yearall[,19])) # MeanSGP



#----------- Section 1 ------------------#
# Looking for Testing Group Disparities by ISD
# Year over Year (YoY) changes by MeanSGP

##### Yoy MeanSGP change by ISD ######
isd.msgp <- yearall[DistrictCode==0 & Grade==0]
isd.msgp[,c("DistrictCode",
            "DistrictName",
            "BuildingName",
            "BuildingCode",
            "Grade",
            "NumberAboveAverageGrowth",
            "NumberAverageGrowth",
            "NumberBelowAverageGrowth",
            "PercentAboveAverage",
            "PercentAverageGrowth",
            "PercentBelowAverage",
            "TotalIncluded"):=NULL] #drop columns; interested in only MeanSGP by Testing Group
# Need to split data into two different years to find YoY changes
isd.msgp.15 <- isd.msgp[SchoolYear=="15.16"] # only 15/16 data
isd.msgp.16 <- isd.msgp[SchoolYear=="16.17"] # only 16/17 data

# Note: Our data set will be smaller. i.e. missing records across the years
isd.msgp.delta <- merge(isd.msgp.15,
                        isd.msgp.16,
                        by = c("IsdCode",
                               "IsdName",
                               "EntityType",
                               "Subject",
                               "TestingGroup"),
                        suffixes = c("old","new"))
isd.msgp.delta[,delta:=MeanSGPnew-MeanSGPold]
isd.msgp.delta[,c("SchoolYearold","SchoolYearnew","MeanSGPold","MeanSGPnew"):=NULL] # drop old columns
isd.msgp <- isd.msgp[order(IsdCode,TestingGroup,Subject)] # reorder table for plotting

#--- Plotting Section ---
# for loop to plot YoY change in MeanSGP by testinggroup and subject
# open pdf to populate with graphs

pdf(file = paste0(dir, repo, "/YoY Percent Changes in MeanSGP by ISD and Testing Group.pdf"), width = 11, height = 8.5)

for (isd in 1:(length(isd.msgp.delta[,unique(TestingGroup)])+1)) {

  # sets TestingGroup for iteration
  testgroup <- isd.msgp.delta[,unique(TestingGroup)][isd]

  # Dynamic tables for plotting
  isd.temp.math <- isd.msgp.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  isd.temp.ela <- isd.msgp.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  isd.temp.sci <- isd.msgp.delta[TestingGroup==testgroup & Subject=="Science"]

  # Once if statement finish, closes pdf
  if(isd==length(isd.msgp.delta[,unique(TestingGroup)])+1){
    graphics.off()
    message("Done plotting, PDF ready for viewing.")
  }
  # Plots each subject for current testing group
  else{
    message("Plotting subjects for ", testgroup)
    # Math Plot
    section1_plot(isd.temp.math, testgroup, isd.temp.math[,unique(Subject)])
    # ELA Plot
    section1_plot(isd.temp.ela, testgroup, isd.temp.ela[,unique(Subject)])
    # Science Plot
    section1_plot(isd.temp.sci, testgroup, isd.temp.sci[,unique(Subject)])
  }
}



##### MeanSGP by Grade #####
# Data for Science is only for Grades 0,11

# Yoy MeanSGP change by ISD (All ISD included in DistrictCode=0), All Students (Testing Group)
isd.all.district.msgp.grade <- yearall[DistrictCode==0 & TestingGroup=="All Students"]
isd.all.district.msgp.grade <- isd.all.district.msgp.grade[order(IsdCode,Grade,Subject)] # reorder table
isd.all.district.msgp.grade[,c("DistrictCode",
                         "DistrictName",
                         "BuildingName",
                         "BuildingCode",
                         "TestingGroup",
                         "NumberAboveAverageGrowth",
                         "NumberAverageGrowth",
                         "NumberBelowAverageGrowth",
                         "PercentAboveAverage",
                         "PercentAverageGrowth",
                         "PercentBelowAverage",
                         "TotalIncluded"):=NULL] #drop columns
isd.all.district.msgp.15.grade <- isd.all.district.msgp.grade[SchoolYear=="15/16"]
isd.all.district.msgp.16.grade <- isd.all.district.msgp.grade[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.msgp.delta.grade <- merge(isd.all.district.msgp.15.grade,
                                     isd.all.district.msgp.16.grade,
                                     by = c("IsdCode",
                                            "IsdName",
                                            "EntityType",
                                            "Subject",
                                            "Grade"),
                                     suffixes = c("old","new"))
isd.all.district.msgp.delta.grade[,delta:=MeanSGPnew-MeanSGPold]
isd.all.district.msgp.delta.grade[,c("SchoolYearold","SchoolYearnew","MeanSGPold","MeanSGPnew"):=NULL] # drop old columns


#--- Plotting Section ---#

# open pdf to be able to populate with graphs
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in MeanSGP by Grade.pdf"), width = 11, height = 8.5)

for (i in 1:(length(isd.all.district.msgp.delta.grade[,unique(Grade)])+1)) {

  grade <- isd.all.district.msgp.delta.grade[,unique(Grade)][i]
  table.temp.math <- isd.all.district.msgp.delta.grade[Grade==grade & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.msgp.delta.grade[Grade==grade & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.msgp.delta.grade[Grade==grade & Subject=="Science"]

  # if statement to close pdf
  if(i==length(isd.all.district.msgp.delta.grade[,unique(Grade)])+1){
    dev.off()
  }
  else{
    print(grade)# Prints in console what grade is being used when making graphs

    # Math Plot
    plotty.math <-
      ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("Mathematics: All Students, ",grade, "th Grade"),
           caption = "*Mean Student Growth Percentile (Mean SGP); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
      xlab("ISD Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    # ELA Plot
    plotty.ela <-
      ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("English Language Arts: All Students, ",grade, "th Grade"),
           caption = "*Mean Student Growth Percentile (Mean SGP); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
      xlab("ISD Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    #  Sci Plot
    plotty.sci <-
      ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("Science: All Students, ",grade, "th Grade"),
           caption = "*Mean Student Growth Percentile (Mean SGP); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
      xlab("ISD Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    # Plotting Plots
    plot(plotty.math)
    plot(plotty.ela)
    plot(plotty.sci)
  }
}
#---- End ----

#--------- End Section ------------#


#---------- Section 2 ----------------#
# Line graph to show overall YoY changes
# By subjects and all students included; Social Science NA for 16/17 year

isd.subjects <- yearall[DistrictCode==0 & IsdCode==0 & TestingGroup=="All Students" & Grade==0]
hillsdale.subjects <- yearall[IsdCode==30 & Grade==0 & TestingGroup=="All Students" & DistrictCode==0]

overall.plot <-
  ggplot(isd.subjects, aes(x = SchoolYear, y = MeanSGP, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  labs(title = "State of Michigan",
       subtitle = "Changes in MeanSGP by Subject",
       caption = "Figure 1.") +
  xlab("School Year") +
  ylab("MeanSGP") +
  scale_x_discrete(labels = c("2015/2016", "2016/2017")) +
  theme(plot.caption = element_text(hjust = 0))



hillsdale.overall <-

  ggplot(hillsdale.subjects, aes(x = SchoolYear, y = MeanSGP, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  labs(title = "HillsDale ISD: All Districts",
       subtitle = "Changes in MeanSGP by Subject",
       caption = "") +
  xlab("School Year") +
  ylab("MeanSGP") +
  scale_x_discrete(labels = c("2015/2016", "2016/2017")) +
  theme(plot.caption = element_text(hjust = 0))



# -------- End Section -----------#


#------------- Section 3 -------------------#
# Looking to show the top & bottom 5 ISD
# by each Subject (All Students, All Grades)


##### Top/Bottom 5 Changes in MeanSGP by ISD (All Students, All Grades) #####
isd.all.district.msgp.delta.topbottom <- isd.all.district.msgp.delta[TestingGroup=="All Students"] # Get top/bottom 5 ISD, and state (All Students)
isd.all.district.msgp.delta.topbottom <- isd.all.district.msgp.delta.topbottom[order(-delta)] #order table by percent change largest to smallest


isd.all.district.msgp.top5.math <- head(isd.all.district.msgp.delta.topbottom[Subject=="Mathematics"],5) # return top 5 delta
isd.all.district.msgp.top5.ela <- head(isd.all.district.msgp.delta.topbottom[Subject=="English Language Arts"],5) # return top 5 delta
isd.all.district.msgp.top5.sci <- head(isd.all.district.msgp.delta.topbottom[Subject=="Science"],5) # return top 5 delta
isd.all.district.msgp.bottom5.math <- tail(isd.all.district.msgp.delta.topbottom[Subject=="Mathematics"],5) # return bottom 5 delta
isd.all.district.msgp.bottom5.ela <- tail(isd.all.district.msgp.delta.topbottom[Subject=="English Language Arts"],5) # return bottom 5 delta
isd.all.district.msgp.bottom5.sci <- tail(isd.all.district.msgp.delta.topbottom[Subject=="Science"],5) # return bottom 5 delta

isd.all.district.msgp.compare.math <- rbind(isd.all.district.msgp.top5.math,isd.all.district.msgp.bottom5.math) # Combine top & bottom 5 isds; math
isd.all.district.msgp.compare.math <- rbind(isd.all.district.msgp.compare.math,
                                            isd.all.district.msgp.delta.topbottom[IsdCode==0 & Subject=="Mathematics"]) # add statewide level

isd.all.district.msgp.compare.ela <- rbind(isd.all.district.msgp.top5.ela,isd.all.district.msgp.bottom5.ela) # combine top & bottom isds; ELA
isd.all.district.msgp.compare.ela <- rbind(isd.all.district.msgp.compare.ela,
                                       isd.all.district.msgp.delta.topbottom[IsdCode==0 & Subject=="English Language Arts"]) # add statewide level

isd.all.district.msgp.compare.sci <- rbind(isd.all.district.msgp.top5.sci,isd.all.district.msgp.bottom5.sci) # combine top & bottom isds; science
isd.all.district.msgp.compare.sci <- rbind(isd.all.district.msgp.compare.sci,
                                           isd.all.district.msgp.delta.topbottom[IsdCode==0 & Subject=="Science"]) # add statewide level


#--- Math Compare Plot ---#

compare.plot.math <-

  isd.all.district.msgp.compare.math %>%
  ggplot(aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient2(low = "red4",
                       mid = "steelblue",
                       high = "green",
                       midpoint = 0,
                       space = "Lab",
                       name = "% Change in Mean SGP") +
  labs(title = "Mathematics") +
  xlab("ISD Name") +
  ylab("Percent Change") +
  geom_text(aes(label = round(delta,2), hjust = ifelse(delta >0,0,1)), size = 4) +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 13)) +
  coord_flip()

#--- ELA Compare Plot ---#

compare.plot.ela <-

  isd.all.district.msgp.compare.ela %>%
  ggplot(aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient2(low = "red4",
                       mid = "steelblue",
                       high = "green",
                       midpoint = 0,
                       space = "Lab",
                       name = "% Change in Mean SGP") +
  labs(title = "English Language Arts") +
  xlab("ISD Name") +
  ylab("Percent Change") +
  geom_text(aes(label = round(delta,2), hjust = ifelse(delta >0,0,1)), size = 4) +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 13)) +
  coord_flip()


#--- Science Compare Plot ---#

compare.plot.sci <-

  isd.all.district.msgp.compare.sci %>%
  ggplot(aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient2(low = "red4",
                       mid = "steelblue",
                       high = "green",
                       midpoint = 0,
                       space = "Lab",
                       name = "% Change in Mean SGP") +
  labs(title = "Science") +
  xlab("ISD Name") +
  ylab("Percent Change") +
  geom_text(aes(label = round(delta,2), hjust = ifelse(delta >0,0,1)), size = 4) +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 13)) +
  coord_flip()

#---- End ----

#----------- End Section ----------#


#-------- Plot Section 2 & 3 --------#
# open pdf to be able to populate with graphs
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/Overall & Comparison Plots.pdf"), width = 11, height = 8.5)

plot(overall.plot)
plot(hillsdale.overall)
plot(compare.plot.math)
plot(compare.plot.ela)
plot(compare.plot.sci)

dev.off()

#------- End of Section --------#


#--------- Section 4 ----------------#
#look at changes in MeanSGP for Hillsdale ISD
#by TestingGroup and Grade

hillsdale.all <- yearall[IsdCode==30]

##### MeanSGP by Testing Group #####

# Yoy MeanSGP change in Hillsdale
hillsdale.msgp <- hillsdale.all[!Subject=="Social Studies"]
hillsdale.msgp <- hillsdale.all[Grade==0]
hillsdale.msgp <- hillsdale.msgp[!BuildingCode==0]
hillsdale.msgp <- hillsdale.msgp[order(DistrictCode,TestingGroup,Subject)]
hillsdale.msgp[,c("IsdCode",
                   "IsdName",
                   "Grade",
                   "NumberAboveAverageGrowth",
                   "NumberAverageGrowth",
                   "NumberBelowAverageGrowth",
                   "PercentAboveAverage",
                   "PercentAverageGrowth",
                   "PercentBelowAverage",
                   "TotalIncluded"):=NULL] #drop columns
hillsdale.msgp.15 <- hillsdale.msgp[SchoolYear=="15/16"]
hillsdale.msgp.16 <- hillsdale.msgp[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
hillsdale.msgp.delta <- merge(hillsdale.msgp.15,
                              hillsdale.msgp.16,
                              by = c("DistrictName",
                                     "DistrictCode",
                                     "BuildingName",
                                     "BuildingCode",
                                     "EntityType",
                                     "Subject",
                                     "TestingGroup"),
                              suffixes = c("old","new"))
hillsdale.msgp.delta[,delta:=MeanSGPnew-MeanSGPold]
hillsdale.msgp.delta[,c("SchoolYearold","SchoolYearnew","MeanSGPold","MeanSGPnew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/Hillsdale - Buildings, Percent Changes in MeanSGP by Testing Group.pdf"),
    width = 11, height = 8.5)

for (i in 1:(length(hillsdale.msgp.delta[,unique(TestingGroup)])+1)) {
  testgroup <- hillsdale.msgp.delta[,unique(TestingGroup)][i]
  table.temp.math <- hillsdale.msgp.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  table.temp.ela <- hillsdale.msgp.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  table.temp.sci <- hillsdale.msgp.delta[TestingGroup==testgroup & Subject=="Science"]


  # if statement to close pdf
  if(i==length(hillsdale.msgp.delta[,unique(TestingGroup)])+1){
    dev.off()
  }
  else{
    print(testgroup) # print testgroup that is being plotted

    # Math Plot
    plotty.math <-
      ggplot(table.temp.math,aes(x = reorder(BuildingName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("Mathematics: All Grades, ",testgroup),
           caption = "*Mean Student Growth Percentile (Mean SGP)") +
      xlab("Building Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    # ELA Plot
    plotty.ela <-
      ggplot(table.temp.ela,aes(x = reorder(BuildingName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("English Language Arts: All Grades, ",testgroup),
           caption = "*Mean Student Growth Percentile (Mean SGP)") +
      xlab("Building Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    #  Sci Plot
    plotty.sci <-
      ggplot(table.temp.sci,aes(x = reorder(BuildingName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("Science: All Grades, ",testgroup),
           caption = "*Mean Student Growth Percentile (Mean SGP)") +
      xlab("Building Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    # Plotting Plots
    plot(plotty.math)
    plot(plotty.ela)
    plot(plotty.sci)
  }
}
#---- End -----

##### MeanSGP by Grade #####

# Yoy MeanSGP change by ISD (all buildings inherent in IsdCode=0), All Students (Grade)
hillsdale.msgp <- hillsdale.all[!Subject=="Social Studies"]
hillsdale.msgp <- hillsdale.all[TestingGroup=="All Students"]
hillsdale.msgp <- hillsdale.msgp[!BuildingCode==0]
hillsdale.msgp <- hillsdale.msgp[order(DistrictCode,TestingGroup,Subject)]
hillsdale.msgp[,c("IsdCode",
                  "IsdName",
                  "TestingGroup",
                  "NumberAboveAverageGrowth",
                  "NumberAverageGrowth",
                  "NumberBelowAverageGrowth",
                  "PercentAboveAverage",
                  "PercentAverageGrowth",
                  "PercentBelowAverage",
                  "TotalIncluded"):=NULL] #drop columns
hillsdale.msgp.15 <- hillsdale.msgp[SchoolYear=="15/16"]
hillsdale.msgp.16 <- hillsdale.msgp[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
hillsdale.msgp.delta <- merge(hillsdale.msgp.15,
                              hillsdale.msgp.16,
                              by = c("DistrictName",
                                     "DistrictCode",
                                     "BuildingName",
                                     "BuildingCode",
                                     "EntityType",
                                     "Subject",
                                     "Grade"),
                              suffixes = c("old","new"))
hillsdale.msgp.delta[,delta:=MeanSGPnew-MeanSGPold]
hillsdale.msgp.delta[,c("SchoolYearold","SchoolYearnew","MeanSGPold","MeanSGPnew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/Hillsdale - Buildings, Percent Changes in MeanSGP by Grade.pdf"),
    width = 11, height = 8.5)

for (i in 1:(length(hillsdale.msgp.delta[,unique(Grade)])+1)) {
  grade <- hillsdale.msgp.delta[,unique(Grade)][i]
  table.temp.math <- hillsdale.msgp.delta[Grade==grade & Subject=="Mathematics"]
  table.temp.ela <- hillsdale.msgp.delta[Grade==grade & Subject=="English Language Arts"]
  table.temp.sci <- hillsdale.msgp.delta[Grade==grade & Subject=="Science"]

  if(i==length(hillsdale.msgp.delta[,unique(Grade)])+1){
    dev.off()
  }
  else{
    print(grade)

    # Math Plot
    plotty.math <-
      ggplot(table.temp.math,aes(x = reorder(BuildingName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("Mathematics: All Grades, ",grade, "th Grade"),
           caption = "*Mean Student Growth Percentile (Mean SGP); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
      xlab("Building Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    # ELA Plot
    plotty.ela <-
      ggplot(table.temp.ela,aes(x = reorder(BuildingName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("English Language Arts: All Grades, ",grade, "th Grade"),
           caption = "*Mean Student Growth Percentile (Mean SGP); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
      xlab("Building Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    #  Sci Plot
    plotty.sci <-
      ggplot(table.temp.sci,aes(x = reorder(BuildingName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue4",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change: Mean SGP*") +
      labs(title = "Percent Changes in Mean SGP* from 2015/2016 - 2016/2017 ",
           subtitle = paste0("Science: All Grades, ",testgroup),
           caption = "*Mean Student Growth Percentile (Mean SGP); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
      xlab("Building Name") +
      ylab("Percent Change") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

    # Plotting Plots
    plot(plotty.math)
    plot(plotty.ela)
    plot(plotty.sci)
  }
}
dev.off()
##### END: MeanSGP by Grade ####

#------ End of Section ------------#









# ---------------

davis.subjects <- yearall[ BuildingCode==876 & Grade==0 & TestingGroup=="All Students"]


ggplot(davis.subjects, aes(x = SchoolYear, y = MeanSGP, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  labs(title = "Davis Middle School",
       subtitle = "Changes in MeanSGP by Subject",
       caption = "No data for Science 2016/2017") +
  xlab("School Year") +
  ylab("MeanSGP") +
  scale_x_discrete(labels = c("2015/2016", "2016/2017")) +
  theme(plot.caption = element_text(hjust = 0))






##### Plot change in MeanSGP by building, testing group, and grade #####

pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/Percent MeanSGP Change by Building.pdf"), width = 11, height = 8.5)

for (Bld in 1:(length(yearall[,unique(BuildingName)])+1)) {# Loop to choose individual school (1st loop)

  #progress(Bld) # Shows progress of graphing

  if(i==length(yearall[,unique(BuildingName)])+1){
    print(paste0("End plotting ", temp.buildingname, " -----------------------"))
    dev.off()
  }


  temp.building <- yearall[BuildingCode==Bld]
  temp.building <- temp.building[order(Grade,TestingGroup)]#Orders data so consistent print of graphs

  # Get info for graphs
  temp.entity <- temp.building[,unique(EntityType)]
  temp.isdname <- temp.building[,unique(IsdName)]
  temp.districtname <- temp.building[,unique(DistrictName)]
  temp.buildingname <- temp.building[,unique(BuildingName)]
  testgrouplength <- length(temp.building[,unique(TestingGroup)])

  # Print info from graphs
  # print(paste0("Entity Type: ",temp.entity))
  # print(paste0("ISD Name: ",temp.isdname))
  # print(paste0("District Name: ", temp.districtname))
  # print(paste0("Building Name: " ,temp.buildingname))
  # print(paste0("# of Test Groups: ", testgrouplength))

  for(TG in 1:testgrouplength){ # 2nd Loop

    temp.testinggroup <- temp.building[,unique(TestingGroup)][TG]# Gets testing group name
    gradelength <- length(temp.building[,unique(Grade)])
    #print(paste0("Testing Group: ", temp.testinggroup))

    for(G in 1:gradelength){ # 3rd Loop

      temp.grade <- temp.building[,unique(Grade)][G]
      #print(paste0("Grade: ", temp.grade))

      #Checks if there is data for both years, if no data, moves to next plot
      if(nrow(temp.building[Grade==temp.grade & TestingGroup==temp.testinggroup])<=2){
        print(G)
        print("if statement")
        #print("Not enough data, plot not made.")
        next
      }
      else{
        plotty <- ggplot(temp.building[Grade==temp.grade & TestingGroup==temp.testinggroup],
                         aes(x = SchoolYear, y = MeanSGP, color = Subject)) +
          geom_point() +
          geom_line(aes(group = Subject)) +
          labs(title = "Changes in MeanSGP by Subject",
               subtitle = paste(temp.buildingname, ": ", temp.testinggroup,", ", temp.grade, "th Grade"),
               caption = paste0(temp.entity, ",", temp.isdname, ",", temp.districtname),
               x = "School Year",
               y = "MeanSGP") +
          geom_text_repel(aes(label = TotalIncluded), show.legend = FALSE) +
          scale_x_discrete(labels = c("2015/2016", "2016/2017")) +
          theme(plot.caption = element_text(hjust = 0))

        print(G)
        print("else statement")
      }


      plot(plotty)
    } # End 3rd Loop

  } # End 2nd Loop

} # End 1st Loop



#### End of Section ####




##### Extraneous Code and Graphs ######





# Data for all of Michigan; Mathematics; All Grades
mi.data.allsubjects <-

# Objective: Present certain Testing Groups differences.
mi.data.math.15 <- mi.data.math[SchoolYear=="15/16"] #15/16 data
mi.data.math.15[,c("IsdCode",
                   "IsdName",
                   "DistrictCode",
                   "DistrictName",
                   "BuildingName",
                   "BuildingCode",
                   "EntityType",
                   "Grade",
                   "Subject",
                   "NumberAboveAverageGrowth",
                   "NumberAverageGrowth",
                   "NumberBelowAverageGrowth"):=NULL] #remove columns


mi.data.math.16 <- mi.data.math[SchoolYear=="16/17"] #16/17 data
mi.data.math.16[,c("IsdCode",
                   "IsdName",
                   "DistrictCode",
                   "DistrictName",
                   "BuildingName",
                   "BuildingCode",
                   "EntityType",
                   "Grade",
                   "Subject",
                   "NumberAboveAverageGrowth",
                   "NumberAverageGrowth",
                   "NumberBelowAverageGrowth"):=NULL] #remove columns

# Merge data sets to find deltas
mi.data.math.delta <- merge(mi.data.math.15,mi.data.math.16, by=c("TestingGroup"), all.x = TRUE)
mi.data.math.delta[,":=" (PAA.delta=PercentAboveAverage.y-PercentAboveAverage.x,
                          PAG.delta=PercentAverageGrowth.y-PercentAverageGrowth.x,
                          PBAG.delta=PercentBelowAverage.y-PercentBelowAverage.x,
                          Total.delta=TotalIncluded.y-TotalIncluded.x,
                          MeanSGP.delta=MeanSGP.y-MeanSGP.x)]
mi.data.math.delta[,2:13:=NULL] # keep only deltas
mi.data.math.delta[,Total.delta:=NULL]
mi.data.math.delta <- mi.data.math.delta[!4] #remove Testing Group 'Asian or Pacific Island' b/c difference in definition


#Plot of deltas by TestingGroup
melt(mi.data.math.delta,id.vars = "TestingGroup") %>%
  ggplot(aes(reorder(TestingGroup,value), value, group = variable, fill = variable)) +
  geom_col() +
  facet_wrap(~variable, scales = "free") +
  geom_hline(yintercept = 0) +
  ggtitle("Percent Changes in Michigan for Mathematics") +
  xlab("Testing Groups") +
  ylab("Percent Change")+
  theme(axis.text.x = element_text(angle = 90))


#Plot of deltas by TestingGroup & PAA delta
mi.data.math.delta %>%
  ggplot(aes(x = reorder(TestingGroup, PAA.delta), y = PAA.delta)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  ggtitle("Percent Changes in Michigan for Mathematics: PAA") +
  xlab("Testing Groups") +
  ylab("Percent Change")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))

#Plot of deltas by TestingGroup & PAG delta
mi.data.math.delta %>%
  ggplot(aes(x = reorder(TestingGroup, PAG.delta), y = PAG.delta)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  ggtitle("Percent Changes in Michigan for Mathematics: PAG") +
  xlab("Testing Groups") +
  ylab("Percent Change")+
  theme(axis.text.x = element_text(angle = 90))

#Plot of deltas by TestingGroup & PBAG delta
mi.data.math.delta %>%
  ggplot(aes(x = reorder(TestingGroup, PBAG.delta), y = PBAG.delta)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  ggtitle("Percent Changes in Michigan for Mathematics: PBAG") +
  xlab("Testing Groups") +
  ylab("Percent Change")+
  theme(axis.text.x = element_text(angle = 90))

#Plot of deltas by TestingGroup & MeanSGP delta
mi.data.math.delta %>%
  ggplot(aes(x = reorder(TestingGroup, MeanSGP.delta), y = MeanSGP.delta)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  ggtitle("Percent Changes in Michigan for Mathematics: Mean SGP") +
  xlab("Testing Groups") +
  ylab("Percent Change")+
  theme(axis.text.x = element_text(angle = 90))








# Data here will be broken into 4 subjects: Math, Science, Social Studies, and English Language Arts
# Subsequent subgroups will be commented



# YoY comparison by Subject and PAA for all of Michigan
alldistrics[IsdCode==0] %>%
  ggplot(aes(x = Subject , y = PercentAboveAverage, fill = SchoolYear )) +
  geom_col(position = "dodge") +
  ggtitle("YoY PAA in Mathematics for Michigan by Subject") +
  xlab("Subject") +
  ylab("Percent Above Average")+
  theme(axis.text.x = element_text(angle = 90))


#Math
allmath <- yearall[Subject=="Mathematics"] #all years

#Science
allSci <- yearall[Subject=="Science"]

#Social Studies
allSS <- yearall[Subject=="Social Studies"]

#English Language Arts
allELA <- yearall[Subject=="English Language Arts"]


#- Histograms of Subjects -#
#-- All Math at ISD Level, YoY, PAA ---#
math.isd <- allmath[Grade==0 & DistrictCode==0 & TestingGroup=="All Students"]

# Barplot
math.isd %>%
  ggplot(aes(x = IsdName, y = PercentAboveAverage, fill = SchoolYear)) +
  geom_col(position = "dodge") +
  ggtitle("YoY PAA in Mathematics for Michigan by ISD") +
  xlab("ISD") +
  ylab("Percent Above Average (PAA)")+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()


#-- Investigate Bar Chart --#

# 15/16 Math at ISD Level
math.isd.15 <- math.isd[PercentAboveAverage & SchoolYear=="15/16"]
math.isd.15[,c("DistrictCode",
               "DistrictName",
               "BuildingName",
               "BuildingCode",
               "Grade",
               "Subject",
               "TestingGroup",
               "NumberAboveAverageGrowth",
               "NumberAverageGrowth",
               "NumberBelowAverageGrowth",
               "PercentAverageGrowth",
               "PercentBelowAverage",
               "TotalIncluded",
               "MeanSGP")
            :=NULL] # Drop extraneous columns

# 16/17 Math at ISD Level
math.isd.16 <- math.isd[PercentAboveAverage & SchoolYear=="16/17"]
math.isd.16[,c("DistrictCode",
               "DistrictName",
               "BuildingName",
               "BuildingCode",
               "Grade",
               "Subject",
               "TestingGroup",
               "NumberAboveAverageGrowth",
               "NumberAverageGrowth",
               "NumberBelowAverageGrowth",
               "PercentAverageGrowth",
               "PercentBelowAverage",
               "TotalIncluded",
               "MeanSGP")
            :=NULL] # Drop extraneous columns

#Merge math.isd.15 & =.=.16
math.isd.delta <- merge(math.isd.15,math.isd.16, by=c("IsdCode","IsdName","EntityType"), all.x = TRUE)
math.isd.delta[,delta.yr:=PercentAboveAverage.x-PercentAboveAverage.y]
math.isd.delta[,c("PercentAboveAverage.x",
                  "PercentAboveAverage.y",
                  "SchoolYear.x",
                  "SchoolYear.y")
               :=NULL]
#math.isd.delta <- math.isd.delta[,marker:= abs(delta.yr)]

#Plot of PAA delta YoY
math.isd.delta %>%
  ggplot(aes(x = reorder(IsdName, delta.yr) , y = delta.yr, color = delta.yr)) +
  geom_hline(yintercept = c(-5,0,5), color = 'black', linetype = 2) +
  geom_point(shape=16, size=5)+
  scale_color_gradient2(low = "red4",
                        mid = "blue",
                        high = "green",
                        midpoint = 0,
                        space = "Lab") +
  ggtitle("YoY Percent Change of PAA in Mathematics for Michigan by ISD, Accending") +
  xlab("ISD") +
  ylab("Percent Change")+
  theme(axis.text.x = element_text(angle = 90))

#ISD of interest: Clinton County RESA(19)
clinton.county.resa <- yearall[IsdCode==19 & Subject=="Mathematics"]

# YoY comparison of PAA in Math for Clinton County RESA by Grade
clinton.county.resa %>%
  ggplot(aes(x = factor(Grade), y = PercentAboveAverage, fill = SchoolYear )) +
  geom_col(position = "dodge") +
  ggtitle("Clinton County RESA YoY Comparison of PAA in Mathematics by Grade") +
  xlab("Grade") +
  ylab("Percent Above Average")

# YoY comparison of PAA in Math for Clinton County RESA by Testing Group
clinton.county.resa %>%
  ggplot(aes(x = TestingGroup, y = PercentAboveAverage, fill = SchoolYear )) +
  geom_col(position = "dodge") +
  ggtitle("Clinton County RESA YoY Comparison of PAA in Mathematics by Testing Group") +
  xlab("Testing Group") +
  ylab("Percent Above Average") +
  theme(axis.text.x = element_text(angle = 90))

# YoY comparison of PAA in Math for Clinton County RESA by District
clinton.county.resa %>%
  ggplot(aes(x = DistrictName, y = PercentAboveAverage, fill = SchoolYear )) +
  geom_col(position = "dodge") +
  ggtitle("Clinton County RESA YoY Comparison of PAA in Mathematics by District") +
  xlab("District Name") +
  ylab("Percent Above Average") +
  theme(axis.text.x = element_text(angle = 90))

# YoY comparison of PAA in Math for Clinton County RESA by Building
clinton.county.resa %>%
  ggplot(aes(x = BuildingName, y = PercentAboveAverage, fill = SchoolYear )) +
  geom_col(position = "dodge") +
  ggtitle("Clinton County RESA YoY Comparison of PAA in Mathematics by Building") +
  xlab("Building Name") +
  ylab("Percent Above Average") +
  theme(axis.text.x = element_text(angle = 90))



##### PAA by Testing Group #####

# Yoy PAA change by ISD (all buildings inherent in IsdCode=0), All Students (Grade)
isd.all.district.paa <- yearall[DistrictCode==0 & Grade==0]
isd.all.district.paa <- isd.all.district.paa[order(IsdCode,TestingGroup,Subject)]
isd.all.district.paa[,c("DistrictCode",
                        "DistrictName",
                        "BuildingName",
                        "BuildingCode",
                        "Grade",
                        "NumberAboveAverageGrowth",
                        "NumberAverageGrowth",
                        "NumberBelowAverageGrowth",
                        "PercentAverageGrowth",
                        "PercentBelowAverage",
                        "TotalIncluded",
                        "MeanSGP"):=NULL] #drop columns
isd.all.district.paa.15 <- isd.all.district.paa[SchoolYear=="15/16"]
isd.all.district.paa.16 <- isd.all.district.paa[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.paa.delta <- merge(isd.all.district.paa.15,
                                    isd.all.district.paa.16,
                                    by = c("IsdCode",
                                           "IsdName",
                                           "EntityType",
                                           "Subject",
                                           "TestingGroup"),
                                    suffixes = c("old","new"))
isd.all.district.paa.delta[,delta:=PercentAboveAveragenew-PercentAboveAverageold]
isd.all.district.paa.delta[,c("SchoolYearold","SchoolYearnew","PercentAboveAverageold","PercentAboveAveragenew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in PAA by Testing Group.pdf"), width = 11, height = 8.5)
for (i in 1:17) {
  testgroup <- isd.all.district.paa.delta[,unique(TestingGroup)][i]
  table.temp.math <- isd.all.district.paa.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.paa.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.paa.delta[TestingGroup==testgroup & Subject=="Science"]
  print(testgroup)

  # Math Plot
  plotty.math <-
    ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAA*") +
    labs(title = "Percent Changes in PAA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Mathematics: All Grades, ",testgroup),
         caption = "*Percent Above Average (PAA)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # ELA Plot
  plotty.ela <-
    ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAA*") +
    labs(title = "Percent Changes in PAA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("English Language Arts: All Grades, ",testgroup),
         caption = "*Percent Above Average (PAA)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  #  Sci Plot
  plotty.sci <-
    ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAA*") +
    labs(title = "Percent Changes in PAA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Science: All Grades, ",testgroup),
         caption = "*Percent Above Average (PAA)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # Plotting Plots
  plot(plotty.math)
  plot(plotty.ela)
  plot(plotty.sci)
}
dev.off()
##### END: PAA by Testing Group ####

##### PAG by Testing Group #####

# Yoy PAG change by ISD (all buildings inherent in IsdCode=0), All Students (Grade)
isd.all.district.pag <- yearall[DistrictCode==0 & Grade==0]
isd.all.district.pag <- isd.all.district.pag[order(IsdCode,TestingGroup,Subject)]
isd.all.district.pag[,c("DistrictCode",
                        "DistrictName",
                        "BuildingName",
                        "BuildingCode",
                        "Grade",
                        "NumberAboveAverageGrowth",
                        "NumberAverageGrowth",
                        "NumberBelowAverageGrowth",
                        "PercentAboveAverage",
                        "PercentBelowAverage",
                        "TotalIncluded",
                        "MeanSGP"):=NULL] #drop columns
isd.all.district.pag.15 <- isd.all.district.pag[SchoolYear=="15/16"]
isd.all.district.pag.16 <- isd.all.district.pag[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.pag.delta <- merge(isd.all.district.pag.15,
                                    isd.all.district.pag.16,
                                    by = c("IsdCode",
                                           "IsdName",
                                           "EntityType",
                                           "Subject",
                                           "TestingGroup"),
                                    suffixes = c("old","new"))
isd.all.district.pag.delta[,delta:=PercentAverageGrowthnew-PercentAverageGrowthold]
isd.all.district.pag.delta[,c("SchoolYearold","SchoolYearnew","PercentAverageGrowthold","PercentAverageGrowthnew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in PAG by Testing Group.pdf"), width = 11, height = 8.5)
for (i in 1:17) {
  testgroup <- isd.all.district.pag.delta[,unique(TestingGroup)][i]
  table.temp.math <- isd.all.district.pag.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.pag.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.pag.delta[TestingGroup==testgroup & Subject=="Science"]
  print(testgroup)

  # Math Plot
  plotty.math <-
    ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAG*") +
    labs(title = "Percent Changes in PAG* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Mathematics: All Grades, ",testgroup),
         caption = "*Percent Average Growth (PAG)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # ELA Plot
  plotty.ela <-
    ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAG*") +
    labs(title = "Percent Changes in PAG* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("English Language Arts: All Grades, ",testgroup),
         caption = "*Percent Average Growth (PAG)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  #  Sci Plot
  plotty.sci <-
    ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAG*") +
    labs(title = "Percent Changes in PAG* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Science: All Grades, ",testgroup),
         caption = "*Percent Average Growth (PAG)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # Plotting Plots
  plot(plotty.math)
  plot(plotty.ela)
  plot(plotty.sci)
}
dev.off()
##### END: PAG by Testing Group ####

##### PBA by Testing Group ####

# Yoy pba change by ISD (all buildings inherent in IsdCode=0), All Students (Grade)
isd.all.district.pba <- yearall[DistrictCode==0 & Grade==0]
isd.all.district.pba <- isd.all.district.pba[order(IsdCode,TestingGroup,Subject)]
isd.all.district.pba[,c("DistrictCode",
                        "DistrictName",
                        "BuildingName",
                        "BuildingCode",
                        "Grade",
                        "NumberAboveAverageGrowth",
                        "NumberAverageGrowth",
                        "NumberBelowAverageGrowth",
                        "PercentAboveAverage",
                        "PercentAverageGrowth",
                        "TotalIncluded",
                        "MeanSGP"):=NULL] #drop columns
isd.all.district.pba.15 <- isd.all.district.pba[SchoolYear=="15/16"]
isd.all.district.pba.16 <- isd.all.district.pba[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.pba.delta <- merge(isd.all.district.pba.15,
                                    isd.all.district.pba.16,
                                    by = c("IsdCode",
                                           "IsdName",
                                           "EntityType",
                                           "Subject",
                                           "TestingGroup"),
                                    suffixes = c("old","new"))
isd.all.district.pba.delta[,delta:=PercentBelowAveragenew-PercentBelowAverageold]
isd.all.district.pba.delta[,c("SchoolYearold","SchoolYearnew","PercentBelowAverageold","PercentBelowAveragenew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in PBA by Testing Group.pdf"), width = 11, height = 8.5)
for (i in 1:17) {
  testgroup <- isd.all.district.pba.delta[,unique(TestingGroup)][i]
  table.temp.math <- isd.all.district.pba.delta[TestingGroup==testgroup & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.pba.delta[TestingGroup==testgroup & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.pba.delta[TestingGroup==testgroup & Subject=="Science"]
  print(testgroup)

  # Math Plot
  plotty.math <-
    ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PBA*") +
    labs(title = "Percent Changes in PBA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Mathematics: All Grades, ",testgroup),
         caption = "*Percent Below Average (PBA)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # ELA Plot
  plotty.ela <-
    ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PBA*") +
    labs(title = "Percent Changes in PBA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("English Language Arts: All Grades, ",testgroup),
         caption = "*Percent Below Average (PBAG)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  #  Sci Plot
  plotty.sci <-
    ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PBA*") +
    labs(title = "Percent Changes in PBA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Science: All Grades, ",testgroup),
         caption = "*Percent Below Average (PBA)") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # Plotting Plots
  plot(plotty.math)
  plot(plotty.ela)
  plot(plotty.sci)
}
dev.off()
##### END: PBAG by Testing Group ####


##### PAA by Grade #####

# Yoy PAA change by ISD (all buildings inherent in IsdCode=0), All Students (Testing Group)
isd.all.district.paa.grade <- yearall[DistrictCode==0 & TestingGroup=="All Students"]
isd.all.district.paa.grade <- isd.all.district.paa.grade[order(IsdCode,Grade,Subject)]
isd.all.district.paa.grade[,c("DistrictCode",
                              "DistrictName",
                              "BuildingName",
                              "BuildingCode",
                              "TestingGroup",
                              "NumberAboveAverageGrowth",
                              "NumberAverageGrowth",
                              "NumberBelowAverageGrowth",
                              "PercentAverageGrowth",
                              "PercentBelowAverage",
                              "TotalIncluded",
                              "MeanSGP"):=NULL] #drop columns
isd.all.district.paa.15.grade <- isd.all.district.paa.grade[SchoolYear=="15/16"]
isd.all.district.paa.16.grade <- isd.all.district.paa.grade[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.paa.delta.grade <- merge(isd.all.district.paa.15.grade,
                                          isd.all.district.paa.16.grade,
                                          by = c("IsdCode",
                                                 "IsdName",
                                                 "EntityType",
                                                 "Subject",
                                                 "Grade"),
                                          suffixes = c("old","new"))
isd.all.district.paa.delta.grade[,delta:=PercentAboveAveragenew-PercentAboveAverageold]
isd.all.district.paa.delta.grade[,c("SchoolYearold","SchoolYearnew","PercentAboveAverageold","PercentAboveAveragenew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in PAA by Grade.pdf"), width = 11, height = 8.5)
for (i in 1:7) {
  grade <- isd.all.district.paa.delta.grade[,unique(Grade)][i]
  table.temp.math <- isd.all.district.paa.delta.grade[Grade==grade & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.paa.delta.grade[Grade==grade & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.paa.delta.grade[Grade==grade & Subject=="Science"]
  print(grade)

  # Math Plot
  plotty.math <-
    ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAA*") +
    labs(title = "Percent Changes in PAA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Mathematics: All Students, ",grade, "th Grade"),
         caption = "*Percent Above Average (PAA); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # ELA Plot
  plotty.ela <-
    ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAA*") +
    labs(title = "Percent Changes in PAA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("English Language Arts: All Students, ",grade, "th Grade"),
         caption = "*Percent Above Average (PAA); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  #  Sci Plot
  plotty.sci <-
    ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAA*") +
    labs(title = "Percent Changes in PAA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Science: All Students, ",grade, "th Grade"),
         caption = "*Percent Above Average (PAA); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # Plotting Plots
  plot(plotty.math)
  plot(plotty.ela)
  plot(plotty.sci)
}
dev.off()
##### END: PAA by Grade ####

##### PAG by Grade#####

# Yoy PAG change by ISD (all buildings inherent in IsdCode=0), All Students (Testing Groups)
isd.all.district.pag.grade <- yearall[DistrictCode==0 & TestingGroup=="All Students"]
isd.all.district.pag.grade <- isd.all.district.pag.grade[order(IsdCode,Grade,Subject)]
isd.all.district.pag.grade[,c("DistrictCode",
                              "DistrictName",
                              "BuildingName",
                              "BuildingCode",
                              "TestingGroup",
                              "NumberAboveAverageGrowth",
                              "NumberAverageGrowth",
                              "NumberBelowAverageGrowth",
                              "PercentAboveAverage",
                              "PercentBelowAverage",
                              "TotalIncluded",
                              "MeanSGP"):=NULL] #drop columns
isd.all.district.pag.15.grade <- isd.all.district.pag.grade[SchoolYear=="15/16"]
isd.all.district.pag.16.grade <- isd.all.district.pag.grade[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.pag.delta.grade <- merge(isd.all.district.pag.15.grade,
                                          isd.all.district.pag.16.grade,
                                          by = c("IsdCode",
                                                 "IsdName",
                                                 "EntityType",
                                                 "Subject",
                                                 "Grade"),
                                          suffixes = c("old","new"))
isd.all.district.pag.delta.grade[,delta:=PercentAverageGrowthnew-PercentAverageGrowthold]
isd.all.district.pag.delta.grade[,c("SchoolYearold","SchoolYearnew","PercentAverageGrowthold","PercentAverageGrowthnew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in PAG by Grade.pdf"), width = 11, height = 8.5)
for (i in 1:7) {
  grade <- isd.all.district.pag.delta.grade[,unique(Grade)][i]
  table.temp.math <- isd.all.district.pag.delta.grade[Grade==grade & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.pag.delta.grade[Grade==grade & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.pag.delta.grade[Grade==grade & Subject=="Science"]
  print(grade)

  # Math Plot
  plotty.math <-
    ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAG*") +
    labs(title = "Percent Changes in PAG* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Mathematics: All Students, ",grade,"th Grade"),
         caption = "*Percent Average Growth (PAG); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # ELA Plot
  plotty.ela <-
    ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAG*") +
    labs(title = "Percent Changes in PAG* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("English Language Arts: All Students, ",grade,"th Grade"),
         caption = "*Percent Average Growth (PAG); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  #  Sci Plot
  plotty.sci <-
    ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PAG*") +
    labs(title = "Percent Changes in PAG* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Science: All Students, ",grade, "th Grade"),
         caption = "*Percent Average Growth (PAG); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # Plotting Plots
  plot(plotty.math)
  plot(plotty.ela)
  plot(plotty.sci)
}
dev.off()
##### END: PAG by Grade ####

##### PBA by Testing Group ####

# Yoy pba change by ISD (all buildings inherent in IsdCode=0), All Students (Testing Group)
isd.all.district.pba.grade <- yearall[DistrictCode==0 & TestingGroup=="All Students"]
isd.all.district.pba.grade <- isd.all.district.pba.grade[order(IsdCode,Grade,Subject)]
isd.all.district.pba.grade[,c("DistrictCode",
                              "DistrictName",
                              "BuildingName",
                              "BuildingCode",
                              "TestingGroup",
                              "NumberAboveAverageGrowth",
                              "NumberAverageGrowth",
                              "NumberBelowAverageGrowth",
                              "PercentAboveAverage",
                              "PercentAverageGrowth",
                              "TotalIncluded",
                              "MeanSGP"):=NULL] #drop columns
isd.all.district.pba.15.grade <- isd.all.district.pba.grade[SchoolYear=="15/16"]
isd.all.district.pba.16.grade <- isd.all.district.pba.grade[SchoolYear=="16/17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.all.district.pba.delta.grade <- merge(isd.all.district.pba.15.grade,
                                          isd.all.district.pba.16.grade,
                                          by = c("IsdCode",
                                                 "IsdName",
                                                 "EntityType",
                                                 "Subject",
                                                 "Grade"),
                                          suffixes = c("old","new"))
isd.all.district.pba.delta.grade[,delta:=PercentBelowAveragenew-PercentBelowAverageold]
isd.all.district.pba.delta.grade[,c("SchoolYearold","SchoolYearnew","PercentBelowAverageold","PercentBelowAveragenew"):=NULL]


#--- Plotting Section ---#

# All Subjects
pdf(file = paste0("C:/Users/johns/Desktop/Data_Analyst_Test/ISD Percent Changes in PBA by Grade.pdf"), width = 11, height = 8.5)
for (i in 1:7) {
  grade <- isd.all.district.pba.delta.grade[,unique(Grade)][i]
  table.temp.math <- isd.all.district.pba.delta.grade[Grade==grade & Subject=="Mathematics"]
  table.temp.ela <- isd.all.district.pba.delta.grade[Grade==grade & Subject=="English Language Arts"]
  table.temp.sci <- isd.all.district.pba.delta.grade[Grade==grade & Subject=="Science"]
  print(grade)

  # Math Plot
  plotty.math <-
    ggplot(table.temp.math,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PBA*") +
    labs(title = "Percent Changes in PBA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Mathematics: All Students, ",grade, "th Grade"),
         caption = "*Percent Below Average (PBA); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # ELA Plot
  plotty.ela <-
    ggplot(table.temp.ela,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PBA*") +
    labs(title = "Percent Changes in PBA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("English Language Arts: All Students, ",grade, "th Grade"),
         caption = "*Percent Below Average (PBAG); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  #  Sci Plot
  plotty.sci <-
    ggplot(table.temp.sci,aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_gradient2(low = "red4",
                         mid = "blue",
                         high = "green",
                         midpoint = 0,
                         space = "Lab",
                         name = "% Change: PBA*") +
    labs(title = "Percent Changes in PBA* from 2015/2016 - 2016/2017 ",
         subtitle = paste0("Science: All Students, ",grade, "th Grade"),
         caption = "*Percent Below Average (PBA); Grade 0 = All Students; No data for Grades: 4-8 in Science") +
    xlab("ISD Name") +
    ylab("Percent Change") +
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0)) +
    coord_flip()

  # Plotting Plots
  plot(plotty.math)
  plot(plotty.ela)
  plot(plotty.sci)
}
dev.off()
##### END: PBAG by Grade ####