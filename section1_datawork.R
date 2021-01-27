###################################
##### Data work for Section 1 #####
###################################

# Looking for Testing Group Disparities in ISD by Testing Groups and Grades
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
isd.msgp.delta <- isd.msgp.delta[order(IsdCode,TestingGroup,Subject)] # reorder table for plotting




##### YoY MeanSGP change by Grade #####
# Data for Science is only for Grades 0,11
isd.msgp.grade <- yearall[DistrictCode==0 & TestingGroup=="All Students"]
isd.msgp.grade[,c("DistrictCode",
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
                  "TotalIncluded"):=NULL] #drop columns; interested in only MeanSGP by Grade
# Need to split data into two different years to find YoY changes
isd.msgp.15.grade <- isd.msgp.grade[SchoolYear=="15.16"]
isd.msgp.16.grade <- isd.msgp.grade[SchoolYear=="16.17"]

# Note: Our data set is smaller. i.e. missing records across the years
isd.msgp.grade.delta <- merge(isd.msgp.15.grade,
                              isd.msgp.16.grade,
                              by = c("IsdCode",
                                     "IsdName",
                                     "EntityType",
                                     "Subject",
                                     "Grade"),
                              suffixes = c("old","new"))
isd.msgp.grade.delta[,delta:=MeanSGPnew-MeanSGPold]
isd.msgp.grade.delta[,c("SchoolYearold","SchoolYearnew","MeanSGPold","MeanSGPnew"):=NULL] # drop old columns
isd.msgp.grade.delta <- isd.msgp.grade.delta[order(IsdCode,Grade,Subject)] # reorder table