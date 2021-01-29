## Data work Section ##
hillsdale.msgp <- yearall[IsdCode==30 & Grade==0]
hillsdale.msgp <- hillsdale.msgp[!Subject=="Social Studies"]
hillsdale.msgp <- hillsdale.msgp[!BuildingCode==0]

### Testing Group ###
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
# Need to split data into two different years to find YoY changes
hillsdale.msgp.15 <- hillsdale.msgp[SchoolYear=="15.16"]
hillsdale.msgp.16 <- hillsdale.msgp[SchoolYear=="16.17"]

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
hillsdale.msgp.delta <- hillsdale.msgp.delta[order(DistrictCode, TestingGroup, Subject)]




### Grade ###
hillsdale.msgp.grade <- yearall[IsdCode==30 & TestingGroup=="All Students"]
hillsdale.msgp.grade <- hillsdale.msgp.grade[!Subject=="Social Studies"]
hillsdale.msgp.grade <- hillsdale.msgp.grade[!DistrictCode==0]
hillsdale.msgp.grade[,c("IsdCode",
                        "IsdName",
                        "TestingGroup",
                        "NumberAboveAverageGrowth",
                        "NumberAverageGrowth",
                        "NumberBelowAverageGrowth",
                        "PercentAboveAverage",
                        "PercentAverageGrowth",
                        "PercentBelowAverage",
                        "TotalIncluded"):=NULL] #drop columns
hillsdale.msgp.grade.15 <- hillsdale.msgp.grade[SchoolYear=="15.16"]
hillsdale.msgp.grade.16 <- hillsdale.msgp.grade[SchoolYear=="16.17"]

# Note: Our data set is smaller. i.e. missing records across the years
hillsdale.msgp.grade.delta <- merge(hillsdale.msgp.grade.15,
                                    hillsdale.msgp.grade.16,
                                    by = c("DistrictName",
                                           "DistrictCode",
                                           "BuildingName",
                                           "BuildingCode",
                                           "EntityType",
                                           "Subject",
                                           "Grade"),
                                    suffixes = c("old","new"))
hillsdale.msgp.grade.delta[,delta:=MeanSGPnew-MeanSGPold]
hillsdale.msgp.grade.delta[,c("SchoolYearold","SchoolYearnew","MeanSGPold","MeanSGPnew"):=NULL]
hillsdale.msgp.grade.delta <- hillsdale.msgp.grade.delta[order(DistrictCode, Grade, Subject)]
