## Data work Section ##
hillsdale.mspg <- yearall[IsdCode==30 & Grade==0]
hillsdale.msgp <- hillsdale.all[!Subject=="Social Studies"]
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
hillsdale.msgp <- hillsdale.msgp[order(DistrictCode,TestingGroup,Subject)]
