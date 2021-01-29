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
