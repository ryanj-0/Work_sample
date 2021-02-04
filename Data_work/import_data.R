# Import files for data analysis exercise

# Key
key <- fread(paste0(dir, repo, "/Data/MI Statewide Student Growth (File Layout Key).csv"))

# 2015 - 2016 School Year
year15 <- fread(paste0(dir, repo, "/Data/MI Statewide Student Growth 2015-16.csv"))

# 2016 - 2017 School Year
year16 <- fread(paste0(dir, repo, "/Data/MI Statewide Student Growth 2016-17.csv"))

# School years combined
yearall <- rbind(year15, year16, use.names = T)
