###### Import files for data analysis exercise #####
#---------------------------------------------------

# List of data to import
files <- c("MI Statewide Student Growth (File Layout Key).csv",
           "MI Statewide Student Growth 2015-16.csv",
           "MI Statewide Student Growth 2016-17.csv")

# Data Dir
data.dir <- "Data/"

# Key
key <- fread(paste0(dir, repo, data.dir, files[1]))

# 2015 - 2016 School Year
year15 <- fread(paste0(dir, repo, data.dir, files[2]))

# 2016 - 2017 School Year
year16 <- fread(paste0(dir, repo, data.dir, files[3]))

# School years combined
yearall <- rbind(year15, year16, use.names = T)


# Check for duplicates in data, returns menu to check table if needed.
if(any(duplicated(yearall))==TRUE){

   switch(menu(choices = c("Yes", "No"),
               title = "Duplicate values found in data, would you like to view? - enter number"),
          "Yes" = View(yearall[which(duplicated(yearall))]),
          "No" = stop("Investigate data source and re-run")
   )

}

# Check to see if first time loading data, if so load it, save RDS
if(file.exists(paste0(dir, repo, data.dir, "dpscd_data.rds"))){

   # save data to RDS for future reading in and get/assign data modified
   old.rds <- saveRDS(yearall, file = paste0(dir, repo, data.dir, "dpscd_data.rds"))
   rds.length <- length(old.rds)

   old.rds
   saveRDS(rds.length, file = paste0(dir, repo, data.dir, "rds_length.rds"))

} else{

   old.rds <- fread(paste0(dir, repo, data.dir, "dpscd_data.rds"))

   rds.length.new <- length(yearall)

   if(rds.length.new>rds.length){

      new.rds <- old.rds
      saveRDS(new.rds, file = paste0(dir, repo, data.dir, "rds_length.rds"))

   }

}



