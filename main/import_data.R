###### Import files for data analysis exercise #####
#---------------------------------------------------

# Data directory
data.dir <- "Data/raw/"
rds.dir <- "working/"

# List of data to import
files <- c("MI Statewide Student Growth 2015-16.csv",
           "MI Statewide Student Growth 2016-17.csv")



# Data Quality Check


# 2015 - 2016 School Year
year15 <- fread(paste0(dir, repo, data.dir, files[1]))

# 2016 - 2017 School Year
year16 <- fread(paste0(dir, repo, data.dir, files[2]))

# School years combined
yearall <- rbind(year15, year16, use.names = T)

#--- Assuming data will always be added, takes new data and appends it to default RDS and adds new archive data

# Check to see if first time loading data, save RDS of data for faster reading in future
if(!(file.exists(paste0(dir, repo, data.dir, "dpscd_data.rds")))){

   # save data to RDS for future reading in and get/assign data modified
   old.rds <- saveRDS(yearall, file = paste0(dir, repo, data.dir, "dpscd_data.rds"))
   old.rds

} else{

   # Create data tables to check for differences
   old.rds <- fread(paste0(dir, repo, data.dir, "dpscd_data.rds"))
   new.rds <- yearall

   # Check for old and new table differences, asks user if they want to add/remove data
   if(uniqueN(setdiff(x,y))>0){

      setdiff(old.rds, new.rds)
      switch(menu(choices = c("Yes", "No"),
                  title = "Old data has records that new data does not.Do you want to replace the old data with the new, changing records with the above?"),
             "Yes" = old.rds <- new.rds,
             "No" = stop("Stopping script to evaluate data.")
      )

      saveRDS(new.rds, file = paste0(dir, repo, data.dir, "dpscd_data.rds"))
      message("Updated dpscd_data.rds file.")

   } else if(uniqueN(setdiff(new.rds,old.rds))>0){

      setdiff(new.rds, old.rds)
      switch(menu(choices = c("Yes", "No"),
                  title = "New data has records that old data does not.Do you want to replace the old data with the new, changing records with the above?"),
             "Yes" = old.rds <- new.rds,
             "No" = stop("Stopping script to evaluate data.")
      )

      saveRDS(new.rds, file = paste0(dir, repo, data.dir, "dpscd_data.rds"))
      message("Updated dpscd_data.rds file.")

   }

}



