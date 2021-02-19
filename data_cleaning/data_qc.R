##### Data Quality Control Function #####
#----------------------------------------

data_qc <- function(curr_file){

   # Directories needed
   switch.dir <- "switches/"

   # File to be QC checked
   temp.file <- paste0(dir, repo, data.dir, curr_file)

   # Variables to be used
   temp.table <- fread(temp.file)
   mod.date <- substr(file.info(temp.file)$mtime, 1,10)
   rds.name <- paste0(file_path_sans_ext(curr_file), ".rds")
   check.names <- c("SchoolYear",
                    "IsdCode",
                    "IsdName",
                    "DistrictCode",
                    "DistrictName",
                    "BuildingName",
                    "BuildingCode",
                    "EntityType",
                    "Grade",
                    "Subject",
                    "TestingGroup",
                    "NumberAboveAverageGrowth",
                    "NumberAverageGrowth",
                    "NumberBelowAverageGrowth",
                    "PercentAboveAverage",
                    "PercentAverageGrowth",
                    "PercentBelowAverage",
                    "TotalIncluded",
                    "MeanSGP")

   # Check to see if first time loading data, save RDS of data for faster reading in future
   if(!(file.exists(paste0(dir, repo, data.dir, rds.dir, rds.name)))){

      # save data to RDS for future reading in and get/assign data modified
      old.rds <- saveRDS(temp.table, file = paste0(dir, repo, data.dir, rds.dir, rds.name))
      old.rds
      message("File created: ", paste0(dir, repo, data.dir, rds.dir, rds.name))

   }

   # Check if file upload is different than current existing RDS file
   if(any(duplicated(temp.table))){

      switch(menu(choices = c("Yes", "No"),
                  title = "Duplicate values found in data, would you like to view?"),
             "Yes" = View(temp.table[which(duplicated(temp.table))]),
             "No" = exit("Investigate data source and re-run")
      )

   }

   # Check for duplicates in data, returns menu to check table if needed
   if(any(duplicated(temp.table))){

      switch(menu(choices = c("Yes", "No"),
                  title = "Duplicate values found in data, would you like to view?"),
             "Yes" = View(temp.table[which(duplicated(temp.table))]),
             "No" = stop("Investigate data source and re-run")
      )

   }

   # Check for appropriate col names
   assert_colnames(data = temp.table,
                   colnames = check.names,
                   only_colnames = T)

   # Checks for appropriate values
   # NA
   assert_values(data = temp,file,
                 colnames = names(temp.table),
                 test = "not_na")
   # Values in range: Percent Above Average
   assert_values(data = temp.table,
                 colnames = "PercentAboveAverage",
                 test = "in",
                 test_val = c(1:100))
   # Values in range: Percent Average Growth
   assert_values(data = temp.table,
                 colnames = "PercentAverageGrowth",
                 test = "in",
                 test_val = c(1:100))
   # Values in range: Percent Below Average
   assert_values(data = temp.table,
                 colnames = "PercentBelowAverage",
                 test = "in",
                 test_val = c(1:100))
   # Values in range: MeanSGP
   assert_values(data = temp.table,
                 colnames = "MeanSGP",
                 test = "in",
                 test_val = c(1:100))


   # Checks to see if there are missing rds files comapred to what is being loaded in
}