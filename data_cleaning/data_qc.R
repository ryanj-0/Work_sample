##### Data Quality Control Function #####
#----------------------------------------

data_qc <- function(curr_file){

   # Directories needed
   switch.dir <- "data_cleaning/switches/"

   # File to be QC checked
   temp.upload <- paste0(dir, repo, data.dir, curr_file)

   # Variables to be used
   temp.table <- fread(temp.upload)
   mod.date <- substr(file.info(temp.upload)$mtime, 1,10)
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

   # Check to see if first time loading data, if true, save RDS of data for faster reading in future
   if(!(file.exists(paste0(dir, repo, data.dir, rds.dir, rds.name)))){

      # save data to RDS for future reading in and get/assign data modified
      temp.rds <- saveRDS(temp.table, file = paste0(dir, repo, data.dir, rds.dir, rds.name))
      temp.rds
      message("File created: ", paste0(dir, repo, data.dir, rds.dir, rds.name))

   }

   # Check if upload file has any duplicates (s1..)
   if(any(duplicated(temp.table))){

      switch(menu(choices = c("Yes", "No"),
                  title = "Duplicate values found in data, would you like to view?"),
             "Yes" = source(paste0(dir, repo, switch.dir, "s1o1.R")),
             "No" = source(paste0(dir, repo, switch.dir, "s1o2.R"))
      )

   }

   # Check if RDS and Upload File have differing data
   if(nrow(setdiff(temp.rds, temp.upload)) > 0){

      rds.diff <- setdiff(temp.rds, temp.upload)
      rds.count <- 1

   }

   if(nrow(setdiff(temp.upload, temp.rds)) > 0){

      upload.diff <- setdiff(temp.upload, temp.rds)
      upload.count <- 1

   }

   if(rds.count > 0 & upload.count > 0){

      # s2
      switch(menu(choices = c("Yes", "No"),
                  title = "Found data which does not match in both existing RDS and it's upload file, would you like to view the data?"),
             "Yes" = source(paste0(dir, repo, switch.dir, "s2o1.R")),
             "No" = source(paste0(dir, repo, switch.dir, "continue_check.R"))
      )

   }else if(rds.count > 0){

      # s3
      switch(menu(choices = c("Yes", "No"),
                  title = "Found data in RDS that in not in Uploadfile, would you like to view the data?"),
             "Yes" = source(paste0(dir, repo, switch.dir, "s3o1.R")),
             "No" = source(paste0(dir, repo, switch.dir, "continue_check.R"))
      )

   }else if(upload.count > 0){

      # s4
      switch(menu(choices = c("Yes", "No"),
                  title = "Found data in Upload File that in not in RDS, would you like to view the data?"),
             "Yes" = source(paste0(dir, repo, switch.dir, "s4o1.R")),
             "No" = source(paste0(dir, repo, switch.dir, "continue_check.R"))
      )

   }


   # Check for appropriate col names
   assert_colnames(data = temp.table,
                   colnames = check.names,
                   only_colnames = T)

   # Checks for appropriate values
   # NA
   assert_values(data = temp.table,
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