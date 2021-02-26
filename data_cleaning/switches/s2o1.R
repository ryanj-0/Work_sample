###### Switch to show inconsistent data between RDS and upload: s2o1 #####

# Set appropriate names for tables
"In RDS, not in Upload" <- rds.diff
"In Upload, not in RDS" <- upload.diff

View(`In RDS, not in Upload`)
View(`In Upload, not in RDS`)

switch(menu(choices = c("Yes", "No"),
            title = "Review data. Would you like to stop the script"),
       "Yes" = message("Very well, continuing script"),
       "No" = warning("Stopping script.")
)
