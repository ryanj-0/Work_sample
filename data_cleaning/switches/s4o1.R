###### Switch to show inconsistent data between RDS and upload: s2o1 #####

# Set appropriate names for tables
"In Upload, not in RDS" <- upload.diff

View(`In Upload, not in RDS`)

message("Since data is in Upload File and not in RDS, implication is new data has been added.")

switch(menu(choices = c("Yes", "No"),
            title = "Would you like to update RDS with the new data and contiune script?"),
       "Yes" = temp.upload <- temp.rds,
       "No" = warning("Stopping script.")
)

message("Data commited, continuing data quality check.")