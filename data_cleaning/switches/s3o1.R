###### Switch to show inconsistent data between RDS and upload: s3o1 #####

# Set appropriate names for tables
"In RDS, not in Upload" <- rds.diff

View(`In RDS, not in Upload`)

message("Since data is in RDS and not in Upload File, implication is that data was removed.")

switch(menu(choices = c("Yes", "No"),
            title = "Would you like to override the Upload file with the RDS data and contiune script?"),
       "Yes" = temp.rds <- temp.upload,
       "No" = warning("Stopping script.")
)

message("Data commited, continuing data quality check.")