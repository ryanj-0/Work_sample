x <- data.table(seq(1, 10, 2))
y <- data.table(seq(0, 9, 2))

x <- rbind(x, data.table(seq(1,5,2)))
y <- rbind(y, data.table(seq(0,4,2)))

y <- cbind(y, x)
x <- cbind(x, x)




#-------------------------------------------------------------------------


if(nrow(setdiff(x, y)) > 0){

   rds.diff <- setdiff(x, y)
   rds.count <- 1

}

if(nrow(setdiff(y, x)) > 0){

   upload.diff <- setdiff(y, x)
   upload.count <- 1

}

if(rds.count > 0 & upload.count > 0){

   switch(menu(choices = c("Yes", "No"),
               title = "Found data which does not match in both existing RDS and it's upload file, would you like to view the data?"),
          "Yes" = source(paste0(dir, repo, switch.dir, "s2o1.R")),
          "No" = source(paste0(dir, repo, switch.dir, "s2o2.R"))
   )

}