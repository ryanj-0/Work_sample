
if(setdiff(x,y) > 0 || sediff(y,x) > 0){

   switch(menu(choices = c("See data that is in RDS but not in upload file.",
                           "See data that is in upload file but not in RDS.",
                           "See all mismatched data for the respective tables.",
                           "Do not view data, continue. Waring: May produce erros by continuing."),
               title = "Differences found in both upload file and corresponding RDS, would you like to view them? (Select 1-4)"),
          "See data that is in RDS but not in upload file." =
             warning("Use 'setdiff(x, y)' to see data that is in RDS but not in upload file."),
          "See data that is in upload file but not in RDS." =
             warning("Use 'setdiff(y, x)' to see data that is in upload file but not in RDS."),
          "See all mismatched data for the respective tables." = "Blah Blah",
          "Do not view data, continue. Waring: May produce erros by continuing." = stop("No error, just stopping script.")
   )

}

message("Seeing if this prints after swtich")


x <- data.table(seq(1, 10, 2))
y <- data.table(seq(0, 9, 2))

x <- rbind(x, data.table(seq(1,5,2)))
y <- rbind(y, data.table(seq(0,4,2)))

y <- cbind(y, x)
x <- cbind(x, x)




#-------------------------------------------------------------------------



if(nrow(setdiff(x, y)) > 0){

   message(paste0("Data is different in both",
              "\n\n",
              paste0(dir, repo, data.dir, rds.dir, rds.name),
              " \n",
              paste0(dir, repo, data.dir, curr_file),
              " \n\n",
              "see below for differences.",
              "\n")
   )

   message(paste0("Rows which are different for:",
              "\n",
              rds.name)
   )

   setdiff(x, y)

}


























#-------------------- OLD --------------------------------------------
tester <- function(first, second){

   if(nrow(setdiff(first, second)) > 0 & nrow(setdiff(second, first)) > 0 ){

      cat(paste0("Data is different in both",
                     "\n\n",
                     paste0(dir, repo, data.dir, rds.dir, rds.name),
                     " \n",
                     paste0(dir, repo, data.dir, curr_file),
                     " \n\n",
                     "see below for differences.",
                     "\n")
      )

   cat(paste0("Rows which are different for:",
                  "\n",
                  rds.name)
   )

   setdiff(first, second)

   cat("\n I'm here \n")

   # message(paste0("Rows which are different for:",
   #                "\n",
   #                curr_file)
   # )
   # setdiff(second, first)

   }
}
