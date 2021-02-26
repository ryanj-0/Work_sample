##### Switch Output for Checking Duplicates: s2o2 #####
switch(menu(choices = c("Yes", "No"),
            title = "Contiuning script may cause errors, would you still like to contiune?"),
       "Yes" = message("Continuing script, watch for possible errors."),
       "No" = warning("Stopping script.")
)