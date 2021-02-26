##### Switch Output for Checing Duplicates: s1o1 #####

table.with.dups <- temp.table[which(duplicated(temp.table))]

View(table.with.dups)

warning("Stopping script, see table for duplicates.")