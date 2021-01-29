##### Top/Bottom 5 Changes in MeanSGP by ISD (All Students, All Grades) #####
isd.msgp.delta.topbottom <- isd.msgp.delta[TestingGroup=="All Students"] # Get top/bottom 5 ISD, and state (All Students)
isd.msgp.delta.topbottom <- isd.msgp.delta.topbottom[order(-delta)] #order table by percent change largest to smallest


isd.msgp.top5.math <- head(isd.msgp.delta.topbottom[Subject=="Mathematics"],5) # return top 5 delta
isd.msgp.top5.ela <- head(isd.msgp.delta.topbottom[Subject=="English Language Arts"],5) # return top 5 delta
isd.msgp.top5.sci <- head(isd.msgp.delta.topbottom[Subject=="Science"],5) # return top 5 delta
isd.msgp.bottom5.math <- tail(isd.msgp.delta.topbottom[Subject=="Mathematics"],5) # return bottom 5 delta
isd.msgp.bottom5.ela <- tail(isd.msgp.delta.topbottom[Subject=="English Language Arts"],5) # return bottom 5 delta
isd.msgp.bottom5.sci <- tail(isd.msgp.delta.topbottom[Subject=="Science"],5) # return bottom 5 delta

isd.msgp.compare.math <- rbind(isd.msgp.top5.math,isd.msgp.bottom5.math) # Combine top & bottom 5 isds; math
isd.msgp.compare.math <- rbind(isd.msgp.compare.math,
                               isd.msgp.delta.topbottom[IsdCode==0 & Subject=="Mathematics"]) # add statewide level

isd.msgp.compare.ela <- rbind(isd.msgp.top5.ela,isd.msgp.bottom5.ela) # combine top & bottom isds; ELA
isd.msgp.compare.ela <- rbind(isd.msgp.compare.ela,
                              isd.msgp.delta.topbottom[IsdCode==0 & Subject=="English Language Arts"]) # add statewide level

isd.msgp.compare.sci <- rbind(isd.msgp.top5.sci,isd.msgp.bottom5.sci) # combine top & bottom isds; science
isd.msgp.compare.sci <- rbind(isd.msgp.compare.sci,
                              isd.msgp.delta.topbottom[IsdCode==0 & Subject=="Science"]) # add statewide level

