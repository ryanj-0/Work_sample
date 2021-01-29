dpscd_overall_plot <- function(data, isdcode){

   # selects only isd of interests
   temp.data <- data[IsdCode==isdcode]

   overall <- ggplot(temp.data, aes(x = SchoolYear, y = MeanSGP, color = Subject)) +
      geom_point() +
      geom_line(aes(group = Subject)) +
      labs(title = paste0(temp.data[,unique(IsdName)], ": All Districts"),
           subtitle = "YoY MeanSGP* Change by Subject",
           caption = "*Mean Student Growth Percentile (Mean SGP)") +
      xlab("School Year") +
      ylab(names(temp.data)[19]) +
      scale_x_discrete(labels = c("2015 - 2016", "2016 - 2017")) +
      theme(plot.caption = element_text(hjust = 0))

   # plot
   plot(overall)
}