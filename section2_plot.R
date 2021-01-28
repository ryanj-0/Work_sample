dpscd_overall_plot <- function(data){

   # ask users for isd code
   isdcode <- readline(prompt = "Enter the ISD Code of interest: ")
   # selects only isd of interests
   temp.data <- data[IsdCode==isdcode]

   # title for pdf
   isdname <- temp.data[IsdCode==isdcode,unique(IsdName)]
   # open pdf to populate with graphs
   pdf(file = paste0(dir, repo, "/Plots/YoY MeanSGP Change by Subject - ", isdname, ".pdf"), width = 11, height = 8.5)

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
   # close pdf
   graphics.off()
   message("Done plotting, PDF ready for viewing.")
}