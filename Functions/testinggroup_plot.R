# Functions plot YoY change in Mean SGP by testing group

testinggroup_plot <- function(plot_data, testgroupname, subjectname){

   temp.plot <- ggplot(plot_data, aes(x = reorder(get(col.interest), delta),
                                             y = delta,
                                             fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue3",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "%-Chg: Mean SGP*") +
      xlab(xlabel) +
      ylab("% Change") +
      labs(title = ("% Change in Mean SGP*: 2015/2016 - 2016/2017 "),
           subtitle = paste0(subjectname,": ",testgroupname, "; All Grades"),
           caption = "*Mean Student Growth Percentile") +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      coord_flip()

   #plot
   plot(temp.plot)
}