#### Plots to show top 5 and bottom 5 compared with state level

comparison_plot <- function(plot_data){


   temp.compare <- ggplot(compare_data, aes(x = reorder(IsdName, delta), y = delta, fill = delta)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_gradient2(low = "red4",
                           mid = "steelblue",
                           high = "green",
                           midpoint = 0,
                           space = "Lab",
                           name = "% Change in Mean SGP") +
      labs(title = paste0("Top and Bottom 5 ISD with State of Michigan for: ", compare_data[,unique(Subject)])) +
      xlab("ISD Name") +
      ylab("% Change") +
      geom_text(aes(label = round(delta,2), hjust = ifelse(delta >0,0,1)), size = 4) +
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0),
            axis.text = element_text(size = 13)) +
      coord_flip()

   # plot
   plot(temp.compare)


   }

