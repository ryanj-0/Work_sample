#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### System Initalization #####
source("~/R/Work_sample/work_sample/shiny_app/system_init.R")

# Set global variables
repo <- "/Work_sample/work_sample"



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Detroit Public School Community District (DPSCD)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(


        selectInput(inputId = "school.year",
                    label = "Select School Year",
                    choices =

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
