#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##### Define UI for application that draws a histogram #####
shinyUI(fluidPage(

    # Application title
    titlePanel("Detroit Public School Community District (DPSCD)"),

    # First row of side panel UI
    fluidRow(

        # 1x1
        column(4,
               selectInput(inputId = "grade_id" ,
                           label = h4("Grade"),
                           selected = "All Grades",
                           choices = list("All Students" = 0,
                                          "4th" = 4,
                                          "5th" = 5,
                                          "6th" = 6,
                                          "7th" = 7,
                                          "8th" = 8,
                                          "11th" = 11,
                                          )
                           )
        )
    )
))