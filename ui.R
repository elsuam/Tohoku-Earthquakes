shinyUI(fluidPage(

    # Application title
    titlePanel("Tohoku Earthquakes 1965 - 2011"),
    
sidebarPanel(
  
  h2("Frequency Plots"),
  
  helpText("Use this panel to view features and make predictions in the Plots tab."),

             radioButtons("scale", label = h4("Select Scale"),
                          choices = list("Standard" = 1, "Logarithmic" = 2), 
                          selected = 1),
             
             sliderInput("capacity", label = h4("Adjust Capacity"), min = 0, 
                         max = 10, value = 0)
),
      
mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Map",
                                  h4("Largest Earthquakes by Year 1965-2011"),
                                  leafletOutput("location", height = 500),
                                    absolutePanel(bottom = 10, right = 40,
                                      sliderInput("time_stamp",
                                              label = "Year Range",
                                              min = date_start, 
                                              max = date_end,
                                              value = c(date_start,yearly$year[10]),
                                              #width = '100%',
                                              timezone = "GMT",
                                              step = 1,
                                              round = F,
                                              sep = "")
                                    )
                                  ),
                         tabPanel("Plots", plotlyOutput("plots", height = 500, width = "80%"),
                                  
                                    conditionalPanel(condition = "input.scale == 2",
                                                    h4("Adjust capacity on the log scale to view predictions:"),
                                                    textOutput("yeet"),
                                      #              checkboxInput("predictions", label = "Show Prediction Point", value = F)
                                    )
                                  
                                  ),
                        # tabPanel("Table", tableOutput("table"))
             ),

)

))
