shinyUI(fluidPage(

    # Application title
    titlePanel("Tohoku Earthquakes 1965 - 2011"),
    

    
sidebarPanel(
  
  
  conditionalPanel(condition = "input.tabs == 'Map'",
                   
    h2("Map"),
                   
    helpText("Use this panel to view a map of the geographic area studied."),
                   
  ),
  
  conditionalPanel(condition = "input.tabs == 'Linear Models'",
                   
    h2("Frequency Plots"),
  
    helpText("Use this panel to view features and make predictions."),

              radioButtons("scale", label = h4("Select Scale"),
                          choices = list("Standard" = 1, "Logarithmic" = 2), 
                          selected = 1),
             
              sliderInput("capacity", label = h4("Adjust Capacity"), min = 0, 
                         max = 10, value = 0)
  ),
  
  conditionalPanel(condition = "input.tabs == 'Neural Network'",
                   
    h2("Neural Network"),
                   
    helpText("Use this panel to view predections made using a Bayesian neural network
             Click the button to engage and re-engage the network to view differences predicted under Bayes."),

    actionButton("refreshnet", label = "Engage Network"),
    
 #   checkboxInput("netline", label = "Display Regularization"),

  )
),
      
mainPanel(
             tabsetPanel(type = "tabs", id = "tabs",
                         tabPanel("Map",
                                  h4("Largest Earthquakes by Year 1965 - 2011"),
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
                         
                         tabPanel("Linear Models", plotlyOutput("plots", height = 500, width = "80%"),
                                  
                                    conditionalPanel(condition = "input.scale == 2",
                                                    h4("Adjust capacity to view predictions:"),
                                                    textOutput("yeet"),
                                      #              checkboxInput("predictions", label = "Show Prediction Point", value = F)
                                    ),
                                  ),
                                  
                         tabPanel("Neural Network", plotlyOutput("neuralnets", height = 500, width = "80%"),

                                                    # textOutput("nets"),
                                                    h4("Table of network weights and biases"),
                                                    tableOutput("netinfo"),
                                  ),
                        # tabPanel("Table", tableOutput("table"))
             ),

)

))
