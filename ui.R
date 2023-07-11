shinyUI(fluidPage(

    # Application title
    titlePanel("Tohoku Earthquakes 1965 - 2011"),
    

    
sidebarPanel(
  
  
  conditionalPanel(condition = "input.tabs == 'Map'",
                   
    h2("Map"),
                   
    helpText("Use this panel to view a map of the geographic area studied.  Using the slider,
             the largest earthquake of each year is displayed at the location where it occurred."),
                   
  ),
  
  conditionalPanel(condition = "input.tabs == 'Linear Models'",
                   
    h2("Frequency Plots"),
  
    helpText("The relative frequency of each size earthquake is displayed and can be observed on either the
             standard scale or on the logarithmic scale. Use this panel to view features and make
             predictions using linear models. (Note: predictions are made based on a logarithic transformation
             of the data.)"
             ),

              radioButtons("scale", label = h4("Select Scale"),
                          choices = list("Standard" = 1, "Logarithmic" = 2), 
                          selected = 1),
             
              sliderInput("capacity", label = h4("Adjust Capacity"), min = 0, 
                         max = 10, value = 0)
  ),
   
  conditionalPanel(condition = "input.tabs == 'MLP Neural Network'",
                  
    h2("Multi-Layer Perceptron"),
                  
    helpText("This panel features a three-layer Multi-Laper Perceptron (MLP) neural network.  Adjust the
             model capacity by setting the layer sizes using the sliders below."),
                  
#    checkboxInput("nntoggle", label = "Display Predictions", value = F),
    
    sliderInput("mlpneurons1", label = h4("Neurons in the First Hidden Layer"), min = 1, 
                max = 8, value = 1),
    sliderInput("mlpneurons2", label = h4("Neurons in the Second Hidden Layer"), min = 1, 
                max = 8, value = 1),

    # helpText("The table to the right displays the training error, test error, gap between, and the
    #          expected lapsed time between occurrences of a magnitude 9.1 earthquake, based on this model.
    #          Minimizing the generalization gap may be a good choice for this prediction."),

    h4("Neural Network Diagram"),
    plotOutput("mlpplot", height = 250), #delete or undo comment when I figure out placement for this
  
  ),
  
  conditionalPanel(condition = "input.tabs == 'Bayesian Neural Network'",
                   
    h2("Bayesian Neural Network"),
                   
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
                                  leafletOutput("location", height = 600),
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
                                  
                         tabPanel("MLP Neural Network", plotlyOutput("mlp", height = 500, width = "80%"),
                                      h4("Table of Predictive Accuracy Measures"),
                                      tableOutput("testerr"),
                                  ),
                         
                         tabPanel("Bayesian Neural Network", plotlyOutput("brnn", height = 500, width = "80%"),
                                  
                                  # textOutput("nets"),
                                  # h4("Table of network weights and biases"),
                                  # tableOutput("netinfo"),
                         ),
                        # tabPanel("Table", tableOutput("table"))
             ),

)

))
