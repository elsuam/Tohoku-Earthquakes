#SERVER LOGIC

shinyServer(function(input, output, session) {
  
  #reactive to call data from ui
  ts <- reactive({ yearly %>%          #reactive for the timestamp in the plot
        filter(year >= input$time_stamp[1],
               year <= input$time_stamp[2]) })
  
  cap <- reactive({ input$capacity })  #reactive for the capacity adjustments in Plots
  
  plotscale <- reactive({if(input$scale == 1){eq}    #to make plots on the standard or log scale
                else if(input$scale == 2){eq_log}
   })

  
  #--------------------------------------------------------------------------
  #---------------------output for Map tab-----------------------------------
  #--------------------------------------------------------------------------
  
  output$location <- renderLeaflet({   
    #Plot of the earthquakes
    leaflet() %>% 
      addTiles() %>% 
      setView( lng = 141.6708683
               , lat = 38.0517569
               , zoom = 6) %>% 
      addLegendSize(values = (exp(yearly$mag))^(1/4),
                    stroke = F,
                    fillOpacity = .5,
                    color = "blue",
                    shape = 'circle',
                    position = 'topright',
                    title = "Magnitude")
  })
  
  observe({         
    leafletProxy("location", data = ts()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       radius = ~(exp(mag))^(1/3),
                       stroke = F,
                       fillOpacity = .5,
                       color = ~ifelse(mag > 9, "darkred", "darkblue"),
                       popup = ~as.character(paste0(place, "<br>",
                                                    "<br><strong>Date and Time: </strong>", timestamp,
                                                    "<br><strong>Magnitude: </strong>", mag)
                       )
      )
  })
  
  
  #----------------------------------------------------------------------------
  #---------------------output for Linear Models tab-----------------------------------
  #----------------------------------------------------------------------------

  output$plots <- renderPlotly({
    
    #for now, keep these as is, but if I have success with the other plots, consider
    # reducing these to one and using the plotscale() reactive instead of eq [4/20/23]

  if(input$scale == 1){  #---frequencies on the standard scale
    
splot <- ggplot(eq, aes(x = mag, y = freqc, group = 1, text = paste("Magnitude ", mag, " (or above)",
                                                              "<br>Frequency: ", 
                                                              "every ", round(1/freqc,2), " years",
                                                              sep = ""))) +
          geom_point(size = 4, shape = 17 ) +
          labs(x = "Magnitude",
              y = "Annual Frequency of At Least this Magnitude",
              title = "Annual Earthquake Frequency near Tohoku, Japan" ) +
              ylim(-5,50) +
              xlim(4.5,9.2) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
          tooltip = c("text", "num")
  
      if(input$capacity == 0){  #display plot with no fit line (capacity set to zero)
        
          ggplotly(splot, tooltip = c("text", "num"))
        
      } else{                   #display fit line when capacity set above zero
        
        ggplotly(
          splot +
            stat_smooth(method = lm,
                        formula = y ~ poly(x,cap()),
                        fullrange = T,
                        se = F),
          tooltip = c("text", "num")
          )
        }
  }
    
  else if(input$scale == 2){ #---frequencies on the logistic scale

lplot <- ggplot(eq, aes(x = mag, y = freqc, group = 1, text = paste("Magnitude ", mag, " (or above)",
                                                                   "<br>Frequency: ",
                                                                   "every ", round(1/(freqc),2), " years",
                                                                   sep = ""))) +
          geom_point(size = 4, shape = 17 ) +
          scale_y_log10(limits = c(.001,100)) +
          scale_x_log10(limits = c(4.5,9.5)) +
          labs(x = "Magnitude",
               y = "Annual Frequency of At Least this Magnitude",
               title = "Annual Earthquake Frequency near Tohoku, Japan - Logarithmic Scale" ) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
          tooltip = c("text", "num")

      if(input$capacity == 0){  #display plot with no fit line (capacity set to zero)

          ggplotly(lplot, tooltip = c("text", "num"))

      } else{                   #display fit line when capacity set above zero

        ggplotly(
          lplot +
            stat_smooth(method = lm,
                        formula = y ~ poly(x,cap()),
                        fullrange = T,
                        se = F),
          tooltip = c("text", "num")
          )
        }

  }
    
  })
  

  #---Output for prediction value underneath log plot
  
  output$yeet <- renderText({
    
    b <- lm(data = eq_log, formula = freqc ~ poly(mag,cap()))
    # b <- train(freqc ~ poly(mag,cap()), data = eq_log,
    #            method = "lm",
    #            preProc = c("center", "scale"),
    #            trControl = fitControl)
    
    p <- predict(b, newdata = data.frame(mag = 9.1))
    
    paste("The expected frequency of a magnitude 9.1 earthquake is one every ", round(1/(10^p),4), " years")
    
  })
 
  
  #----------------------------------------------------------------------------
  #---------------------output for Multi-Layer Perceptron tab------------------
  #----------------------------------------------------------------------------
  mlpneurons1 <- reactive({ input$mlpneurons1 }) #first hidden layer size
  mlpneurons2 <- reactive({ input$mlpneurons2 }) #second hidden layer size
  
#--- run model with chosen neurons ---
  mlp <- reactive({
    set.seed(4723)
    neuralnet(freqc ~ mag,
                              stepmax = 1e+06,
                              data = train,
                              hidden = c(mlpneurons1(),mlpneurons2()))    })
  
#--- make predictions based on model ---
  mlp_react <- reactive({
    predicted_log_mlp <- data.frame(mag = mlp_preds,
                                    freqc = predict(mlp(), newdata = data.frame(mag = mlp_preds)),
                                    type = "predicted")
        #combine test and predictions for plot
    mlp_plot <- rbind(predicted_log_mlp,actual_log_mlp[,c(1,3,4)])
  })

  
  output$mlp <- renderPlotly({
    
    ggplot(mlp_react(), aes(x = mag, y = freqc, group = type, color = type)) +
      geom_line() +
      geom_point(size = 2, shape = 17 ) +
      ylim(-3.5,1.5) +
      xlim(4.5,9.2) +
      theme_minimal() +
      labs(x = "Magnitude",
           y = "Annual Frequency of At Least this Magnitude",
           title = "Annual Earthquake Frequency near Tohoku, Japan - Logarithmic Scale",
           subtitle = "Three-Layer Neural Network")
    
  })
  
  testpreds <- reactive({
    predict(mlp(), newdata = data.frame(mag = test$mag))
  })
  
  
  # Table below the plot displaying training error, test error, generalization gap
  output$testerr <- renderTable(
    
    expr = data.frame(TrainError = mlp()[["result.matrix"]][1],
                      TestError = sum((testpreds() - test$freqc)^2),
                      GeneralizationGap = mlp()[["result.matrix"]][1] - sum((testpreds() - test$freqc)^2),
                      Prediction_9.1 = 1/10^predict(mlp(), newdata = data.frame(mag = 9.1)) ),
    digits = 8
    
  )


  
  
  #--------------------------------------------------------------------------------------
  #---------------------output for Bayesian Neural Network tab---------------------------
  #--------------------------------------------------------------------------------------
  

  output$brnn <- renderPlotly({
    
nnplot <- ggplot(gg_net, aes(x = mag, y = freqc, text = paste("Magnitude ", mag, " (or above)",
                                                                    "<br>Frequency: ", 
                                                                    "every ", round(1/(freqc),2), " years",
                                                                    sep = ""))) +
          geom_point(size = 4, shape = 17) + 
          labs(x = "Magnitude",
               y = "Annual Frequency of At Least this Magnitude",
               title = "Annual Earthquake Frequency near Tohoku, Japan" )
    
          ggplotly(nnplot, tooltip = c("text", "num"))
  })
  



#---------------------Begin refresh button code--------------------------------------------
  
  observeEvent(input$refreshnet,{ #commented out to see if this is what causes R to crash

#-----run BNN, make predictions, and append prediction for 9.1 to the data used in Plotly-----
    bnn <- brnn(y~x,neurons=6)
    ps <- predict(bnn, newdata = data.frame(x = 9.1))
    gg_net <- add_row(gg_net, mag = 9.1, freq = ps, freqc = 10^ps)

#append predicted values to data frame (for better plot visual)
#    for now, try and do without until I perfect the code to plot the line
    preds <- predict(bnn)
    preds[32] <- ps
    gg_net <- cbind(gg_net,preds)

# #-----render table of weights and biases-----  #commented out to avoid R chrashing
#       output$netinfo <- renderTable({
# 
#         layer1 <- NULL
#         layer1_2 <- NULL
#         layer3 <- NULL
#         for(i in 1:6){
#           layer1[i] <-  as.character(bnn$theta[[i]][1])
#           layer1_2[i] <-  as.character(bnn$theta[[i]][2])
#           layer3[i] <-  as.character(bnn$theta[[i]][3])
#         }
# 
#         data.frame(neuron = as.character(c(1:6)), w_k = layer1, b_k = layer1_2, beta_k = layer3)
# 
#       })
      
#-----re-render plot with new data point-----
      output$brnn <- renderPlotly({   
        
        nnplot <- ggplot(gg_net, aes(x = mag, y = freqc, text = paste("Magnitude ", mag, " (or above)",
                                                                        "<br>Frequency: ", 
                                                                        "every ", round(1/(freqc),2), " years",
                                                                        sep = ""))) +
          geom_point(size = 4, shape = 17, col = "darkblue") + 
          labs(x = "Magnitude",
               y = "Annual Frequency of At Least this Magnitude",
               title = "Annual Earthquake Frequency near Tohoku, Japan" )
        
        ggplotly(nnplot, tooltip = c("text", "num"))
      })
    
    
# #    (In progress) code for dispaying the fit line from bnn
    #research the package some more (see GitHub and referenced texts) to see if there is a way to add error boundss
#     
#     #this code works outside of shiny
#     ggplot(gg_net, aes(x = mag, y = freqc)) +
#           geom_point(size = 4, shape = 17, col = "darkblue") +
#           geom_line(aes(y = preds))
# 
#     #this is what will be put into Shiny when it is ready
#     if(input$netline == T){
#     ggplot(gg_net, aes(x = mag, y = freqc)) +
#           geom_point(size = 4, shape = 17, col = "darkblue") +
#           geom_line(aes(y = preds))
#     }
  
  })

#---------------------end refresh button code--------------------------------------------


  
})
