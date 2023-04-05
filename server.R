#SERVER LOGIC

shinyServer(function(input, output) {
  
  #reactive to call data from ui
  ts <- reactive({ yearly %>%          #reactive for the timestamp in the plot
      filter(year >= input$time_stamp[1],
             year <= input$time_stamp[2]) })
  
  cap <- reactive({ input$capacity })  #reactive for the capacity adjustments in Plots

  
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
  #---------------------output for Plots tab-----------------------------------
  #----------------------------------------------------------------------------

  output$plots <- renderPlotly({

  if(input$scale == 1){  #---frequencies on the standard scale
    
splot <- ggplot(gg, aes(x = mag, y = freqc, group = 1, text = paste("Magnitude ", mag, " (or above)",
                                                              "<br>Frequency: ", 
                                                              "every ", round(1/freqc,2), " years",
                                                              sep = ""))) +
          geom_point(size = 4, shape = 17 ) +
          labs(x = "Magnitude",
              y = "Annual Frequency of At Least this Magnitude",
              title = "Annual Earthquake Frequency near Tohoku, Japan" ) +
              ylim(-5,50) +
              xlim(4.5,9.2) +
 #         theme_minimal() +
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
    
lplot <- ggplot(gg, aes(x = mag, y = freqc, group = 1, text = paste("Magnitude ", mag, " (or above)",
                                                                   "<br>Frequency: ", 
                                                                   "every ", round(1/(freqc),2), " years",
                                                                   sep = ""))) +  
          geom_point(size = 4, shape = 17 ) +
          scale_y_log10(limits = c(.001,100)) +
          scale_x_log10(limits = c(4.5,9.5)) +
          labs(x = "Magnitude",
               y = "Annual Frequency of At Least this Magnitude",
               title = "Annual Earthquake Frequency near Tohoku, Japan - Logarithmic Scale" ) +
  #       theme_minimal() +
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
    
    b <- train(freqc ~ poly(mag,cap()), data = gg_log,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = fitControl)
    
    p <- predict(b, newdata = data.frame(mag = 9.1))
    
    paste("The expected frequency of a magnitude 9.1 earthquake is one every ", round(1/(10^p),4), " years")
    
  })

  #--------------------------------------------------------------------------------------
  #---------------------output for Neural Networks tab-----------------------------------
  #--------------------------------------------------------------------------------------
  

  output$neuralnets <- renderPlotly({
    
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
  
  # output$nets <- renderText({
  # 
  #   # paste("The expected frequency of a magnitude 9.1 earthquake is one every ", round(1/(10^ps),4), " years")
  # 
  # })


#---------------------Begin refresh button code--------------------------------------------
  
  observeEvent(input$refreshnet,{

#-----run BNN, make predictions, and append prediction for 9.1 to the data used in Plotly-----
    bnn <- brnn(y~x,neurons=6)
    ps <- predict(bnn, newdata = data.frame(x = 9.1))
    gg_net <- add_row(gg_net, mag = 9.1, freq = ps, freqc = 10^ps)

#append predicted values to data frame (for better plot visual)
    #for now, try and do without until I perfect the code to plot the line
    # preds <- predict(bnn)
    # preds[32] <- ps
    # gg_net <- cbind(gg_net,preds)

#-----render table of weights and biases-----
      # output$netinfo <- renderTable({
      # 
      #   layer1 <- NULL
      #   layer1_2 <- NULL
      #   layer3 <- NULL
      #   for(i in 1:6){
      #     layer1[i] <-  as.character(bnn$theta[[i]][1])
      #     layer1_2[i] <-  as.character(bnn$theta[[i]][2])
      #     layer3[i] <-  as.character(bnn$theta[[i]][3])
      #   }
      # 
      #   data.frame(neuron = as.character(c(1:6)), w_k = layer1, b_k = layer1_2, beta_k = layer3)
      # 
      # })
      
#-----re-render plot with new data point-----
      output$neuralnets <- renderPlotly({   
        
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

