#SERVER LOGIC

shinyServer(function(input, output) {
  
  #reactive to call data from ui
  ts <- reactive({ yearly %>%
      filter(year >= input$time_stamp[1],
             year <= input$time_stamp[2]) })
  
  cap <- reactive({ input$capacity })

  
  #--Output for prediction value underneath plots
  output$yeet <- renderText({
    
    gg2 <- gg
    
    gg2$freqc <- log10(gg$freqc) #transforming to the log scale
    
    fitControl <- trainControl(method = "cv", number = 10)
    
    b <- train(freqc ~ poly(mag,cap()), data = gg2,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = fitControl)
    
    p <- predict(b, newdata = data.frame(mag = 9.1))
    
    paste("The expected frequency of a magnitude 9.1 earthquake is one every ", 1/(10^p), " years")
    
  })

  
  
  #-------output for Map tab-------
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
  
  #-------output for plots tab-------

  output$plots <- renderPlotly({

  if(input$scale == 1){
    
splot <- ggplot(gg, aes(x = mag, y = freqc)) +   #---frequencies on the standard scale
         geom_point(size = 4, shape = 17 ) +
         labs(x = "Magnitude",
              y = "Annual Frequency of At Least this Magnitude",
              title = "Annual Earthquake Frequency near Tohoku, Japan" ) +
              ylim(-5,50) +
              xlim(4.5,9.2)       
  
      if(input$capacity == 0){
        splot
      } else{
        splot +
          stat_smooth(method = lm,
                      formula = y ~ poly(x,cap()),
                      fullrange = T,
                      se = F)
        }
  }
    
  else if(input$scale == 2){
    
lplot <- ggplot(gg, aes(x = mag, y = freqc)) +   #---frequencies on the logistic scale
         geom_point(size = 4, shape = 17 ) +
         scale_y_log10(limits = c(.001,100)) +
         scale_x_log10(limits = c(4.5,9.5)) +
         labs(x = "Magnitude",
              y = "Annual Frequency of At Least this Magnitude",
              title = "Annual Earthquake Frequency near Tohoku, Japan - Logarithmic Scale" ) 

      if(input$capacity == 0){
        lplot
      } else{
        lplot +
          stat_smooth(method = lm,
                      formula = y ~ poly(x,cap()),
                      fullrange = T,
                      se = F)
      }

  }
    
  })

  


  
})




1/(10^(-2.54))


