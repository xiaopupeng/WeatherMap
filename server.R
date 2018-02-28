library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(darksky)
library(ggplot2)
library(purrr)

suffix = ".rda"

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })

  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=4000, layerId=~zipcode,
        stroke=FALSE, fillOpacity=0.4, fillColor="#03F")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      #tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Current Weather: %s", now["currently"]$currently$icon), tags$br(),
      sprintf("Current Tempreture: %s F", as.integer(now["currently"]$currently$temperature)), tags$br(),
      sprintf("Current Wind Speed: %s mph", now["currently"]$currently$windSpeed)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, anywhere
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_click
    if (is.null(event))
      return()
    dlng <- event$lng - cleantable$Long
    dlat <- event$lat - cleantable$Lat
    dd = (dlng ^ 2 + dlat ^ 2)^0.5
    index <- which.min(dd)
    nid <- cleantable[index,]$"Zipcode"
    nlat <- cleantable[index,]$"Lat"
    nlng <- cleantable[index,]$"Long"
    print("get the nid")
    print(nid)
    isolate({
      showZipcodePopup(nid, nlat, nlng)
    })
    fname <- paste(nid,suffix,sep="")
    if(file.exists(fname)){
      # 24 hours expired
      if((Sys.time() - file.info(fname)$ctime) < 1440){
        load(fname)
        now <<- now
        print("loading file:")
      }else{
        now <<- get_current_forecast(nlat, nlng)
        save(now,file=fname)
        print("file expired:")
      }
    }else{
      now <<- get_current_forecast(nlat, nlng)
      save(now,file=fname)
      print("no caching file, download:")
    }
  })


  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    # cwang
    dlng <- event$lng - cleantable$Long
    dlat <- event$lat - cleantable$Lat
    dd = (dlng ^ 2 + dlat ^ 2)^0.5
    index <- which.min(dd)
    nid <- cleantable[index,]$"Zipcode"
    nlat <- cleantable[index,]$"Lat"
    nlng <- cleantable[index,]$"Long"
    print("get closely nid")
    print(nid)
    
    
    fname <- paste(nid,suffix,sep="")
    if(file.exists(fname)){
      print("file exists")
      # 24 hours expired
      if((Sys.time() - file.info(fname)$ctime) < 1440){
        load(fname)
        now <<- now
        print("loading file:")
      }else{
        now <<- get_current_forecast(nlat, nlng)
        save(now,file=fname)
        print("file expired:")
      }
    }else{
      print("file is not exist")
      now <<- get_current_forecast(nlat, nlng)
      save(now,file=fname)
      print("no caching file, download:")
    }
    
    isolate({
      showZipcodePopup(nid, nlat, nlng)
    })
    
    
  })
  
  
  # test query specific date
  # if (is.null(input$date))
  #   return()
  # else{
  #   print("date is")
  #   print(input$date)
  #   # specific_tmp = get_forecast_for(43.2672, -70.8617,)
  # }
  
  

  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
    # get the zipcodes
    print(input$zipcodes)
    if (is.null(input$zipcodes))
      return()
    else{
      print(input$zipcodes)
      tmp <- cleantable[cleantable$Zipcode == input$zipcodes,]
      print("else ")
      print(tmp)
      x <- tmp$Lat
      y <- tmp$Long
      print("we have Lat from zipcode")
      print(x)
      print(y)
      
      check_location <<- get_current_forecast(x,y)
      check_location_icon <<- check_location["currently"]$currently$icon
      # print(check_location_icon)
      
      srcstr <<- ""
      if (check_location_icon == "clear-day") {
          srcstr = "./images/1.png"
      }
      else if (check_location_icon == "clear-night") {
        srcstr = "./images/2.png"
      }
      else if (check_location_icon == "rain") {
          srcstr = "./images/3.png"
      }
      else if (check_location_icon == "snow") {
          srcstr = "./images/4.png"
          }
      else if (check_location_icon == "sleet") {
          srcstr = "./images/5.png"
          }
      else if (check_location_icon == "wind") {
          srcstr = "./images/6.png"
          }
      else if (check_location_icon == "fog") {
          srcstr = "./images/7.png"
          }
      else if (check_location_icon == "cloudy") {
          srcstr = "./images/89.png"
          }
      else if (check_location_icon == "partly-cloudy-day") {
          srcstr = "./images/89.png"
          }
      #check_location_icon == "partly-cloudy-night"
      else{
          srcstr = "./images/10.png"
      }
      output$weatherIcon <- renderImage({
        print(srcstr)
        return(list(
          src = srcstr,
          filetype = "image/png",
          alt = "This is a raining"
        ))
      }, deleteFile = FALSE)
      
      # replot the figure
      output$Hourplot <-renderPlot({
        
        # now <- get_current_forecast(37.8267,-122.4233)
        
        now = check_location
        print(now)
        # output$plotwether = weather_plot(now)
        hourlytmp <- now["hourly"]
        
        userfuldata <- hourlytmp$hourly
        print(userfuldata)
        print("get time")
        print(userfuldata["time"][,c(1)])
        # hist(rnorm(100))
        
        # x <- as.POSIXct(as.Date(c(userfuldata["time"][,c(1)]),"%Y-%m-%d %H:%M:%S", tz="Europe/London"))
        # print("x is")
        # print(range(x))
        axis(1, at=1:7)
        
        print(plot(temperature ~ time, data = userfuldata, ylim = range(userfuldata["temperature"]), type="b"))
      })
      
      output$MinPlot <-renderPlot({
        
        # now <- get_current_forecast(37.8267,-122.4233)
        now = check_location
        print(now)
        # output$plotwether = weather_plot(now)
        hourlytmp <- now["minutely"]
        
        userfuldata <- hourlytmp$minutely
        print(userfuldata)
        print("get time")
        print(userfuldata["time"][,c(1)])
        # hist(rnorm(100))
        
        # x <- as.POSIXct(as.Date(c(userfuldata["time"][,c(1)]),"%Y-%m-%d %H:%M:%S", tz="Europe/London"))
        # print("x is")
        # print(range(x))
        axis(1, at=1:7)
        
        print(plot(precipIntensity ~ time, data = userfuldata, ylim = range(userfuldata["precipIntensity"]), type="b"))
      })
      
      output$Dailyplot <-renderPlot({
        
        # now <- get_current_forecast(37.8267,-122.4233)
        now = check_location
        print(now)
        # output$plotwether = weather_plot(now)
        hourlytmp <- now["daily"]
        
        userfuldata <- hourlytmp$daily
        print(userfuldata)
        print("get time")
        print(userfuldata["time"][,c(1)])
        # hist(rnorm(100))
        
        # x <- as.POSIXct(as.Date(c(userfuldata["time"][,c(1)]),"%Y-%m-%d %H:%M:%S", tz="Europe/London"))
        # print("x is")
        # print(range(x))
        axis(1, at=1:7)
        
        print(plot(temperatureHigh ~ time, data = userfuldata, ylim = range(userfuldata["temperatureHigh"]), type="b"))
      })
      
      
      
      
      # update the tmp
      current_tmp = check_location["currently"]$currently$temperature
      output$test1 <- renderText({ 
        paste(c(current_tmp, "F"), collapse = "")
      })
      
      
      
      # check history data of above city
      output$hisHourplot <-renderPlot({
        if (is.null(input$date))
          return()
        else{
          print("date is")
          print(input$date)
          specific_tmp = get_forecast_for(x, y,input$date)
          print("specific tmp")
          histo_tmp = specific_tmp
          # print(now)
          # output$plotwether = weather_plot(now)
          hourlytmp <- histo_tmp["hourly"]
          
          userfuldata <- hourlytmp$hourly
          print(userfuldata)
          print("get time")
          print(userfuldata["time"][,c(1)])
          # hist(rnorm(100))
          
          # x <- as.POSIXct(as.Date(c(userfuldata["time"][,c(1)]),"%Y-%m-%d %H:%M:%S", tz="Europe/London"))
          # print("x is")
          # print(range(x))
          axis(1, at=1:7)
          
          print(plot(temperature ~ time, data = userfuldata, ylim = range(userfuldata["temperature"]), type="b"))
        }
      })
    }
    })
      
        
  

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      print("map")
      print(lat)
      print(lng)
      now <- get_current_forecast(lat, lng)
      print(now)
      # output$plotwether = weather_plot(now)
      hourlytmp <- now["daily"]
      
      userfuldata <- hourlytmp$daily
      print(userfuldata)
      
      # print("time is ")
      # print(hourlytmp["time"])
      
      # my_xlim <- userfuldata$time(as.POSIXct('2007-08-24 17:30:00', format="%Y-%m-%d %H:%M:%S"))
      
      
      # output$plotwether <- renderPlot({
      #   if (nrow(zipsInBounds()) == 0)
      #     return(NULL)
      #   print("-------------")
      #   # print(zipsInBounds())
      #   # my_xlim <- userfuldata$time(as.POSIXct('2007-08-24 17:30:00', format="%Y-%m-%d %H:%M:%S"))
      #   # x <- as.POSIXct(as.Date(c("20/01/2001","20/02/2003","21/06/2004"),"%d/%m/%Y"))
      #   # y <- c(1,2,3)
      #   # xyplot(y~x)
      #   
      #   print(plot(temperatureHigh ~ time, data = userfuldata, ylim = range(userfuldata["temperatureHigh"]), type="b"))
      #   # print(xyplot(temperatureHigh ~ time, data = userfuldata, ylim = range(userfuldata["temperatureHigh"])))
      #   })
      
      # has <- intersect(readings, names(x))
      # rows <- sapply(has, function(y) nrow(x[[y]]))
      # has <- names(which(rows>1))
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
    
  })
  

  
  
  
  
  
  
  
  
  
  # output$summary <-renderText({
  #   print("hello")
  # })
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    
    # print(input$zipcodes)
    # if(input$zipcodes == ""){
    #   print("wait")
    # }else{
    #   print("if we alraedy have zipcode")
    #   print(input$zipcodes)
    # }
  })
}
