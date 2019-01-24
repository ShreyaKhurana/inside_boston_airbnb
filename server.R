library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(ggplot2)
library(leaflet)

function(input, output, session) {
 
  # Scatter plot code
  
  output$scat <- renderPlot({
    
    ggplot(data = mydata)+
      geom_jitter(aes(mydata[[input$scat1]], input$scat2, color = mydata[["neighbourhood_cleansed"]]))+
      labs(x = input$scat1,title = paste("Scatterplot of", input$scat1 , "vs", input$scat2))+
      theme(axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title.y=element_blank(),
            axis.title.x = element_text(size = 20),
            # legend.key = element_blank(),
            
            # axis.title.y = element_text(size = 20),
            plot.title = element_text(size=20, hjust = 0.5),
            panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
            # panel.grid.minor = element_blank(),
            # panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA)
            # strip.text.x = element_text(size = 16, face = 'italic')
      )
  }, bg = 'transparent', height = 700, width = 1000)
  
  # Correlation Plot code
  
  output$plot3 <- renderPlot({
    
    CorrelationResults0 = round(cor(mydata0),2)
    corrplot(CorrelationResults0, tl.srt = 35)
  }, bg = 'transparent', width = 1000, height = 800)
  
  # Boxplots code
  
  output$plot4 <- renderPlot({
    
    ggplot(mapping = aes(x=mydatad[[input$var]], y = mydata$log.Price)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
      # geom_jitter(width = 0.05)+
      labs(x = input$var, y = "Log (price)")+
      theme(axis.text.x = element_text(angle = 45, size = 20),
            axis.text.y = element_text( size = 20),
            axis.title.x = element_text(size = 20),
            # axis.title.y = element_text(size = 20),
            # plot.title = element_text(size=20, hjust = 0.5),
            panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
            # panel.grid.minor = element_blank(),
            # panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA)
            # strip.text.x = element_text(size = 16, face = 'italic')
      )
    
    }, bg = 'transparent', width = 1050, height = 800)
  
  
  # Bar plot of median price
  
  output$showpic <- renderImage({
    filename <- normalizePath(file.path(paste(input$var2, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number"))
  }, deleteFile = FALSE)
  
  # Histogram of price and log price
  
  output$plot7 <- renderPlot({
    mydata<-read.csv("Mydata.csv")
    
    hist(mydata$price, breaks = 1000,xlab="price", main="Histogram for price", border= 'firebrick3')
   
  }, bg='transparent')
  
  output$plot8 <- renderPlot({
    mydata<-read.csv("Mydata.csv")
    mydatad <- mydata[,named]
    mydata$log.Price <- log(mydata$price)
   
    hist(mydata$log.Price, breaks = 1000,xlab="log(price)", main="Histogram for log(price)",border ="cyan4")
  }, bg = 'transparent')
 
  # Sentiment Analysis code
  
  output$plot9 <- renderPlot({
    botby_hood_sentiment <- read.csv('sentimentfinaldata.csv')
    ggplot(data=botby_hood_sentiment) +
      geom_bar(mapping=aes(x=neighbourhood_cleansed,
                           y=prop),
               stat="identity",  fill = "firebrick") +
      facet_wrap( ~ sentiment) +
      labs(title="Proportional Representation of Sentiment Categories in Listing Descriptions",
           x="Neighbourhood", y="Proportion \n (sentiment word count / total word count)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            plot.title = element_text(size=20, hjust = 0.5),
            # panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
            # panel.grid.minor = element_blank(),
            # panel.grid.major = element_blank(),
            # plot.background = element_rect(fill = "transparent",colour = NA),
            strip.text.x = element_text(size = 16, face = 'italic')
      )
    
  }, width = 1200, height = 800)#, bg = "transparent")
    
  #  Explanation sentiment Analysis
  
    output$text1 <- renderText({paste("<font face=\"verdana\" size=4 color=\"#696969\">", "We took a subset of 10 listings and tried to categorize the review comments into various categories as shown on the left using sentiment analysis. Difference can be seen neighbourhood wise as well as sentiment wise. Neighbourhoods like Fenway, Jamaica Plain and Back Bay tend to have higher proportion of positive words- trust, joy and anticipation sentiments are highlighted in these neighbourhoods. In the general bag of words however we see that these sentiments tend to have a higher proportion.",
                                      "</font>")})
    
    
  
  # Wordcloud code
    
  terms <- reactive({
    input$update
    # input$nbr
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        myfunction(input$nbr, input$rating[1], input$rating[2], input$host)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot1 <- renderPlot({
    v <- terms()
    pal2 <- brewer.pal(8,"Dark2")
    outfile <- tempfile(fileext = '.png')
    # png(outfile, width=20,height=15, units='in', res=300, bg = "transparent")
    wordcloud_rep(names(v), v, min.freq = 10, max.words=input$max, random.order=F, rot.per=.3, colors=pal2)
   
  }, bg = "transparent", height = 500, width = 900)
  
  output$plot2 <- renderPlot({
    v2 <- terms()
    df <- data.frame(word=names(v2), freq=v2)
    dffreq <- subset(df, freq>=1000)
    
    ggplot(aes(reorder(word,-freq),freq, fill = freq), data = dffreq)+
      scale_colour_brewer()+
      geom_bar(stat = "identity")+#, position = 'position_dodge')+
      theme(axis.text.x=element_text(angle=45,hjust=1, size=16),
            axis.title.x=element_blank(),
            axis.title.y = element_text(size=16),
            panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
            # panel.grid.minor = element_blank(),
            # panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            legend.position="none"
  )
  }, bg = 'transparent', width = 600, height = 400)

  
  #  Map code. We generate a map and update it everytime
  
  output$mymap <- renderLeaflet({
    # listing <- terms2()
    listing<- read.csv("listings_v1.csv")
    df = data.frame(latitude = listing$latitude,longitude = listing$longitude)
    ##explanation setting
    pop<-paste("ID",listing$id,"<br/>",
               "Name",listing$name,"<br/>",
               "Room Type",listing$room_type,"<br/>",
               "Neighborhood",listing$neighbourhood_cleansed,"<br/>",
               "Price $",listing$price,"<br/>",
               "<a href='",listing$listing_url,"'>See More Information<br/>"
    )


    ##map
    leaflet(df)%>%
      addTiles()%>%
      addProviderTiles("CartoDB.Positron")%>%
      addCircles(data=listing[price<=100,],color="violet",weight=price/60)%>%
      addCircles(data=listing[100<price&price<=200,],color="blue",weight=price/60)%>%
      addCircles(data=listing[200<price&price<=300,],color="green",weight=price/60)%>%
      addCircles(data=listing[300<price&price<=400,],color="yellow",weight=price/60)%>%
      addCircles(data=listing[400<price&price<=500,],color="orange",weight=price/60)%>%
      addCircles(data=listing[500<price,],color="red",weight=price/60)%>%
      addMarkers(clusterOptions = markerClusterOptions(),popup=pop)%>%
      addLegend("bottomright",colors=c("violet", "blue", "green","yellow","orange","red"),labels=c("0~$100","$100~$200","$200~$300","$300~$400", "$400~$500","$500 and above "))

  })
  
  #  Map updation
  
  terms2 <- reactive({
    input$listing_update
    isolate({
      withProgress({
        setProgress(message = "Just a few more seconds...")
        map_data(input$nbr2, input$minPrice, input$maxPrice, input$property,
                 input$room, input$superhost, input$hostverified,
                 input$accomodates, input$bathroom, input$instant,
                 input$cancel, input$internet, input$checkin, input$tv, input$ac,
                 input$family, input$pets, input$parking, input$breakfast)
      })
    })
  })
  
  observeEvent(input$listing_update, {
    
      output$mymap <- renderLeaflet({
        initial_lat = 42.27838291
        initial_lng = -71.1287801
        initial_zoom = 15
        v1 <- terms2()
        # listing<- read.csv("U:/documents/listings_v1.csv")
        if(nrow(terms2())==0) { leaflet(df)%>% addTiles() %>% setView(initial_lng, initial_lat, initial_zoom)}
        else {
          v1 <- terms2()
          df = data.frame(latitude = terms2()$latitude,longitude = terms2()$longitude)
          ##explanation setting
          pop<-paste("ID",v1$id,"<br/>",
                     "Name",v1$name,"<br/>",
                     "Room Type",v1$room_type,"<br/>",
                     "Neighborhood",v1$neighbourhood_cleansed,"<br/>",
                     "Price $",v1$price,"<br/>",
                     "<a href='",v1$listing_url,"'>See More Information<br/>"
          )
          # leafletProxy('mymap', data = terms2())%>%
          leaflet(df)%>%
           
            addTiles()%>%
            addProviderTiles("CartoDB.Positron")%>%
            addCircles(data=v1[price<=100,],color="violet",weight=price/60)%>%
            addCircles(data=v1[100<price&price<=200,],color="blue",weight=price/60)%>%
            addCircles(data=v1[200<price&price<=300,],color="green",weight=price/60)%>%
            addCircles(data=v1[300<price&price<=400,],color="yellow",weight=price/60)%>%
            addCircles(data=v1[400<price&price<=500,],color="orange",weight=price/60)%>%
            addCircles(data=v1[500<price,],color="red",weight=price/60)%>%
            addMarkers(clusterOptions = markerClusterOptions(),popup=pop)%>%
            addLegend("bottomright",colors=c("violet", "blue", "green","yellow","orange","red"),labels=c("0~$100","$100~$200","$200~$300","$300~$400", "$400~$500","$500 and above "))
        }
    
  
  
    })
  })

  
}