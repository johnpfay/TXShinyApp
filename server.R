
#This is the server side of the Shiny app- so when a user clicks
    #on a tab, or selects a button, this script says how it reacts

#Sections:
  #2. Create the server file
        #2a. Prep the data (TXIndus) so that it is filtered by the date selected by user
              #I have marked this out for now. It just reads in the file w/o filtering by date
      
        #Start writing script for each tab
        #2b. Make a Graph of the data for the Historical Trends tab
        #2c. Make map of Texas by water usage
        #2d.Make Table of largest Water users
        #2e. Write Credits for the Credits tab

#2. Server Script ------
  shinyServer(function(input,output) { #server is defined within these parentheses
    
    #2a. We set the data source and make it change with the date! ------ prep data once and then pass around the program
    passData <- reactive({
      TXIndus <- subset(TXIndus, DateProper > ymd(input$dateRange[1]) & DateProper < ymd(input$dateRange[2]))
    })


#2b.For the Historical Trends Tab: make a graph of the data----  
  output$HistoricalTrends <- renderPlot({
    
    #Get the data ready to be plotted!!
      #graphData <- ddply(passData(), .(Domain, Date), numcolwise(sum))
      graphdata<-passData() %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
      #Groundwater
      graphGWdata<-passData()[TXIndus$`Water Type` == "Ground Water",]%>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
      #SurfaceWater
      graphSWdata<-passData()[TXIndus$`Water Type` == "Surface Water",]%>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE ))
      #ReuseWater
      graphRWdata<-passData()[TXIndus$`Water Type` == "Reuse",]%>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(monthlyWaterUseMG = sum(monthlyWaterUseMG, na.rm = TRUE )) 
      
      #Graph of total water usage
      theGraph <- ggplot(data= graphdata,aes(graphdata$Year,graphdata$monthlyWaterUseMG)) + geom_line(color="darkgreen")+
        
        labs(x ="Year", y="Industrial Water Use (MG)", title = "U.S. Industrial Water Use by Water Type",
             caption= "Based on data from TWDB") +
        scale_y_continuous(labels=comma, breaks = seq(0,1500000, by =250000))+
        scale_x_continuous(breaks = seq(1970,2015, by= 5))+
        geom_ribbon(ymin=0, ymax=graphdata$monthlyWaterUseMG, fill = 'lightgreen', alpha=0.3)
      
      if(input$WaterType == "Groundwater"){ #need to be adjusted to reflect change in years
      theGraph <- theGraph +
            geom_line(data= graphGWdata, aes(graphGWdata$Year,graphGWdata$monthlyWaterUseMG), color='steelblue')
      }
      if(input$WaterType == "SurfaceWater"){ #need to be adjusted to reflect change in years/ correct SS/GW
        theGraph <- theGraph +
          geom_line(data= graphSWdata, aes(graphSWdata$Year,graphSWdata$monthlyWaterUseMG), color='purple')
        
      }
      if (input$WaterType == "Reuse"){ #need to be adjusted to reflect change in years/ correct SS/GW
        theGraph <- theGraph+
          geom_line(data= graphRWdata, aes(graphRWdata$Year,graphRWdata$monthlyWaterUseMG), color='orange')
      }
      if(input$WaterType == ("All" )){ #need to be adjusted to reflect change in years
        theGraph <- theGraph+
          geom_line(data= graphGWdata, aes(graphGWdata$Year,graphGWdata$monthlyWaterUseMG), color='steelblue')+
          geom_line(data= graphSWdata, aes(graphSWdata$Year,graphSWdata$monthlyWaterUseMG), color='purple')+
          geom_line(data= graphRWdata, aes(graphRWdata$Year,graphRWdata$monthlyWaterUseMG), color='orange')
      }
      
      
      if(input$smoother){
      theGraph <- theGraph + geom_smooth()
      }
      print(theGraph)
      })
  
#2c. Texas Map trends tab: make map of TX
  output$TexasMap<- renderPlot({
    #get data ready
    Mapdata<-TXIndus %>%
      dplyr::group_by(Year, TXIndus$'County Used' ) %>%
      dplyr::summarise(monthlyWaterUseMG = 
                         sum(monthlyWaterUseMG, na.rm = TRUE ))
    #change column names
    names(Mapdata)[2] <- paste("countyused")
    
    #load TX county map file from online
    TXmap<- map_data("county", "texas")
    #change column names to match Mapdata file
    names(TXmap)[6] <- paste("countyused")
    #The county names in the map_data file are lowercase, so we need to change it
    TXmap[,6] = toupper(TXmap[,6])
    
    #merge the Mapdata file with the county map      
    choro <- left_join(TXmap, Mapdata, by = "countyused")
    choro <- choro[order(choro$order), ]
    
    #plot 
    gg<-ggplot(TXmap, aes(long, lat))+
      geom_polygon(data=choro,  aes(x= long,y=lat,
                                    fill = monthlyWaterUseMG, group= group), 
                   color= "white", size= 0.1)+
      labs(x= NULL, y= NULL,
           title= "Texas Industrial Water Usage",
           caption= "Data from TWDB")
    
    #send plot to output
    plot(gg)
  })     


#2d.Largest Users tab: make table of largest water users in time frame
  output$LargestUsers<- DT::renderDataTable({
    groupUsers<- TXIndus%>%
    dplyr::group_by(Organization, Description)%>%
    dplyr::summarise('Total Water Usage (MG)'=  
               as.numeric(round(sum(monthlyWaterUseMG))))
    sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
    head(sortedUsers, 10)
    #groupUsers<- group_by(passData(), TXIndus$Organization)
     })
 
  #2E. summary tab
  output$Summary<- renderText({
    
  #Percent Surface Water  
    GroupPurSS<- TXIndus %>%
      dplyr::group_by(PurchasedSS)%>%
      dplyr::summarise("TotalWaterUseMG" = sum(monthlyWaterUseMG)) %>%
      dplyr:: mutate(Percent = round(TotalWaterUseMG/ sum(TotalWaterUseMG)*100))
    paste0("Key facts:")
    TextWater<- paste0("The total amount of surface water used in this time period is", " ", GroupPurSS$Percent[GroupPurSS$PurchasedSS =="SS"], "%")
    
  #Aquifer with the largest withdrawal
    #group by the aquifer name----
    GroupBasin<- TXIndus %>%
      dplyr::group_by(TXIndus$`Basin Source`)%>%
      dplyr::summarise("TotalWaterUseMG" = format(round(sum(monthlyWaterUseMG)), big.mark = ","))
    colnames(GroupBasin)[1]<- "Basin"
    TextAquifer<- paste0("The aqufier with the most groundwater withdrawal is", " ", head(GroupBasin$Basin, 1))
#, ". The total amount ofwithdrawal in this time period is", sum(as.numeGroupBasin$TotalWaterUseMG), "MG.")
    
    #organization company use
    groupUsers<- TXIndus%>%
      dplyr::group_by(Organization, Description)%>%
      dplyr::summarise('Total Water Usage (MG)'=  
                         as.numeric(round(sum(monthlyWaterUseMG))))
    sortedUsers<- arrange(groupUsers, desc(groupUsers$`Total Water Usage (MG)`))
    TextUser<-paste("The organziation with the most overall water use is", head(sortedUsers$Organization, 1),", which specializes in", 
          head(sortedUsers$Description, 1, ".", "They have used a total of ", head(sortedUsers$`Total Water Usage (MG)`, 1), "MG"))
    paste(TextWater,  TextUser)
    
   # infoBox(
    #  "Percent of Surface Water", 
     # paste0(GroupPurSS$Percent[GroupPurSS$PurchasedSS =="SS"], "%"),
      #icon= icon("pie-chart"),
      #color= "green")
    
  })
#list of Counties
  output$reactCounties<- renderUI({
    countyList= unique(as.character(passData()$`County Used`))
  })
  

  

#2e.Credits Page: 
  output$Credits<- renderText({
    #creditText <- paste("Data provided by the Texas Water Development Board")
    creditText <- paste(ymd(input$dateRange[1]))
    print(creditText)
    })

#End server Script    
})
