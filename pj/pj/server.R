library(shiny)
library(leaflet)
library(dplyr)
library(readr)

setwd("C:/Rshiny/pj")


rd<-read.csv("busan_west_wifi2.csv", header = T, stringsAsFactors = F, fileEncoding = "euc-kr")
write_rds(rd, "r.rds") 
wifi<- read_rds("r.rds")


shinyServer(function (input, output) {
    
    output$map <- renderLeaflet({                         #output에 지도를 불러와라
        filtered <- 
            if (input$location == "All location") {       #input에 All location을 선택하면 전체, 
                wifi
            } else {
                filter(wifi, location == input$location)     #그렇지 않으면 내가 선택한 지역만 보여줘라
            }
        
        leaflet(filtered) %>%
            addProviderTiles(providers$Esri.WorldTopoMap)%>%   #지도모양 선택-종류는 다양하게 있다
            addMarkers(lng=~LON,                         #addMarkers는 지도에 각 지역에 표시되는 마크모양 
                       lat=~LAT
            )
    })
    
})
