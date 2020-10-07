library(shiny)
library(leaflet)

fluidPage(
    leafletOutput("map", height = 1000),   #지도 불러오기위한 함수  
    absolutePanel(top = 30, right =15,     #패널 위치 설정
                  selectInput("location", "Select a location:",    #데이터의 location데이터를 넣어라, Select a location:은 label 이름
                              choices = list("All location",       #모든 지역과 각각의 지역 선택
                                             location = c(
                                               "천마재활원", "천마산조각공원","은애모자원", "암남동 주민센터","아미시장","아미동 주민센터", "송도가정", "송남시장","서대신4동 주민센터", "서대신3동 주민센터","서대신1동 주민센터", "서대신1동 골목시장",
                                                "서구청 평생학습관","서구노인복지관", "부산서구청", "부민동 주민센터", "부민노인복지관",
                                                "동대신3동 주민센터", "동대신2동 주민센터", "동대신1동 주민센터", "동대신1동 골목시장", "대신공원","남부민2동 주민센터", "남부민1동 주민센터","부산 스마트팜"
                                               )))))
