        #house price prediction

#install package 
install.packages("shiny")

#import library
library(shiny)

#making frountend
ui<-fluidPage(
  
  
  headerPanel("HOUSE PRICE PREDICTION"),
  
  sidebarPanel(
    
    selectInput("area","chose the area",list("built-up area 1",
                                             "super built- up area 4",
                                             "plot area   3",
                                             "carpet area  2")),
    textInput("area id ","enter the area id you see in the above part :- ","" ),
    textInput("location id","enter the location id :-",""),
    testinput("bkh","how many bkh flat you want :-",""),
    textInput("sqft","enter the total sqft:- ",""),
    textInput("bath","how many bath you want:- ",""),
    textInput("balcony","how many balcony you want:- ",""),
    actionButton('go',"predict")
  ),
  
  
  mainPanel( width = 25,
             
             headerPanel("THE PRICE OF THE HOUSE IS :-"),
             
             textOutput("value ")
    )
  )



#making backend
server<-function(input,output){
  
  data2= reativevalues()
  observeEvent(input$go,{
    
    data=read.csv("https://storage.googleapis.com/kagglesdsdata/datasets/20710/26737/Bengaluru_House_Data.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20230315%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20230315T095258Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=8b32d8d71b8379fdb9cca0b6f790514bfc10143c321ba5bba20698f99a6eab045dc1093155104488aee559358dfd682e42a73cfe5ac759c51e717d87df9b2257ec13c0e1b91289d9b421ebaf4ff53412d6de0ac3edf155e08f32a98c73a4110a828723a0560b7181b354d100397c7ef735e4b6611d6a80e1ef5a2f23831386357d3c738a5cede4f8d4fd726b1456e6118fc520d4fd60d630c19f43787b1b052eabb1ba57f3917acfa1fb2833900b55d5e3f86b493527f306aec06c613c03cee837cf37e1f6f8b0946f400483f310732bc5ce94680c4296e97115a6cc382169ef89af803951ce387bb663717fea9f7389b49518389483cdd2ecbe519e420fc5ce")
    View(data)
    summary(data)
    str(data)
    
    is.factor(data$area_type)
    is.factor(data$location)
    
    data$area_type = as.factor(data$area_type)
    data$location = as.factor(data$location)
    
    str(data$area_type)
    str(data$location)
    str(data)
    
    use_data= data[,c("area type","loaction","size","total sqft","bath","balcony","prize")]
    head(use_data)
    summary(use_data)
    
    na_clean_data=na.omit(use_data)
    summary(na_clean_data)
    str(na_clean_data)
    View(na_clean_data)
    
    area.type=supply(na_clean_data$location, as.numeric)
    View(data.location)
    
    second_final=cbind(na_clean_data,area_id=area.type)
    View(second_final)
    
    second_main_dataset=cbind(second_final,location_id=data.location)
    View(second_main_dataset)
    
    main_data_set= second_main_dataset[,c("area type","loaction","size","total sqft","bath","balcony","prize")]
    View(main_data_set)
    
    inputdata=main_data_set[,c("area type","loaction","size","total sqft","bath","balcony","prize")]
    View(inputdata)
    
    data2$myarea_id <- as.numeric(input$area_id)
    data2$mylocation_id <-as.numeric(input$location_id)
    data2$mybhk <- as.numeric(input$bhk)
    data2$mysqft <- as.numeric(input$sqft)
    data2$mybath <- as.numeric(input$bath)
    dat2$mybalcony <- as.numeric(input$balcony)
    
    newPredict =data.frame(area_id =data2$myarea_id , location_id=data2$mylocation_id,
                           size = data2$mybhk , total_sqft= data2$mysqft,
                           bath = data2$mybath, balcony = data2$mybalcony)
    
    
    modelLM = lm(price ~ area_id+location_id+size_total_sqft+bath+balcony,
                 data =inputdata,weights=1/inputdata$price^1.9)
    
    data$
    
  })
}


















































