library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(png)
library(knitr)
library(ggthemes)


#Read in advantages chart
url1<-"https://raw.githubusercontent.com/defense031/ST590_Proj2_PoGo/master/PoGoAdvantageChart.csv"
advantages<-read.csv(quote="",text=getURL(url1),header=TRUE)
row.names(advantages)<-advantages[,1]
colnames(advantages)[20]<-"noType2"

#Read in Individual Pokemon Data
url2<-"https://raw.githubusercontent.com/defense031/ST590_Proj2_PoGo/master/PoGoIndividualData.csv"
pogo<-read.csv(text=getURL(url2),header=TRUE)
newDPS<-rep(0,length(pogo$Pokemon))
fastAdv<-rep(0,length(pogo$Pokemon))
chargeAdv<-rep(0,length(pogo$Pokemon))
totAdv<-rep(0,length(pogo$Pokemon))
pogo<-cbind(pogo,newDPS,fastAdv,chargeAdv,totAdv)
newData<-pogo

#Read in damage mechanics external URL
url3<-a("Damage Mechanics", href="https://pokemongo.gamepress.gg/damage-mechanics")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #Create title page
  output$titleText<-renderUI({
    text<-paste0("Best Pokemon Go Raid Counters against ",str_to_title(input$bossName))
    h3(text)
  })
  
  #Create function that outputs bossName
  output$name<-renderUI({
    text2<-paste0(input$bossName)
    h4(text2)
  })

  #Create link for info page
  output$link <- renderUI({
    tagList("URL link:", url3)
  })
  

getData<- reactive({
  
  #If not sorting by generation
  if(input$includeGens==FALSE){
    #filter by legendary yes/no
      if(input$leg==FALSE){
        filterData<-filter(newData,Legendary==FALSE)
      }else{filterData<-pogo}
    #If sorting by generation
  }else{
    #sorting by generation with legendary selected FALSE
    if(input$leg==FALSE){
      filterData<-filter(newData[newData$Generation %in% input$gens,])
      filterData<-filter(filterData,Legendary==FALSE)
                         
      #sorting by generation with legendary selected TRUE
    }else{filterData<-filter(newData[newData$Generation %in% input$gens,])
    }
  }

  #Code to find type1 of raid boss
  bossType1<-as.character(pogo[which(pogo$Pokemon==input$bossName)[1],2])
  #Code to find type2 of raid boss
  bossType2<-ifelse(as.character(pogo[which(pogo$Pokemon==input$bossName)[1],3])=="","noType2",as.character(pogo[which(pogo$Pokemon==input$bossName)[1],3]))
  #boss advantages chart for raid boss
  bossAdvantages<-advantages[c(bossType1,bossType2)]
  
  #Initialize vector to catch fastType, fastAdv, chargeType
  
  for(i in 1:length(filterData$Pokemon)){
    #Find fast type advantage for each Pokemon
    filterData$fastAdv[i]<-bossAdvantages[as.character(filterData$FastType[i]),bossType1]*bossAdvantages[as.character(filterData$FastType[i]),bossType2]
    #Find charge type advantage for each Pokemon
    filterData$chargeAdv[i]<-bossAdvantages[as.character(filterData$ChargedMoveType[i]),bossType1] * bossAdvantages[as.character(filterData$ChargedMoveType[i]),bossType2]
    #Find DPS Modifier for each Pokemon
    filterData$totAdv[i]<-mean(c(filterData$chargeAdv[i],filterData$fastAdv[i]))
    filterData$newDPS[i]<-filterData$DPS[i]*filterData$totAdv[i]
  }
  #Now find top n in adjusted DPS vs raid boss
  top<-arrange(filterData,desc(newDPS))[1:input$numMon,]
  nameCat<-paste0(top$Pokemon," ",top$FastMove," ",top$ChargedMove)
  top<-cbind(top,nameCat)
  top<-top
  
})


#Create DPS plot
  output$DPSPlot<-renderPlot({
    top<-getData()
    g<-ggplot(data=top)
    g+geom_point(aes(x=top$nameCat,y=top$newDPS),color=top$TypeColor,
                 size=10*percent_rank(top$TDO))+
      theme_solarized()+
      theme(axis.text.x=element_text(angle=60,hjust=1))+
      xlab("")+ylab("Adjusted DPS")
  })

#Create DPS vs Health plot
  output$HealthPlot<-renderPlot({
    top<-getData()
    h<-ggplot(data=top,aes(x=top$Stamina,y=top$newDPS),color=top$TypeColor)
    h+geom_point()+
      geom_text(aes(label=top$nameCat),hjust=.1,vjust=1)+
      theme_solarized()+
      xlab("Health")+ylab("Adjusted DPS")
  }) 
  
  
  
  
#Create output of observations    
  output$DPStable <- renderTable({
    top<-getData()
    sumTable<-top[,c(1,2,3,4,6,8,9,10,18,12,16)]
    sumTable[,9]<-round(sumTable[,9],digits=1)
    colnames(sumTable)<-c("Pokemon","Type 1", "Type 2","Fast Move", "Charged Move","Stamina","Attack","Defense","Adj. DPS","Total Damage Output","Leg.")
    sumTable
  })
  
#Download Data
  datasetInput <- reactive({
    switch(input$dataset,
           "Top Counters" = getData(),
           "Full Dataset" = pogo,
           "Type Advantages"=advantages
           )
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )

#Download plot1
  output$downloadPlot1<- downloadHandler(
    filename = 'test.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = DPSPlot, device = device)
    })
  
                                           
  
#End of app
})
  

