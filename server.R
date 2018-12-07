library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(png)
library(knitr)
library(ggthemes)
library(factoextra)
library(class)
library(tree)


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
newData$CP<-as.numeric(newData$CP)

#Read in damage mechanics external URL
url3<-a("Damage Mechanics", href="https://pokemongo.gamepress.gg/damage-mechanics")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #Create titles for subplots
  output$titleTextDPS1<-renderUI({
    text<-paste0("Plot of Top DPS Counters against ",str_to_title(input$bossName))
    h3(text)
  })
  output$titleTextDPS2<-renderUI({
    text<-paste0("Plot of Top TDO against ",str_to_title(input$bossName))
    h3(text)
  })
#Create title for tab
  output$topMon<-renderUI({
    text<-paste0("Top 'Mon Against: ",str_to_title(input$bossName))
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
  
  #Initialize vector to catch fastType, fastAdv, chargeType, chargeAdv
  
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

getData2<- reactive({
  
  #If not sorting by generation
  if(input$includeGens2==FALSE){
    #filter by legendary yes/no
    if(input$leg2==FALSE){
      filterData<-filter(newData,Legendary==FALSE)
    }else{filterData<-pogo}
    #If sorting by generation
  }else{
    #sorting by generation with legendary selected FALSE
    if(input$leg2==FALSE){
      filterData<-filter(newData[newData$Generation %in% input$gens2,])
      filterData<-filter(filterData,Legendary==FALSE)
      
      #sorting by generation with legendary selected TRUE
    }else{filterData<-filter(newData[newData$Generation %in% input$gens2,])
    }
  }
  #Code to find type1 of raid boss
  bossType1<-as.character(pogo[which(pogo$Pokemon==input$bossName2)[1],2])
  #Code to find type2 of raid boss
  bossType2<-ifelse(as.character(pogo[which(pogo$Pokemon==input$bossName2)[1],3])=="","noType2",as.character(pogo[which(pogo$Pokemon==input$bossName)[1],3]))
  #boss advantages chart for raid boss
  bossAdvantages<-advantages[c(bossType1,bossType2)]
  
  #Initialize vector to catch fastType, fastAdv, chargeType, chargeAdv
  
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
  top<-arrange(filterData,desc(newDPS))[1:input$numMon2,]
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
    top<-getData2()
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
  
#Supervised Learning (classification tree)
    subDataClass<-select(pogo,Type1,Attack,Defense,Stamina,CP,Legendary,DPS,Generation)
    subDataClass$CP<-as.numeric(subDataClass$CP)
    train<-sample(1:nrow(subDataClass),size=nrow(subDataClass)*.8)
    test<-dplyr::setdiff(1:nrow(subDataClass),train)
    data.train<-subDataClass[train, ]
    data.test<-subDataClass[test, ]
    fitTree<-tree(Type1~DPS+Stamina+Attack+Defense+CP+Generation+Legendary,
                  data=data.train,
                  method="class")
    
#Classification Tree Plot
    output$classTreePlot<-renderPlot({
      #Prund Tree
      pruneTree<-prune.tree(fitTree,best=input$numTrees)
      #Get misclass rate
      predsTree<-predict(pruneTree, dplyr::select(data.test,-"Type1"),
                         type="class")
      predTable.tree<-table(data.frame(predsTree,data.test[, "Type1"]))
      misclass.tree<-1-sum(diag(predTable.tree)/sum(predTable.tree))
      misclass.tree
      #Output plot
      par(bg="gray40")
      plot(pruneTree)
      text(pruneTree,col="aquamarine",srt=80,cex=1)
    })

#Get text for classification tree misclass paragraph
output$classText<-renderUI({
  pruneTree<-prune.tree(fitTree,best=input$numTrees)
  #Get misclass rate
  predsTree<-predict(pruneTree, dplyr::select(data.test,-"Type1"),
                     type="class")
  predTable.tree<-table(data.frame(predsTree,data.test[, "Type1"]))
  misclass.tree<-1-sum(diag(predTable.tree)/sum(predTable.tree))
  misclass.tree
  text<-paste0("The misclassification rate, based off of an 80/20 training vs test set partition is: ", round(misclass.tree,digits=3))
})

#Supervised Learning...KNN
#Supervised...KNN
    subDataKNN <- select(pogo, Type1,Attack, Defense, Stamina,DPS, Legendary, CP)
    subDataKNN$CP<-as.numeric(subDataKNN$CP)
    subDataKNN%>%
      mutate_if(is.numeric, scale)
    subDataKNN$Legendary<-as.numeric(subDataKNN$Legendary)
    subDataKNN.train<-subDataKNN[train, ]
    subDataKNN.test<-subDataKNN[test, ]

##Fit KNN
getKNNFit<-reactive({
  knnFit<-knn(train=select(subDataKNN.train,Attack,Defense,Stamina,DPS,Legendary,CP),
              test=select(subDataKNN.test,Attack,Defense,Stamina,DPS,Legendary,CP),
              cl=subDataKNN.train$Type1,
              k=input$knnFolds)
})

#Get knn confusion matrix
  getMisclassKNN<-reactive({
  knnFit<-getKNNFit()
  fitInfoKNN<-tbl_df(data.frame(knnFit,select(subDataKNN.test,Type1,Attack)))
  tblKNN <- table(fitInfoKNN$knnFit,fitInfoKNN$Type1)
    })

#KNN Text
  output$knnText<-renderUI({
    tblKNN2<-getMisclassKNN()
    misclassKNN<-1-sum(diag(tblKNN2))/sum(tblKNN2)
    misclassKNN
    text<-paste0("The misclassification rate, based off of an 80/20 training vs test set partition is: ", round(misclassKNN,digits=3))
  })
  
#KNN Plot
  output$knnPlot<-renderPlot({
    knnFit<-getKNNFit()
    typeColor<-rep("",length(subDataKNN.test$Type1))
    subDataKNN.test<-cbind(subDataKNN.test,typeColor)
    subDataKNN.test$typeColor<-pogo$TypeColor[match(subDataKNN.test$Type1, pogo$Type1)]
    
    plot.df = data.frame(subDataKNN.test, predicted = knnFit)
    plot.df1 = data.frame(x = plot.df$Attack, 
                          y = plot.df$Type1, 
                          predicted = plot.df$predicted)
    
    find_hull = function(df) df[chull(df$x, df$y), ]
    boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)
    
    kn<-ggplot(plot.df, aes(Attack, Type1, color = predicted, fill = predicted))
    kn+geom_point(size = 5) +geom_polygon(data = boundary, aes(x,y), alpha = 0.5)
  })
    
#PCA
  getPCA<-reactive({
  subData<-select(pogo,Attack,Defense, Stamina, DPS, CP, Legendary)
  subData$Legendary<-as.numeric(subData$Legendary)-1
  subData$CP<-as.numeric(subData$CP)
  subDataPCA <- select(subData,c(input$pcaChoices))
  PCs <- prcomp(subDataPCA, center = TRUE, scale = TRUE)
  PCs
  })

#PCA Visualization  
  output$pcaPlot<-renderPlot({
  PCA<-getPCA()
  fviz_pca_var(PCA,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     # Avoid text overlapping
  )
  })
#PCA Scree plot
  output$scree<-renderPlot({
    PCA<-getPCA()
    fviz_screeplot(PCA, addlabels = TRUE, ylim = c(0, 50))
  })
  
#Clustering
  #define data
  subDataCluster<-select(pogo,Attack,Defense,Stamina,CP,DPS,Legendary)
  subDataCluster$CP<-as.numeric(subDataCluster$CP)
  subDataCluster$Legendary<-as.numeric(subDataCluster$Legendary)
  
  #Get cluster method input
  getClustMeth<-reactive({
    switch(input$clusterMethod,
           "Average"="average",
           "Complete"="complete",
           "Median"="median",
           "Single"="single",
           "Centroid"="centroid")
  })
  
  #Get number of clusters
  getNumClust<-reactive({
    numClust<-input$numClust
  })
  
  #Fit hierarchical cluster model
  getClusterMod<-reactive({
  numClust<-getNumClust()
  clustMethod<-getClustMeth()
  hierClust<-hclust(dist(subDataCluster),method=clustMethod)
  plot(hierClust,xlab="")
  pruneClust<-cutree(hierClust,k=numClust)
  newSubDataCluster<-cbind.data.frame(subDataCluster,cluster=as.factor(pruneClust))
  })
  
#Cluster Text
  output$clusterText<-renderText({
    k<-getNumClust()
    paste0("K = ",k," Clusters")
  })
  
  #cluster plot
  output$clusterPlot<-renderPlot({
  clusterModel<-getClusterMod()
  clustMethod<-getClustMeth()
  hc<-ggplot(data=clusterModel,aes(x=Attack,y=Defense, color=cluster))
  hc+geom_point()
  })
  
  #cluster comp plot
  output$compPlot<-renderPlot({
    cp<-ggplot(data=pogo,aes(x=Attack,y=Defense,color=Type1))
    cp+geom_point()
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
  
#Make DPS Plots clickable
  output$healthPlotInfo<-renderText({
    paste0("Health= ", input$plot_click$x, "\nAdj. DPS = ", input$plot_click$y)
  })

  output$dpsPlotInfo<-renderText({
    paste0("\nAdj. DPS = ", input$plot_click$y)
  })
    
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
  

