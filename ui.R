#Austin Semmel Project 2
library(shiny)
library(dplyr)
library(ggplot2)
library(knitr)
library(shinythemes)
library(shinydashboard)
library(RCurl)
library(shinydashboardPlus)

url2<-"https://raw.githubusercontent.com/defense031/ST590_Proj2_PoGo/master/PoGoIndividualData.csv"
pogo<-read.csv(text=getURL(url2),header=TRUE)

shinyUI(dashboardPagePlus(skin="blue",
  
  # Application title
  dashboardHeaderPlus( 
    title="Pokemon Go Data Exploration",
    titleWidth=300,
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 75}"),
            tags$style(".main-header .logo {height: 75;}"),
            tags$style(".sidebar-toggle {height: 75; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:75 !important}")
    )
  ),
  # Sidebar 
  dashboardSidebar(
    sidebarMenu(
      menuItem("About this App", tabName="about",icon=icon("question")),
      menuItem("Raid Counters", tabName = "raids", icon = icon('line-chart'),
                    menuSubItem('Top Counters by DPS', tabName = 'subDPS1',selected=TRUE) ,
                    menuSubItem('DPS vs Health', tabName = 'subDPS2') 
      ),
      menuItem("Supervised Learning", tabName="supLearn",icon=icon("eye"),
               menuSubItem('Classification Tree',tabName='classTree',selected=TRUE),
               menuSubItem('KNN', tabName="knn")
               ),
      menuItem("Unsupervised Learning", tabName="unsupLearn",icon=icon("eye-slash"),
               menuSubItem('PCA',tabName='pca',selected=TRUE)
               ),
               
      menuItem("Data", tabName = "data",icon = icon("th"))
      )
    ),
    #Main Panel
    dashboardBody(
      tabItems(
          tabItem(tabName="about",
                h3("How This App Works and Other Cool Info"),
                fluidRow(
                  box(title="Motivation for App",
                      h5(
                      "Pokemon Go Raids are pretty popular in Durham and Raleigh these days.
                      People gather together at random points of interest called 'gyms' and
                      battle against a computer boss Pokemon.  Tier 5 Raids can be pretty difficult!
                      You only get one unpaid raid per day, so you have to make them count.
                      To do so, you need a good knowledge of how to make a party based on
                      what Pokemon you are going up against!  This app will help show you the 
                      best counters against a boss based on their type-adjusted Damage Per Second
                      (DPS).  The formulas on the right show how I calculated DPS and adjusted DPS based
                      on real data from the original games.  These are the same formulas that the
                      developers use (minus Mewtwo who infamously received a nerf for being OP).
                      Check out this link for more info on damage mechanics!"
                      ),
                  uiOutput("link")
                  ),
                  box(title="Formulas",
                      withMathJax(),
                      helpText(h5("The initial damage of a single move is calculated via this formula rounded down:
                                $$Damage=Power*\\frac{Attack}{Defense}*Multiplier+1$$
                                The damage multiplier, though, changes based on the type of attack being used
                                and the opponent's type.  For example, a pokemon using a 'Fire' based attack
                                would be very effective against 'grass' but ineffective against 'water.'
                                We can dynamically find the damage-type multiplier for each pokemon based on the raid boss.
                                $$Fast Atk Mult=$$
                                $$(FastMove*BossType1)*(FastMoveAdv*BossType2)$$
                                $$ Charged Atk Mult=$$
                                $$(ChargedMoveAdv*BossType1)*(ChargedMoveAdv*BossType2)$$
                                $$Total Damage Multiplier=$$
                                $$Average(Fast Mult,Charged Mult)$$
                                Assuming that roughly half of the damage is from the fast attack and half
                                is from the charged attack.  Then, the Adjusted DPS is:
                                $$AdjDPS= DPS * Total Multiplier$$
                                You can see the values for this chart in the 'Advantages' table."
                               ))
                    )
                )
          ),
          tabItem(tabName="subDPS1",
                  uiOutput("titleTextDPS1"),
                  fluidRow(
                    column(12,
                    plotOutput("DPSPlot",click="plot_click"),
                           verbatimTextOutput("dpsPlotInfo"))),
                  fluidRow(h4("This plot displays the top Pokemon in terms of DPS against ", uiOutput("name"))),
                  fluidRow(
                    column(3,
                           textInput("bossName","What Pokemon are you battling against?",value="Charizard")
                    ),
                    column(3,
                           checkboxInput("leg","Do you want to include legendaries?",value=TRUE)
                    ),
                    column(3,
                           #Generation conditional panel
                           checkboxInput("includeGens","Do you want to separate by Generation?",value=FALSE),
                           conditionalPanel(condition="input.includeGens=='1'",
                                            checkboxGroupInput("gens","Which Generations do you want to include?",
                                                               choices=levels(pogo$Generation),inline=TRUE))
                    ),
                    column(3,
                           sliderInput(inputId="numMon",
                                       label="How many counters would you like to display?",
                                       min = 6,
                                       max = 12,
                                       value = 6)
                    )
                    ),
                  downloadButton('downloadPlot1', 'Download Plot')
          ),
          tabItem(tabName="subDPS2",
                  uiOutput("titleTextDPS2"),
                  fluidRow(
                    column(12,
                          plotOutput("HealthPlot",click="plot_click"),
                          verbatimTextOutput("healthPlotInfo"))),
                  fluidRow(h5("This graph shows a plot of each Pokemon's DPS vs its health.  
                               This could be of value if you want to make sure you don't simply have
                               Pokemon that are 'glass cannons.'")),
                  fluidRow(
                    column(3,
                           textInput("bossName2","What Pokemon are you battling against?",value="Charizard")
                    ),
                    column(3,
                           checkboxInput("leg2","Do you want to include legendaries?",value=TRUE)
                    ),
                    column(3,
                           #Generation conditional panel
                           checkboxInput("includeGens2","Do you want to separate by Generation?",value=FALSE),
                           conditionalPanel(condition="input.includeGens2=='1'",
                                            checkboxGroupInput("gens2","Which Generations do you want to include?",
                                                               choices=levels(pogo$Generation),inline=TRUE))
                    ),
                    column(3,
                           sliderInput(inputId="numMon2",
                                       label="How many counters would you like to display?",
                                       min = 6,
                                       max = 12,
                                       value = 6)
                    )),
                  downloadButton('downloadPlot2', 'Download Plot')
          ),
          
          tabItem(tabName="classTree",
                  h3("Classification Tree on Pokemon Type"),
                  fluidRow(plotOutput("classTreePlot")),
                  fluidRow(
                    sliderInput(inputId="numTrees",
                                       label="Select the number of trees to display",
                                       min = 2,
                                       max = 13,
                                       value = 10)),
                  fluidRow(
                    column(9,
                           "This classification tree fits Pokemon main typing as the response with Attack, Defense, Stamina, DPS, CP, and Legendary Status as predictors.
                           We are mainly interested in seeing how these stats contribute to a Pokemon's type.
                           While this method does not seem to perform very well, as evidenced by the fairly large misclassification rate,
                           it is still significantly better than randomly guessing (1/17)",
                           uiOutput("classText"))
                            )
          ),
          
          tabItem(tabName="knn",
                  h3("KNN"),
                  fluidRow(plotOutput("knnPlot")),
                  fluidRow(uiOutput('knnText'),
                           "It seems as though KNN does not do well in this situation.  Since the developers do a good deal of 'balancing,' inside the game,
                            we don't see much correlation between stats and type, other than a few types.  For example, psychic is fairly well classified since 
                            it tends to have very high attack ratings"),
                  fluidRow( sliderInput(inputId="knnFolds",
                                        label="Select the number of neighbors to use",
                                        min = 1,
                                        max = 20,
                                        value = 10))
                  ),
          
          tabItem(tabName="pca",
                  h3("Principal Component Analysis"),
                  fluidRow(
                    column(6,
                      plotOutput("pcaPlot")),
                    column(6,
                    plotOutput("scree"))
                  ),
                  fluidRow(checkboxGroupInput("pcaChoices","Which variables would you like to include in the PCA Model?",
                                              choices=c("Attack","Defense","Stamina","DPS","CP","Legendary"),inline=TRUE,
                                              selected=c("Attack","Defense","Stamina","DPS","CP","Legendary")))
          ),
          
          tabItem(tabName="data",
              uiOutput("topMon"),
              fluidRow(tableOutput("DPStable")),
              selectInput("dataset", "Choose a dataset:",
                          choices = c("Top Counters","Full Dataset","Type Advantages")),
              downloadButton("downloadData","Download Data")
          )
    )
    )
)
)
