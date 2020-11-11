#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(wesanderson)
library(dbplyr)
library(MASS)
library(ISLR)
library(RColorBrewer)
library(GGally)
library(gghighlight)
library(hrbrthemes)
library(viridis)
library(scales)
library(png)
library(grid)
library(fmsb)
library(RColorBrewer)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = "bootstrap.css",
    
    # Application title
    titlePanel("Interactive Fifa-Data Analysis"),
    
    navbarPage(title = "Select a tab which analysis of the Fifa Data of 2019 you want to see",
        tabPanel("Fifa Tabelle",
                 mainPanel(
                    p("This is the complete Dataset which was used to make the following analyses"),
                     DTOutput(outputId = "table")
                 )
        ),
        
        # tabPanel("Gegenüberstellungen",
        #          sidebarLayout(
        #              sidebarPanel(
        #                selectInput('x','X', choices = c("Age", "Wage")),
        #                            #selected = "age"),
        #                conditionalPanel(
        #                  condition = "input.x == 'Age'",
        #                  sliderInput(inputId = "numAge",
        #                             label = "Select an Age from",
        #                             value = 18, min = 10, max = 50),
        #                ),
        #                selectInput('y', 'Y', choices = c( "Overall","Crossing")),
        #              ),
        #              
        #              # Show a plot of the generated distribution
        #              mainPanel(
        #                  plotOutput("gegenueberstellungen")
        #              )
        #          )
        #          ),
        
        tabPanel("Heading Precision Relations",
                 mainPanel(
                   p(
                     "In order to visualize whether the heading performance is closely related to weight and height we need to see if these factors correlate. The easiest way to do that is by having a relationship between these factors. Luckily there already is an established factor for these values - it's called the BMI. So we added a column to our dataset and calculated each BMI for every player. The heading performance is on a scale from 0-100"
                     ),
                   DTOutput("headingPerfTable"),
                   p(
                     "Now we need to see how these values correlate to each other. To do that, we make a correlation chart."
                   ),
                   plotOutput("headingPerfCorr"),
                   p(
                     "These values give us a clear image of the general build of a football player. Weight and height have a positive correlation of 1 - more height more weight. Weight alone has a low positive correlation to the accuracy. Makes sense, the heavier a player the less nimble they are and don't have a high vertical reach (provided the height doesn't attribute to the players weight). Funnily enough the accuracy also doesn't have a strong positive correlation to either height or weight, which is clear if we look at the BMI relationship. Since the BMI is medium to strongly correlated to height and weight but relatively low to the heading accuracy gives us the clear impression that height and weight, so therefore the BMI, has to be in a certain range to be optimal for a players performance. Which gives the conclusion, that there is a BMI where a player is either tall enough or muscular (heavy) anough to have good heading prescion. To show how weight in height matter we created a heatmap:"
                   ),
                   plotOutput("headingPerf"),
                   p(
                     "So with that, it is obvious that the players with good accuracy (red tiles) are either rather tall and weigh less or are shorter and weight more which means they are more muscular. Since we can assume that every player has around the same level of fitness and carry the same bodyfat percentage."
                   ),
                   p(
                     "To make this chart a bit more entertaining, with the sliders below you can enter your weight and height and see where your body would fit within these players. Obviously, there is no real take away from that number, but it is a nice to see. Your standing is the purple dot"
                   ),
                   sliderInput(
                     inputId = "weightM",
                     label = "Enter your weight in kg",
                     value = 50, min = 50, max = 120
                  ),
                  sliderInput(
                    inputId = "heightM",
                    label="Enter your height in cm",
                    value= 150,min=150,max=200
                  )
                 )
                 
        ),
        
        tabPanel("Correlation of Overall Rating and Salary",
                mainPanel(
                     p(
                       "As we all know, good football players are worth a lot. Like a lot lot. So we were wondering how strong the correlation between overall performance rating and monetary value is. In order to do that we had to clean the data, since the value column had a Euro sign in it and omit nonsence values"
                     ),
                     DTOutput("valueTable"),
                     p(
                       "So of course, we want to see just how much the Overall performance, Value and Wage correlate. To do that, we again aggregate all values by Overall and make a correlation. For this, we specifically didn0t use the median but the mean function in order to have the extreme monetary values as well."
                     ),
                     plotOutput("corrGraphValue"),
                     p(
                       "Not suprisingly, the Rating, Value and Wage are all strongly correlated to each other"
                     ),
                     p(
                       "No it would be interesting to see just how much destributed these values are. To visualize that, we made a bubble plot. In this plot, the size of the bubble is the Wage per week (in thousand), they are colour coded according to the FIFA Top Ten Ranked countries (as of May 2020). The y axis is the overall rating and the x axis is the value of the player. The x axis is a logarithmic scale (base 10)"
                     ),
                     p(
                       "Since there are so many big values, the data is filtered by default by Value in the upper 90th percentile. With the slider you can change that filter to your liking"
                     ),
                     sliderInput(
                       inputId = "valueQuant",
                       label="Enter your preferred quantile",
                       value= 90,min=10,max=100
                     ),
                     plotOutput("ratingValue"),
                     p(
                       "To make this graph simpler to read, we made one with just overall rating and wage or value. You can choose which value should be compared to overall rating. We also overlayed the density of the plots (red line). As we have again very extreme values, you can choose the quantile again."
                     ),
                     sliderInput(
                       inputId = "valueQuant2",
                       label="Enter your preferred quantile",
                       value= 50,min=10,max=100
                     ),
                     selectInput('selectValueWage','Value or Ware', choices = c("Value", "Wage"),
                     selected = "Value"),
                     plotOutput("densGraph")
                  )
        ),
        
        tabPanel("Which section of performances do typically decrease/increase with age?",
                   # Show a plot of the generated distribution
                   mainPanel(
                     p(
                       "We were trying to find out which sections of performances typically increase or decrease with age. One would assume that performance factors increase by age. Because age usually goes hand in hand with experience. To show these relationships and whether they correlate or not we aggregated the dataset of all players and calculated the mean of every abilty by age. All abilities are on a scale from 0-100. With this method we get an average (median) dribbling score for all players at the age 20 at 80 for example. The aggregated data is shown below. Disclaimer: All Data is for field players only. Goalkeepers were exluded from the dataset."
                     ),
                     DTOutput("AgeStatsTable"),
                     p(
                        "With this data aggregated by age we created a correlation matrix to see what abilities correlate with on another"
                     ),
                     DTOutput("AgeStatsCorr1"),
                     p(
                       "The same data shown as a heatmap. The more saturated the colour, the closer the relationship is to 1. A correlation of 1 means the variables are propotional"
                     ),
                     plotOutput("HeatmapCorr1"),
                     p(
                       "With these representations of the data it is easier to see, which value is closer to 1 in relationship to the age."
                     ),
                     plotOutput("CorrelationGraph1"),
                     plotOutput("CorrelationGraph2"),
                     p(
                       "Weirdly enough, the data didn't make much sense at first. Why is the overall rating so loosely correlated with the other abilities? So we made the same aggregations and steps again, but this time tied it to the overall rating.
                       This datatable shows the same aggregation but this time for the Overall rating"
                     ),
                     DTOutput("OveStatsTable"),
                     p("Creation of the correlation matrix"),
                     DTOutput("OveStatsCorr1"),
                     p(
                       "As we look at the correlation graph, we see these values are much closer to 1 which makes sense. The better the abilities, the better the overall rating."
                     ),
                     plotOutput("CorrelationGraph3"),
                     plotOutput("CorrelationGraph4"),
                     p(
                       "At first glance, this doesnt make much sense. Why do we have different values, when aggreageted by age then by overall rating? Well, the explanation is actually quite simple. As age progresses, the abilities and therefore overall rating of a football player progress with him. However, there is a zenith where the age is high enough and abilities stagnate due to the natural wear and tear of a body dealing with a higher age. The Point is easily shown in a point graph where we aggregate the overall rating by age and see that at there is a threshhold, as shown below."
                     ),
                     plotOutput("AgeOverall"),
                     p(
                       "As we can see, the zenith is at age 37 with an overall rating of 70. The data gets scewed further since most players retire at their late 30s and there are few values for the ages higher than that since these are the players who have a great overall rating even at their higher age."
                     )
                   )
        ),
        
        tabPanel("Which positions do have which average stats (preferred performance factors)",
                 mainPanel(
                   p(
                     "To see who in which position has which average stats, we first had to have an underlying image of a football field. Then we had to extract the data and aggregate it in order to have the positions and enrich them with the coordinates on the football field. Then, each position is given a unique colour. Now, you can choose, if you want to see the mean of the overall rating, the players value or each unique ability of every player in that certain position. Or the number of players in that positions from our given data frame. Since the descriptions of each position would be too long, we added them below"
                   ),
                   radioButtons(
                     "fieldChoice","Select a value to be displayed at the corresponding positions",
                     c("Overall","Value","Crossing","Finishing","HeadingAccuracy","ShortPassing",
                       "Volleys","Dribbling","Curve","Count"="count")
                   ),
                   plotOutput("avgStatsField",height = "682px"),
                   p(
                     "Below is a spider chart wich shows the expression of each ability (0-100) in the chosen position."
                   ),
                   tags$head(
                     tags$style(HTML("
                                        
                                             .multicol {
                                        
                                               -webkit-column-count: 3; /* Chrome, Safari, Opera */
                                        
                                               -moz-column-count: 3; /* Firefox */
                                        
                                               column-count: 3;
                                        
                                             }
                                        
                    "))
                     
                   ),
                   
                   ## use the css, assuming your long list of vars comes from global.R
                   
                   wellPanel(
                     
                     tags$div(class = "multicol", checkboxGroupInput("choicePosition", choices = c('RF','ST','LW','GK','RCM','LF','RS','RCB','LCM','CB','LDM','CAM','CDM','LS','LCB','RM','LAM','LM','LB','RDM','RW','CM','RB','RAM','CF','RWB')
                                                                       , label = "Position:", selected = 'RF'))
                     
                   ),
                   plotOutput("posSpider")
                 )
          
        ),
        tabPanel("Value Prediction",
                 sidebarLayout(
                   sidebarPanel(
                   # sliderInput(inputId = "pAge", label="Age",value = 18, min = 10, max = 50),
                   # sliderInput(inputId = "pFinishing",label="Finishing",value = 20, min = 1, max = 100),
                   sliderInput(inputId = "pHeadingAccuracy",label="Heading Accuracy",value = 20, min = 1, max = 100),
                   sliderInput(inputId = "pShortPassing",label="Short Passing",value = 20, min = 1, max = 100),
                   sliderInput(inputId = "pVolleys",label="Volleys",value = 20, min = 1, max = 100),
                   sliderInput(inputId = "pCurve",label="Curve",value = 20, min = 1, max = 100),
                   # sliderInput(inputId = "pDribbling",label="Dribbling",value = 20, min = 1, max = 100),
                   ),
                   mainPanel(
                     p(
                       "Now with this little application, we're trying to predict your value, wage and overall rating from the most common abilities. Enter your abilities and the value will be predicted. Now this prediction relies on a linear prediction model where the most correlated variables to the value are taken. A players value is also reliant on a players prestige and many other factors which would stretch the framwork of this project too far. Since the abilities are only weakly correlated to the value, the model is probably "
                     ),
                     uiOutput("forec")
                   )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 

    fifaDataRaw <- read.csv("data/data.csv",stringsAsFactors=FALSE,encoding="UTF-8")

    fifaDataRaw <- as_tibble(fifaDataRaw)

    fifaDataRaw <- fifaDataRaw %>%
        separate(Height, c("HFeet", "HInches"), "'")
    fifaDataRaw <- fifaDataRaw %>%
        mutate(HeightMetric = ((as.numeric(HFeet)*30) + (as.numeric(HInches)*2.54)) )

    fifaDataRaw <- fifaDataRaw %>%
        separate(Weight, c("Weight"), "lbs",convert=TRUE)
    fifaDataRaw <- fifaDataRaw %>%
        mutate(WeightMetric = (as.numeric(Weight)/2.2) )

    #Sperate the Value and Wage column with the euro sign and multiplicator (either M or K)
    fifaDataRaw <- fifaDataRaw %>%
      separate(Wage,c(NA,"Wage"),"€",convert=TRUE)
    fifaDataRaw <- fifaDataRaw %>%
      separate(Wage,c("Wage"),"K",convert=TRUE)
    fifaDataRaw <- fifaDataRaw %>%
      separate(Value,c(NA,"Value"),"€",convert=TRUE)
    fifaDataRaw <- fifaDataRaw %>%
      separate(Value,c("Value", "multiplicator"), "(?<=[0-9])(?=[A-Z])")
    fifaDataRaw <- fifaDataRaw %>%
      mutate(
        Value = ifelse(multiplicator=="K",as.numeric(Value)*1000,
                       ifelse(multiplicator=="M",as.numeric(Value)*1000000,0))
      )

    newdata <- (fifaDataRaw %>% filter(Wage > quantile(Wage , 0.25 )))

    newdata <- newdata[order(newdata$Wage),]
    head(newdata)

    print(newdata$Wage)

    fifaDataRaw$WeightMetric <- as.numeric(as.character(fifaDataRaw$WeightMetric))
    fifaDataRaw$HeightMetric <- as.numeric(as.character(fifaDataRaw$HeightMetric))
    fifaDataRaw$HeadingAccuracy <- as.numeric(as.character(fifaDataRaw$HeadingAccuracy))

    str(fifaDataRaw)

    #Make Dataset Reactive
    fifaData <- reactive({
      fifaData=fifaDataRaw
    })
    


    ######################################################################################################
    ####Outputs####
    ######################################################################################################


    #####################################################
    #Question Which section of Performance....
    #Omit Goalkeepers and select Age and Performance Factors
    AgeStats <-  fifaDataRaw %>% filter(Position != "GK")
    AgeStats <- AgeStats[,c("Age","Overall","Crossing","Finishing","HeadingAccuracy",
                "ShortPassing","Volleys","Dribbling","Curve")]
    
    #AgeStats <- AgeStats %>% filter(Age < quantile(Age , 0.90 ))
    #Aggregate the mean of every ability by age
    AgeStats.agg <- aggregate(.~Age, AgeStats, FUN=median, na.rm=TRUE, na.action=NULL)
    #Output
    output$AgeStatsTable <- renderDataTable(
      AgeStats.agg
    )
    #Correlation matrix
    AgeStats.corr <-cor(AgeStats.agg)
    #Output
    output$AgeStatsCorr1 <- renderDataTable(
      AgeStats.corr
    )
    paletteHM1 = colorRampPalette(brewer.pal(3, "BuGn"))(20)
    #Output
    output$HeatmapCorr1 <- renderPlot(
      heatmap(x = AgeStats.corr, col = paletteHM1, symm = TRUE)
    )
    #print(AgeStats[order(-AgeStats$Age),])
    #Correlation Matrix only by age, splitting the dataframe and putting 
    #it back together as tibble in order to create the geom_point graph 
    correlationAge <- AgeStats.corr[,'Age']
    colAge <- colnames(AgeStats.corr)
    Age.corr <- tibble(colAge, correlationAge)
    #Output
    output$CorrelationGraph1 <- renderPlot(
    ggplot(data=Age.corr) +
      geom_point(aes(x=colAge, y=correlationAge)) + geom_hline(yintercept=0, color="blue", size=2)+
      theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1)) +
      xlab("Ability")+
      ylab("Corrleation Value")
    )
    #Output
    output$CorrelationGraph2 <- renderPlot(
    ggcorr(AgeStats.agg,label=TRUE,hjust=1,size=5)
    )
    #The mean of every ability by 'Overall' stat
    OveStats.agg <- aggregate(.~Overall, AgeStats, FUN=median, na.rm=TRUE, na.action=NULL)
    #Output
    output$OveStatsTable <- renderDataTable(
      OveStats.agg
    )
    #Correlation matrix and heatmap
    OveStats.corr <-cor(OveStats.agg)
    #Output
    output$OveStatsCorr1 <- renderDataTable(
      OveStats.corr
    )
    # #Output
    # paletteHM2 = colorRampPalette(brewer.pal(3, "BuGn"))(20)
    # output$HeatmapCorr2 <- renderPlot(
    # heatmap(x = OveStats.corr, col = paletteHM2, symm = TRUE)
    # )
    #Output
    output$CorrelationGraph3 <- renderPlot(
      ggcorr(OveStats.agg,label=TRUE,hjust=1,size=5)
    )
    print(OveStats.corr)
    #Correlation data by Overall and create tibble again in order to create the geom_point
    correlationOve <- OveStats.corr[,'Overall']
    col <- colnames(OveStats.corr)
    Ove.corr <- tibble(col, correlationOve)
    #Output
    output$CorrelationGraph4 <- renderPlot(
      ggplot(data=Ove.corr) +
        geom_point(aes(x=col, y=correlationOve)) + geom_hline(yintercept=0, color="blue", size=2)+
        theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1))+
        xlab("Ability")+
        ylab("Correlation Value")
    )
    #Output
    output$AgeOverall <- renderPlot(
    ggplot(AgeStats.agg,aes(x=Age,y=Overall))+
      geom_line()+
      geom_point(aes(x=AgeStats.agg[which.max(Overall),1],
                     y=AgeStats.agg[which.max(Overall),2]),
                 col="red",size=3,show.legend=TRUE)+
      geom_text(aes(label=ifelse(Overall==AgeStats.agg[which.max(Overall),2],
                                 round(Overall),'')),hjust=1,vjust=2,color="red",size=5,fontface="bold")+
      geom_text(aes(label=ifelse(Overall==AgeStats.agg[which.max(Overall),2],
                                 Age,'')),hjust=1,vjust=4,color="blue",size=5,fontface="bold")
    )
    
    
    ##########################################
    #Heading Performance
    #Add BMI. BMI is a relationship between Weight and Height
    headPerf <- fifaDataRaw[, c("HeadingAccuracy","WeightMetric","HeightMetric")]
    headPerf <- headPerf %>% mutate(BMI = as.numeric(WeightMetric)/(as.numeric(HeightMetric)/100)^2)
    #Output
    output$headingPerfTable <- renderDataTable(
      headPerf
    )
    headPerf.agg <- aggregate(.~HeadingAccuracy,headPerf,FUN=mean,na.rm=TRUE,na.action=NULL)
    #Output
    output$headingPerfCorr <- renderPlot(
      ggcorr(headPerf.agg,label=TRUE,hjust=1,size=5) 
    )
    #Output and gradient color
    output$headingPerf <- renderPlot({
      palHP <- wes_palette("Zissou1", 100, type = "continuous")
      ggplot(fifaData(), aes(x = WeightMetric, y = HeightMetric, fill = HeadingAccuracy)) +
        geom_tile() +
        scale_fill_gradientn(colours = palHP) +
        coord_equal()+
        xlab("Weight in kilogramms")+
        ylab("Height in meters")+
        geom_point(aes(x=input$weightM,y=input$heightM),size=3,colour="purple")
    })

    ###########################################
    #Fifa Data complete
    output$table <- renderDataTable(fifaData())

    ###########################################
    #Gegenüberstellungen
    output$gegenueberstellungen <- renderPlot({
      ggplot(fifaData() %>% filter(Age >= input$numAge),
             aes_string(x=input$x))+geom_smooth(aes_string(y=input$y))
    })
    
    #########################################################
    #How does Value and the overall rating correlate ?
    output$valueTable <- renderDataTable(
      datatable(fifaData()[,c("Name","Age","Nationality","Overall","Club","Value")]
      )
    )
    
    valueRat <- fifaDataRaw[,c("Name","Age","Nationality","Overall","Club","Value","Wage")]
    valueRat <- valueRat %>%
      mutate( topTen=
        case_when(
          Nationality == "Belgium" ~ TRUE,
          Nationality == "France" ~ TRUE,
          Nationality == "Brazil" ~TRUE,
          Nationality == "England" ~TRUE,
          Nationality == "Uruguay" ~TRUE,
          Nationality == "Croatia" ~TRUE,
          Nationality == "Portugal" ~TRUE,
          Nationality == "Spain" ~TRUE,
          Nationality == "Argentina" ~TRUE,
          Nationality == "Colombia" ~TRUE,
          TRUE ~ FALSE
          )
      )
    
    valueRatR <- reactive({
      valueRatR=valueRat
    })
    
    Value.agg <- aggregate(.~Overall, valueRat[,c("Overall","Value","Wage")], FUN=mean, na.rm=TRUE, na.action=NULL)
    output$corrGraphValue <- renderPlot(
      ggcorr(Value.agg,label=TRUE) 
    )
    
    output$ratingValue <- renderPlot({
      ggplot(valueRatR() %>% filter(Value>quantile(Value,input$valueQuant/100,na.rm= TRUE)),
             aes(x=Value,y=Overall,size=Wage,color=ifelse(topTen==TRUE,Nationality,"other")))+
        geom_point(alpha=0.5)+
        scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")+
        theme_ipsum()+
        labs(colour="Fifa Top Ten (2020)",size="Wage in thousands")+
        scale_x_continuous(labels=comma,trans='log10')
    })
    
    output$densGraph <- renderPlot(
      ggplot(
        valueRatR()
        %>%
        filter(
          case_when(
            input$selectValueWage == "Value" ~ Value>quantile(Value,input$valueQuant2/100,na.rm=TRUE),
            input$selectValueWage == "Wage" ~ Wage>quantile(Wage,input$valueQuant2/100,na.rm=TRUE)
          )
          )
        ,aes_string(input$selectValueWage))+
        geom_point(aes(y=Overall),alpha=0.5)+
        theme_ipsum()+
        geom_density_2d(aes(y=Overall),colour="red")+
        scale_x_continuous(labels=comma,trans='log10')
    )
    
    
    ###################################
    ##Stats on preferred position
    posData <- fifaDataRaw[,c("Position","Overall","Value","Crossing",
                              "Finishing","HeadingAccuracy","ShortPassing",
                              "Volleys","Dribbling","Curve"
                              )]
    
    #Adding count column for later use in aggregat
    posData <- posData %>% mutate(count=1)
    
    #count the occurences of each position
    posData.temp1 <- aggregate(count ~ Position, posData,sum,na.rm=TRUE)
    #aggregate the mean of each other column
    posData.temp2 <- aggregate(. ~ Position,posData,mean)
    #omit the empty positions
    posData.temp1 <- posData.temp1 %>% filter(Position != "")
    #set count column to null
    posData.temp2$count <- NULL
    #merging the aggregated dataframes
    posData.agg <- merge(posData.temp1,posData.temp2)
    
    posData.agg <- posData.agg %>%
      mutate(
        posCoordX =
          case_when(
            Position == "LF" ~ 20,
            Position == "LB" ~ 20,
            Position == "LCB" ~ 40,
            Position == "LCM" ~ 40,
            Position == "LAM" ~ 40,
            Position == "LDM" ~ 40,
            Position == "LM" ~ 20,
            Position == "LS" ~ 20,
            Position == "LW" ~ 20,
            Position == "LWB" ~ 20,
            Position == "RB" ~ 80,
            Position == "RWB" ~ 80,
            Position == "RCB" ~ 60,
            Position == "RCM" ~ 60,
            Position == "RAM" ~ 60,
            Position == "RDM" ~ 60,
            Position == "RM" ~ 80,
            Position == "RS" ~ 80,
            Position == "RF" ~ 80,
            Position == "RW" ~ 80,
            Position == "CF" ~ 50,
            Position == "CAM" ~ 50,
            Position == "CB" ~ 50,
            Position == "CDM" ~ 50,
            Position == "CM" ~ 50,
            Position == "ST" ~ 50,
            Position == "GK" ~ 50,
            TRUE ~ 0
          )
        ,
        posCoordY =
          case_when(
            Position == "LF" ~ 160,
            Position == "LB" ~ 80,
            Position == "LCB" ~ 60,
            Position == "LCM" ~ 100,
            Position == "LAM" ~ 120,
            Position == "LDM" ~ 80,
            Position == "LM" ~ 100,
            Position == "LS" ~ 140,
            Position == "LW" ~ 150,
            Position == "LWB" ~ 60,
            Position == "RB" ~ 80,
            Position == "RWB" ~ 60,
            Position == "RCB" ~ 80,
            Position == "RCM" ~ 100,
            Position == "RAM" ~ 120,
            Position == "RDM" ~ 60,
            Position == "RM" ~ 100,
            Position == "RS" ~ 140,
            Position == "RF" ~ 160,
            Position == "RW" ~ 150,
            Position == "CF" ~ 160,
            Position == "CAM" ~ 120,
            Position == "CB" ~ 80,
            Position == "CDM" ~ 80,
            Position == "CM" ~ 100,
            Position == "ST" ~ 160,
            Position == "GK" ~ 20,
            TRUE ~ 0
          )
      )
    
    # view(posData.agg)

    field <- readPNG("data/field2.png")
    # grid <- rasterGrob(field, width=unit(1,"npc"), height=unit(1,"npc"))
    grid <- rasterGrob(field, interpolate=TRUE, height = 1, width = 1)
    
    # ggplot(posData.agg, aes(posCoordX,posCoordY,color=Position)) +
    #   annotation_custom(grid) +
    #   geom_point(aes(size=Overall)) +
    #   scale_x_continuous(expand=c(0,0), lim=c(0,100)) +
    #   scale_y_continuous(expand=c(0,0), lim=c(0,200)) +
    #   theme_void() +
    #   theme(aspect.ratio = nrow(field)/ncol(field))+
    #   scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")+
    #   guides(color=guide_legend(order=1),
    #          size=guide_legend(order=2))
    
    #Make aggregated Data reactive
    posDataR <- reactive({
      posDataR=posData.agg
    })
    
    #make input reactive
    fieldChoice <- reactive(input$fieldChoice)
    
    #make graph reactive for later if statement (scaling of values)
    graph <- reactive({ 
      ggplot(posDataR(), aes(posCoordX,posCoordY,color=Position)) +
        annotation_custom(grid) +
        geom_point(aes_string(size=fieldChoice())) +
        scale_x_continuous(expand=c(0,0), lim=c(0,100)) +
        scale_y_continuous(expand=c(0,0), lim=c(0,200)) +
        theme_void() +
        theme(aspect.ratio = nrow(field)/ncol(field),legend.key.size = unit(1, "cm"),legend.key.width = unit(1,"cm"))+
        # scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")+
        guides(color=guide_legend(order=1),
               size=guide_legend(order=2))
    })
    
    output$avgStatsField <- renderPlot({
      if(fieldChoice()=="Value"){
        graph()+
          scale_size(range = c(0.1, 20),labels=comma)
      }else{
        graph()+
          scale_size(range = c(0.1, 15))
      }
    })
    
    #Input Position
    choicePosR <- reactive({
     choicePosR=input$choicePosition 
    })
    
    #Data for Spiderchart and Rownames
    spidDat <- 
      posData.agg[,c("Overall","Crossing","Finishing","HeadingAccuracy","ShortPassing","Dribbling","Curve")]
    spidDat
    rownames(spidDat) <- posData.agg$Position
    spidDat

    #First two lines have to have minimun and maximum values for spiderchart
    spidDat <- rbind(rep(100,7), rep(0,7) ,spidDat)
    
    spidDatR <- reactive({
      spidDatR = spidDat
    })
    
    colors_borderSpid <- brewer.pal(3, "Set3")
    colors_inSpid <- alpha(colors_borderSpid,0.5)
    
    output$posSpider <- renderPlot({
      radarchart(spidDatR()%>%filter(rownames(spidDatR()) %in% c(1,2,choicePosR())), axistype=1 ,
                #custom polygon
                pcol=colors_borderSpid , pfcol=colors_inSpid , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,100), cglwd=0.8,
                #custom labels
                vlcex=0.8)
                # Add a legend
                legend(x=-1.9, y=0, legend = choicePosR(), bty = "n", pch=20 , col=colors_borderSpid , text.col = "grey", cex=1.2, pt.cex=3)
    })
    
    ################################################
    ######--- Prediction Value
    # Create training and test data
    
    set.seed(69)
    forecData <- fifaDataRaw[,c("Age","Value","Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling","Curve","HeightMetric","WeightMetric")]
    ##Remove Value = NULL
    forecData <- forecData %>% filter(Value!="")
    ##Remove outliers
    outliers <- boxplot(forecData$Value, plot = TRUE)$out
    x <- forecData
    forecData <- x[-which(x$Value %in% outliers),]
    
    #Train and Test Data
    train.data<- sample_frac(forecData,0.7) #select 70% random samples
    test.data <- setdiff(forecData,train.data)
    
    #Create Correlation Table
    forecData.corr<- cor(forecData,use = "complete.obs")
    # view(forecData.corr[,c("Value")])
    
    #View Correlations
    ggcorr(forecData.corr,label=TRUE)
    
    #strating with the most correlated variable as predictor
    #Wage would be the the most correlated. but since wage and value are naturally 
    #dependend on each other (since the more value, the more wage) 
    #and it would be meaningless to predict ones value from the wage we go from the 2nd highest
    lm1 <- lm(Value~ShortPassing,data=train.data )
    summary(lm1)
    ##All Coefficients are positive
    
    lm2 <- lm(Value~ShortPassing+Curve,data=train.data)
    summary(lm2)
    ##All Coefficients are positive
    
    lm3 <- lm(Value~ShortPassing+Curve+Dribbling,data=train.data)
    summary(lm3)
    ##The Dribbling Coefficient is negative. Which means greater value effects the value negativels
    
    lm4 <- lm(Value~ShortPassing+Curve+Dribbling+Volleys,data=train.data)
    summary(lm4)
    ##Dribbling Coeficcient is still negative
    
    lm5 <- lm(Value~ShortPassing+Curve+Dribbling+Volleys+Crossing,data=train.data)
    summary(lm5)
    ##Crossing AND Dribbling are negative Coefficients
    
    #All Data - this is generally a bad idea due to data noise
    lm.tot <- lm(Value~.,data=train.data)
    summary(lm.tot)
    ##Here the neagtive coefficients are Crossing and HeadingAccuracy
    
    #playing around with the least correlated variables and the variable which were a negative coefficient earlier
    lm.red <- lm(Value~.-HeightMetric-WeightMetric-Age-Dribbling-Crossing-Finishing,data=train.data)
    summary(lm.red)
    ##Here HeadingAccuracy,ShortPassing,Volleys and Curve are coefficient and are positive
    
    #let's cpompare the models obtained, in a structured way
    anova(lm1, lm2, lm3, lm4,lm5, lm.red , lm.tot)
    
    #Conlcusion: 
    #lm.tot is the best but has way too many parameters to be given in our interactive UI and includes the Wage and has negative coefficients
    #lm.red is the second best, with the least amount of variables and no negative coeffcients
    #lm.red has the coefficients ShortPassing,Volleys and Curve
    lm <- lm(Value~HeadingAccuracy+ShortPassing+Volleys+Curve,data=train.data)
    
    #Set the predictions
    pred1 <- predict(lm,data.frame(HeadingAccuracy=test.data$HeadingAccuracy,ShortPassing=test.data$ShortPassing,Volleys=test.data$Volleys,Curve=test.data$Curve), interval="prediction")
    pred2 <- predict(lm,data.frame(HeadingAccuracy=test.data$HeadingAccuracy,ShortPassing=test.data$ShortPassing,Volleys=test.data$Volleys,Curve=test.data$Curve), interval="confidence")
    
    pred1.df <- data.frame(HeadingAccuracy=test.data$HeadingAccuracy,ShortPassing=test.data$ShortPassing,Volleys=test.data$Volleys,Curve=test.data$Curve,prediction=pred1) 
    pred2.df <- data.frame(HeadingAccuracy=test.data$HeadingAccuracy,ShortPassing=test.data$ShortPassing,Volleys=test.data$Volleys,Curve=test.data$Curve,prediction=pred2) 
    
    pred1.df
    pred2.df
  
    pred_test <- predict(lm,data.frame(HeadingAccuracy=100,ShortPassing=100,Volleys=100,Curve=100), interval="prediction")
    pred_test[1]
    #Prediction Output
    output$forec <- renderUI({
      pred<-predict(lm,data.frame(HeadingAccuracy=as.numeric(input$pHeadingAccuracy),ShortPassing=as.numeric(input$pShortPassing),Volleys=as.numeric(input$pVolleys),Curve= as.numeric(input$pCurve)), interval="prediction")
      tagList(
        tags$p("Your estimated Value is: "),
        tags$h2(format(round(as.numeric(pred[1])), nsmall=0, big.mark="'"))
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

