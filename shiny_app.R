library(shiny)
library(shinythemes)
library(ISLR)
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071) 
library(leaflet)
library(markdown)
library(randomForest)
library(MLmetrics)
library(knitr)
library(tidyverse)
library(owmr)
library(ggmap)
library(corrplot)
library(RColorBrewer)
library(reticulate)
library(httr)
library(jsonlite)
library(rpart)
library(forecast)

getwd()
setwd("E:/sem6/EDA/project")
df<-read.csv("Crop_recommendation.csv")
df1 <- df[, -which(names(df) == "label")]
price_df<-read.csv("kalimati_tarkari_dataset.csv")


var<-c("Groundnut - Kharif"= "p21","Groundnut - Rabi"= "p20","Groundnut - Total"= "p19","Castorseed - Kharif"= "p18","Sesamum - Kharif"= "p1","Nigerseed - Kharif"= "p2","Soyabean - Kharif"= "p3","Sunflower - Kharif"= "p4","Sunflower - Rabi"= "p5","Sunflower - Total"= "p6","R & M - Rabi"= "p7","Linseed - Rabi"= "p8","Safflower - Rabi"= "p9","Total_Oilseeds_Kharif"= "p10","Total Oilseeds - Rabi"= "p11","Total Oilseeds - Total"= "p12","Sugarcane"= "p13","Cotton"= "p14","Jute"= "p15","Mesta"= "p16","Jute & Mesta" = "p17")

ui <- fluidPage(
  navbarPage("Crop!",
             tabPanel(title="Crop Distribution", 
                      tabsetPanel(
                        tabPanel("Crop Data",
                                 downloadButton("downloadData", "Download CSV"),
                                 DT::dataTableOutput("cont")
                        ),
                        
                        tabPanel("Price data",
                                 downloadButton("downloadData_price", "Download CSV"),
                                 DT::dataTableOutput("price_cont")
                        ) 
                      )
             ),
             
             tabPanel("Summary",
                        verbatimTextOutput("summary"),
                        verbatimTextOutput("str"),
                        verbatimTextOutput("missing")
             ),
             
             navbarMenu("EDA",
                          tabPanel("Correlation"
                                   ,mainPanel("Correlation plot",
                                              fluidRow(
                                                  plotOutput("corr_plot")
                                              )
                                   )
                          ),
                          tabPanel("histogram"
                                   ,mainPanel("histogram",
                                              fluidRow(
                                                plotOutput("hist_plot")
                                              )
                                   )
                          ),
                          tabPanel("bar - chart",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("column", "Select a column:", choices = colnames(df1))
                                     ),
                                     mainPanel(
                                       plotOutput("plot")
                                     )
                                   )
                          )
                          
             ),
             
             navbarMenu("Models",
                        tabPanel("Support Vector Machine",
                                 mainPanel("Support Vector Machine",
                                           verbatimTextOutput("svm")
                                 )
                                 
                        ),
                        tabPanel("Random Forest",
                                 mainPanel("Random Forest",
                                           verbatimTextOutput("ran_for")
                                 )
                                 
                        ),
                        tabPanel("Naive Bayes",
                                 mainPanel("Naive Bayes",
                                           verbatimTextOutput("naive_bayes")
                                 )
                                 
                        ),
                        tabPanel("Decision Tree",
                                 mainPanel("Decision Tree",
                                           verbatimTextOutput("dt")
                                 )
                                 
                        )
                        
                        
             ),
             tabPanel("Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("N", "N:", 0, 140, 73),
                          sliderInput("P", "P:", 5, 145, 50),
                          sliderInput("K", "K:", 5, 205, 23),
                          sliderInput("temp", "Temperature:", 8.82, 41.94, 23.34),
                          sliderInput("humidity", "humidity:", 50.05, 94.96, 63.34),
                          sliderInput("ph", "ph:", 5.005, 7.996, 6.34),
                          sliderInput("Rainfall", "Rainfall:", 60.65, 298.56, 133.34)
                        ),
                        mainPanel(
                          verbatimTextOutput("contents")
                        )
                      )
             ),tabPanel("Crop Diseases",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("file", "Choose an image file"),
                            br()
                          ),
                          mainPanel(
                            verbatimTextOutput("class_output")
                          )
                        )
             ),
             tabPanel("Forecasting",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "Catgory_Name", 
                            "Category Name",
                            unique(price_df$Commodity ))
                        ),
                        mainPanel(
                          plotOutput("plot_forecast")
                        )
                      )
             )
  )
  
)


server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = function() {
      data<-df
      paste("crop-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data<-df
      write.csv(data, file)
    }
  )
  output$downloadData_price <- downloadHandler(
    filename = function() {
      data<-price_df
      paste("price-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data<-price_df
      write.csv(data, file)
    }
  )
  output$summary <- renderPrint({
    summary(df)
  })
  output$cont <- DT::renderDataTable({
    df
  })
  output$price_cont <- DT::renderDataTable({
    price_df
  })
  output$str <- renderPrint({
    str(df)
  })
  output$missing <- renderPrint({
    knitr::kable(sapply(df, function(x) sum(is.na(x))), col.names = c("Missing Value Count"))
  })
  output$corr_plot <- renderPlot({
    # Assuming 'df' is a data.frame object
    df1 <- df[, -which(names(df) == "label")]
    numeric_df <- as.data.frame(lapply(df1, as.numeric))
    corr_mat <- cor(numeric_df)
    
    # Create a heatmap of the correlation matrix
    corrplot(corr_mat, method="color", type="full", tl.col="black",
             tl.srt=45, tl.cex=0.7, tl.offset=0.5, addCoef.col="black",
             number.cex=0.8, cl.cex=0.8, col=colorRampPalette(c("blue","white","red"))(200),
             title="Correlation Matrix", mar=c(0,0,2,0))
  })
  output$hist_plot <- renderPlot({
    # Load data
    df1 <- df[, -which(names(df) == "label")]
    # Define column names
    columns <- colnames(df1)
    
    # Plot histogram for each column
    plots <- lapply(columns, function(column) {
      ggplot(df, aes(x = !!sym(column))) +
        geom_histogram(fill = "steelblue", color = "white")+
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Combine plots into a grid
    gridExtra::grid.arrange(grobs = plots, ncol = 3)
  })
  output$contents <- renderPrint({
    set.seed(1000)
    trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
    training_data <- df[ trainIndex,]
    testing_data  <- df[-trainIndex,]
    pr<- data.frame (
      N = c(input$N),
      P = c(input$P),
      K = c(input$K),
      temperature=c(input$temp),
      humidity=c(input$humidity),
      ph=c(input$ph),
      rainfall=c(input$Rainfall)
    )
    
    rf <- randomForest(factor(label)~., data = training_data)
    pred.rf <- predict(rf, pr,type ="response")
    if(length(pr)>0){
      paste(pred.rf)
    }
  })
  output$svm <- renderPrint({
    df<-read.csv("Crop_recommendation.csv")
    set.seed(1000)
    trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
    training_data <- df[ trainIndex,]
    testing_data  <- df[-trainIndex,]
    learn_svm <- svm(factor(label)~.,data=training_data) 
    predict_svm <- predict(learn_svm, testing_data,type ="response") 
    cm_svm <- confusionMatrix(as.factor(testing_data$label),as.factor(predict_svm),mode = "everything",positive='1')
    cm_svm
    
  })
  output$ran_for <- renderPrint({
    df <- read.csv("Crop_recommendation.csv")
    set.seed(1000)
    trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
    training_data <- df[trainIndex, ]
    testing_data <- df[-trainIndex, ]
    
    rf <- randomForest(factor(label) ~ ., data = training_data)
    
    pred.rf <- predict(rf, testing_data, type = "response")
    cm_rf <- confusionMatrix(as.factor(testing_data$label), as.factor(pred.rf), mode = "everything", positive = '1')
    
    print(cm_rf)
    
  })
  output$naive_bayes <- renderPrint({
    df <- read.csv("Crop_recommendation.csv")
    set.seed(1000)
    trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
    training_data <- df[trainIndex, ]
    testing_data <- df[-trainIndex, ]
    
    nb <- naiveBayes(factor(label) ~ ., data = training_data)
    
    pred.nb <- predict(nb, testing_data, type = "class")
    cm_nb <- confusionMatrix(as.factor(testing_data$label), as.factor(pred.nb), mode = "everything", positive = '1')
    
    print(cm_nb)
    
  })
  output$dt<- renderPrint({
    df <- read.csv("Crop_recommendation.csv")
    set.seed(1000)
    trainIndex <- createDataPartition(df$label, p = 0.7, list = FALSE, times = 1)
    training_data <- df[trainIndex, ]
    testing_data <- df[-trainIndex, ]
    
    dt <- rpart(factor(label) ~ ., data = training_data)
    
    pred.dt <- predict(dt, testing_data, type = "class")
    cm_dt <- confusionMatrix(as.factor(testing_data$label), as.factor(pred.dt), mode = "everything", positive = '1')
    
    print(cm_dt)
    
  })
  output$plot <- renderPlot({
    ggplot(df, aes(x = label, y = .data[[input$column]], fill = label)) +
      geom_bar(stat = "identity", position = "dodge",fill = "steelblue") +
      labs(x = "label", y = input$column) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  })
  output$plot_forecast <- renderPlot({
    inname<-input$Catgory_Name
    data<-read.csv("kalimati_tarkari_dataset.csv")
    tomato_data <- subset(data, Commodity == inname)
    tomato_data$Date <- as.Date(tomato_data$Date, format = "%d-%m-%Y")
    ts_data <- ts(tomato_data$Average, start = c(2013, 6), end = c(2021, 12), frequency = 12)
    
    ts_data
    
    arima_model <- auto.arima(ts_data)
    forecast_data <- forecast(arima_model, h = 12)
    forecast_data
    
    # Plot the past data
    autoplot(forecast_data) +
      labs(title = "prices over time", y = "Price (NPR)")
    
  })
  output$mymap <- renderLeaflet({
    df<-read.csv("crop.csv")
    if(input$selectMap == 'p10')
    {   
      y <- df$Total_Oilseeds_Kharif
      radius <- sqrt(df$Total_Oilseeds_Kharif)*800
      pal <- colorNumeric(
        palette = "Dark2",
        domain = df$Total_Oilseeds_Kharif
      )
    }
    m <- leaflet(data =df ,height = 400) %>%setView(lng= 78.0419,lat = 17.1750,zoom=5)%>%
      addTiles()%>%
      addCircles(lng,lat,weight =1,radius = radius)
    m
  })
  observeEvent(input$file, {
    file_path <- input$file$datapath
    url <- "http://localhost:5000/file-info"
    body <- list(file_path = file_path)
    response <- POST(url, body = body)
    response_json <- content(response, as = "text")
    response_obj <- fromJSON(response_json)
    output$class_output <- renderText(
      paste("Class: ", response_obj$class1, "\t\tProb: ", response_obj$prob1, "\n",
            "Class: ", response_obj$class2, "\tProb: ", response_obj$prob2, "\n",
            "Class: ", response_obj$class3, "\tProb: ", response_obj$prob3))
   
  })
  

}

shinyApp(ui, server)

