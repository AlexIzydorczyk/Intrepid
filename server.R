library(shiny)
library(datasets)
library(googleVis)
library(Quandl)
library(xts)
library(shinyIncubator)
library(randomForest)
library(e1071)

Quandl.auth('a9Q3uQjauWe2f41Nhsuo')

#oil <- read.csv('http://www.quandl.com/api/v1/datasets/WORLDBANK/WLD_CRUDE_WTI.csv?&trim_start=1982-01-31&trim_end=2013-08-31&sort_order=asc', colClasses=c('Date'='Date'))
#natural_gas <- read.csv('http://www.quandl.com/api/v1/datasets/IMF/PNGASUS_USD.csv?&trim_start=1991-01-31&trim_end=2013-07-31&sort_order=asc', colClasses=c('Date'='Date'))
#gold <- read.csv('http://www.quandl.com/api/v1/datasets/BUNDESBANK/BBK01_WT5511.csv?&trim_start=1968-04-01&trim_end=2013-09-06&collapse=monthly&sort_order=asc', colClasses=c('Date'='Date'))
validateDates = function(listofXTS){
  
  merged_data <- listofXTS[[1]]
  for (i in c(2:length(listofXTS))){
    merged_data <- merge(merged_data,listofXTS[[i]])    
  }
  
  #model_data <- as.data.frame(Date=time(merged_data), merged_data)
  model_data <- merged_data
  model_data <- trimDates(model_data)
  dates <- as.character(time(model_data))
  #print(class(model_data))
  model_data <- data.frame(Date=dates, as.data.frame(model_data))
  #model_data <- data.frame(date=time(model_data))
  return(model_data)  
}

trimDates = function(model_data){
  model_data <- model_data[complete.cases(model_data),]
  return(model_data)  
}

xts_dataframe = function(model_data){
  d <- data.frame(Date=index(model_data))
  for (i in c(1:length(model_data))){
    d <- data.frame(d, model_data[,i])
  }
  return(d)
}


shinyServer(function(input, output) {
  
  observe({
    # Don't do anything if the button has never been pressed
    if (input$action == 0)
      return()
    output$downloadData <- downloadHandler(
      filename = function() { paste(input$dataset, '.csv', sep='') },
      content = function(file) {
        write.csv(formulateData()[[1]], file)
        #if(!is.null(datasetInputX1)){
        # write.csv(datasetInputX1(), file)
        #}
      }
    )
    
    isolate({
      #print("Hello World")
      #print(head(formulateData()[[1]][,c(2:length(formulateData()[[1]]))]))
      
      x_len <- c(2:length(formulateData()[[1]]))
      to_model = formulateData()[[1]][,x_len]
      colnames(to_model)[1] <- "y"
      #print(to_model)
      #print(length(to_model[,1]))
      
      data_length <- length(to_model[,1])
      train_end <- (floor(splitPoint()*length(to_model[,1])))
      
      
      #fit <<- lm(formulateData()[[1]][,2] ~ formulateData()[[1]][,x_len], data=formulateData()[[2]])
      if (MLalgo() == "lm"){
        fit <<- lm(y~., data=to_model[1:train_end,])
      }
      if (MLalgo() == "rf"){
        fit <<- randomForest(y~., data=to_model[1:train_end,], ntree=500)
      }
      if (MLalgo() == "svm"){
        fit <<- svm(y~., data=to_model[1:train_end,])
      }
      
      #print(to_model[,-1])
      #fit <<- lm(to_model[1:train_end,1]~., data=to_model[1:train_end,])
      
      testingdata = to_model[(train_end+1):data_length,]
      
      #namesofheadings = names(to_model)
      #testingdata <- data.frame()
      #print(testingdata)
      preds <- predict(fit, newdata=testingdata)
      #print(length(testingdata))
      #print(length(preds))
      
      print(fit)
      #print(splitPoint())
      
      

      
      output$results_summary <- renderPrint({
        summary(fit)
      })
      output$table_predictions <- renderTable({
        predsvsactual <<- data.frame(Y=to_model[(train_end+1):data_length,1], Predictions=preds)
        #print(row.names(predsvsactual))
        predsvsactual
      })
      
      
      output$results_graph <- renderGvis({
        gvisScatterChart(predsvsactual, options=list(title="Actual Values versus Prediction", vAxis="{title:'Predictions'}",
                                                     hAxis="{title:'Actual'}"))
      })
      print(MLalgo())
      
      #predict/evaluate classifier now
      #preds <- predict(model=fit, data=to_model[(train_end+1):data_length,])
      #print(preds)
      
    })
  })
  
  output$caption1 <- renderText({
    "Y:"
  })
  
  
  MLalgo <- reactive({
    input$choose_model
  })
  
  splitPoint <- reactive({
    input$splitperc/100
  })
  
  datasetInput <- reactive({
      (Quandl(input$dataset, type='xts'))           
  })
  
  #########################################
  # Search functionality!
  
  dataqueryInput <- reactive({
    if (input$dataquery != ""){
      Quandl.search(input$dataquery, silent=T)
      
    }
  })
  
  output$search1 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[1]]$name, " | Code: ", dataqueryInput()[[1]]$code, 
                  " | Description: ", dataqueryInput()[[1]]$desc, " | Frequency: ", 
                  dataqueryInput()[[1]]$frequency
                  #" | Column Names: ",
                  #dataqueryInput()[[1]]$column_names
        ))
      
    }
  })
  
  output$search2 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[2]]$name, " | Code: ", dataqueryInput()[[2]]$code, 
                  " | Description: ", dataqueryInput()[[2]]$desc, " | Frequency: ",
                  dataqueryInput()[[2]]$frequency))
      
    }
  })
  
  output$search3 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[3]]$name, " | Code: ", dataqueryInput()[[3]]$code, 
                  " | Description: ", dataqueryInput()[[3]]$desc, " | Frequency: ",
                  dataqueryInput()[[3]]$frequency))
      
    }
  })
  
  output$search4 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[4]]$name, " | Code: ", dataqueryInput()[[4]]$code, 
                  " | Description: ", dataqueryInput()[[4]]$desc, " | Frequency: ",
                  dataqueryInput()[[4]]$frequency))
      
    }
  })
  
  output$search5 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[5]]$name, " | Code: ", dataqueryInput()[[5]]$code, 
                  " | Description: ", dataqueryInput()[[5]]$desc, " | Frequency: ",
                  dataqueryInput()[[5]]$frequency))
      
    }
  })
  output$search6 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[6]]$name, " | Code: ", dataqueryInput()[[6]]$code, 
                  " | Description: ", dataqueryInput()[[6]]$desc, " | Frequency: ",
                  dataqueryInput()[[6]]$frequency))
      
    }
  })
  output$search7 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[7]]$name, " | Code: ", dataqueryInput()[[7]]$code, 
                  " | Description: ", dataqueryInput()[[7]]$desc, " | Frequency: ",
                  dataqueryInput()[[7]]$frequency))
      
    }
  })
  output$search8 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[8]]$name, " | Code: ", dataqueryInput()[[8]]$code, 
                  " | Description: ", dataqueryInput()[[8]]$desc, " | Frequency: ",
                  dataqueryInput()[[8]]$frequency))
      
    }
  })
  output$search9 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[9]]$name, " | Code: ", dataqueryInput()[[9]]$code, 
                  " | Description: ", dataqueryInput()[[9]]$desc, " | Frequency: ",
                  dataqueryInput()[[9]]$frequency))
      
    }
  })
  output$search10 <- renderPrint({
    if (input$dataquery != ""){
        cat(paste("Name: ", dataqueryInput()[[10]]$name, " | Code: ", dataqueryInput()[[10]]$code, 
                  " | Description: ", dataqueryInput()[[10]]$desc, " | Frequency: ",
                  dataqueryInput()[[10]]$frequency))
      
    }
  })
  
  ##################### ^^^^^^ SEARCH ^^^^^^ #######################
  
  
  
  datasetInputX1 <- reactive({
    if (input$p1 != ""){
      Quandl(input$p1, type='xts')
    }
    
  })
  
  datasetInputX2 <- reactive({
    if (input$p2 != ""){
      Quandl(input$p2, type='xts')
    }
    
  })
  
  datasetInputX3 <- reactive({
    if (input$p3 != ""){
      Quandl(input$p3, type='xts')
    }
    
  })
  
  datasetInputX4 <- reactive({
    if (input$p4 != ""){
      Quandl(input$p4, type='xts')
    }
    
  })
  
  datasetInputX5 <- reactive({
    if (input$p5 != ""){
      Quandl(input$p5, type='xts')
    }
    
  })
  

  

  output$x1 <- renderUI({
    if (input$numberOfX >= 1){
      textInput("p1", "X1")
    }
    })
  output$x2 <- renderUI({
    if (input$numberOfX >= 2){
      textInput("p2", "X2")
    }
  })
  output$x3 <- renderUI({
    if (input$numberOfX >= 3){
      textInput("p3", "X3")
    }
  }) 
  output$x4 <- renderUI({
    if (input$numberOfX >= 4){
      textInput("p4", "X4")
    }
  }) 
  output$x5 <- renderUI({
    if (input$numberOfX >= 5){
      textInput("p5", "X5")
    }
  })
  
  output$column_namesX1 <- renderUI({
    if (input$p1 != ""){
      
      selectInput("headingsX1","Choose a factor:", choices = names(datasetInputX1()))
    }
  })
  
  output$column_namesX2 <- renderUI({
    if (input$p2 != ""){
      
      selectInput("headingsX2","Choose a factor:", choices = names(datasetInputX2()))
    }
  })
  
  output$column_namesX3 <- renderUI({
    if (input$p3 != ""){
      
      selectInput("headingsX3","Choose a factor:", choices = names(datasetInputX3()))
    }
  })
  
  output$column_namesX4 <- renderUI({
    if (input$p4 != ""){
      
      selectInput("headingsX4","Choose a factor:", choices = names(datasetInputX4()))
    }
  })
  
  output$column_namesX5 <- renderUI({
    if (input$p5 != ""){
      
      selectInput("headingsX5","Choose a factor:", choices = names(datasetInputX5()))
    }
  })
  
  
  #Select the factor for Y
  
  output$column_namesY <- renderUI({
    if (input$dataset != ""){
      
      selectInput("headingsY","Choose a factor:", choices = names(datasetInput()))
    }
  })
  
  output$LagsY <- renderUI({
    if (input$dataset != ""){
      numericInput("nLagsY", "Number of Lags for Predicted", 0)
    }    
  })
  output$cX1 <- renderUI({
    if (input$p1 != ""){
      checkboxInput("usex1", "Use Current Predictor #1", 1)
    }  
  })
  output$cX2 <- renderUI({
    if (input$p2 != ""){
      checkboxInput("usex2", "Use Current Predictor #2", 1)
    }  
  })
  output$cX3 <- renderUI({
    if (input$p3 != ""){
      checkboxInput("usex3", "Use Current Predictor #3", 1)
    }  
  })
  output$cX4 <- renderUI({
    if (input$p4 != ""){
      checkboxInput("usex4", "Use Current Predictor #4", 1)
    }  
  })
  output$cX5 <- renderUI({
    if (input$p5 != ""){
      checkboxInput("usex5", "Use Current Predictor #5", 1)
    }  
  })
  
  uX1 <- reactive({
    input$usex1
  })
  uX2 <- reactive({
    input$usex2
  })
  uX3 <- reactive({
    input$usex3
  })
  uX4 <- reactive({
    input$usex4
  })
  uX5 <- reactive({
    input$usex5
  })
  
  
  
  output$LagsX1 <- renderUI({
    if (input$p1 != ""){
      numericInput("nLagsX1", "Number of Lags for Predictor #1", 0)
    }      
  })
  output$LagsX2 <- renderUI({
    if (input$p2 != ""){
      numericInput("nLagsX2", "Number of Lags for Predictor #2", 0)
    }      
  })
  output$LagsX3 <- renderUI({
    if (input$p3 != ""){
      numericInput("nLagsX3", "Number of Lags for Predictor #3", 0)
    }      
  })
  output$LagsX4 <- renderUI({
    if (input$p3 != ""){
      numericInput("nLagsX4", "Number of Lags for Predictor #4", 0)
    }      
  })
  output$LagsX5 <- renderUI({
    if (input$p3 != ""){
      numericInput("nLagsX5", "Number of Lags for Predictor #5", 0)
    }      
  })
  
  lagYfactor <<- reactive({
    v <- input$nLagsY
    v
  })
  
  lagX1factor <<- reactive({
    v <- input$nLagsX1
    v
  })
  lagX2factor <<- reactive({
    v <- input$nLagsX2
    v
  })
  lagX3factor <<- reactive({
    v <- input$nLagsX3
    v
  })
  lagX4factor <<- reactive({
    v <- input$nLagsX4
    v
  })
  lagX5factor <<- reactive({
    v <- input$nLagsX5
    v
  })
  
  
  
  Yfactor <<- reactive({
    input$headingsY
    v <- input$headingsY 
    n <- names(datasetInput())
    datasetInput()[,which(n == v)]
  })
  
  X1factor <<- reactive({
    v <- input$headingsX1 
    n <- names(datasetInputX1())
    datasetInputX1()[,which(n == v)]
  })
  
  X2factor <<- reactive({
    v <- input$headingsX2
    n <- names(datasetInputX2())
    datasetInputX2()[,which(n == v)]
  })
  
  X3factor <<- reactive({
    v <- input$headingsX3
    n <- names(datasetInputX3())
    datasetInputX3()[,which(n == v)]
  })
  
  X4factor <<- reactive({
    v <- input$headingsX4
    n <- names(datasetInputX4())
    datasetInputX4()[,which(n == v)]
  })
  
  X5factor <<- reactive({
    v <- input$headingsX5 
    n <- names(datasetInputX5())
    datasetInputX5()[,which(n == v)]
  })
  
  output$view <- renderTable({
    head(formulateData()[[2]])
  })
  
  output$table_preprocess <- renderTable({
    formulateData()[[1]]   
  })
  
  output$graph <- renderGvis({
    if (is.na(formulateData())==F){
      
      gvisAnnotatedTimeLine(formulateData()[[2]], datevar="Date",
                            numvar="Value", idvar="Type",
                            options=list(
                              colors="['blue', 'lightblue']",
                              width=600, height=400, scaleColumns='[0]',
                              scaleType='allmaximized')
      )
    }
  })
  

  
  formulateData <- reactive({
    if (input$ready == T) {
        #print(head(Yfactor()))
        #print(head(lag(Yfactor()), k=1))
        
        lagsy = list()
        if (!is.null(lagYfactor())){
        if (lagYfactor() != 0){
          for (i in c(1:lagYfactor())){
            lagsy[[i]] <- lag(Yfactor(), k=i)
            colnames(lagsy[[i]]) = paste("L", i, colnames(lagsy[[i]]), sep="")
          }
        }
        }
        lagsx1 = list()
        if (!is.null(lagX1factor())){
          if (lagX1factor() != 0){
            for (i in c(1:lagX1factor())){
              lagsx1[[i]] <- lag(X1factor(), k=i)
              colnames(lagsx1[[i]]) = paste("L", i, colnames(lagsx1[[i]]), sep="")
            }
          }
        }
        lagsx2 = list()
        if (!is.null(lagX2factor())){
          if (lagX2factor() != 0){
            for (i in c(1:lagX2factor())){
              lagsx2[[i]] <- lag(X2factor(), k=i)
              colnames(lagsx2[[i]]) = paste("L", i, colnames(lagsx2[[i]]), sep="")
            }
          }
        }
        lagsx3 = list()
        if (!is.null(lagX3factor())){
          if (lagX3factor() != 0){
            for (i in c(1:lagX3factor())){
              lagsx3[[i]] <- lag(X3factor(), k=i)
              colnames(lagsx3[[i]]) = paste("L", i, colnames(lagsx3[[i]]), sep="")
            }
          }
        }
        lagsx4 = list()
        if (!is.null(lagX4factor())){
          if (lagX4factor() != 0){
            for (i in c(1:lagX4factor())){
              lagsx4[[i]] <- lag(X4factor(), k=i)
              colnames(lagsx4[[i]]) = paste("L", i, colnames(lagsx4[[i]]), sep="")
            }
          }
        }
        lagsx5 = list()
        if (!is.null(lagX5factor())){
          if (lagX5factor() != 0){
            for (i in c(1:lagX5factor())){
              lagsx5[[i]] <- lag(X5factor(), k=i)
              colnames(lagsx5[[i]]) = paste("L", i, colnames(lagsx5[[i]]), sep="")
            }
          }
        }
        

      
        #if (input$numberOfX == 1){predata <- list(Yfactor(), X1factor())}
      
        #if (input$numberOfX == 2){predata <- list(Yfactor(), X1factor(), X2factor())}
      
        #if (input$numberOfX == 3){predata <- list(Yfactor(), X1factor(), X2factor(), X3factor())}
      
        #if (input$numberOfX == 4){predata <- list(Yfactor(), X1factor(), X2factor(), X3factor(), X4factor())}
        currents = list()
        
        if (!is.null(uX1())){
        if (uX1() == T){
          currents <- c(currents, list(X1factor()))
        }else{
        }} else{
          currents <- c(currents, list(X1factor()))
        }
        if (!is.null(uX2())){
          if (uX2() == T){
            currents <- c(currents, list(X2factor()))
          }else{
          }} else{
            currents <- c(currents, list(X2factor()))
          }
        if (!is.null(uX3())){
          if (uX3() == T){
            currents <- c(currents, list(X3factor()))
          }else{
          }} else{
            currents <- c(currents, list(X3factor()))
          }
        if (!is.null(uX4())){
          if (uX4() == T){
            currents <- c(currents, list(X4factor()))
          }else{
          }} else{
            currents <- c(currents, list(X4factor()))
          }
        if (!is.null(uX5())){
          if (uX5() == T){
            currents <- c(currents, list(X5factor()))
          }else{
          }} else{
            currents <- c(currents, list(X5factor()))
          }
    
        
        predata <- c(list(Yfactor()),currents, lagsy, lagsx1, lagsx2, lagsx3, lagsx4, lagsx5)
        
          

        
        #predata <- c(list(Yfactor(), X1factor(), X2factor(), X3factor(), X4factor(), X5factor()), lagsy, lagsx1, lagsx2, lagsx3, lagsx4, lagsx5)
        
#         if (length(lagsy) == 0){
#           if (input$numberOfX == 5){predata <- list(Yfactor(), X1factor(), X2factor(), X3factor(), X4factor(), X5factor())}                 
#         }
#         if (length(lagsy) == 1){
#           if (input$numberOfX == 5){predata <- list(Yfactor(), X1factor(), lagsy[[1]], X2factor(), X3factor(), X4factor(), X5factor())}                 
#         }
#         if (length(lagsy) == 2){
#           if (input$numberOfX == 5){predata <- list(Yfactor(), X1factor(), lagsy[[1]],lagsy[[2]], X2factor(), X3factor(), X4factor(), X5factor())}                 
#         }
#         if (length(lagsy) == 3){
#           if (input$numberOfX == 5){predata <- list(Yfactor(), X1factor(), lagsy[[1]],lagsy[[2]],lagsy[[3]], X2factor(), X3factor(), X4factor(), X5factor())}                 
#         }
#         if (length(lagsy) == 4){
#           if (input$numberOfX == 5){predata <- list(Yfactor(), X1factor(), lagsy[[1]],lagsy[[2]],lagsy[[3]],lagsy[[4]], X2factor(), X3factor(), X4factor(), X5factor())}                 
#         }
#         if (length(lagsy) == 5){
#           if (input$numberOfX == 5){predata <- list(Yfactor(), X1factor(), lagsy[[1]],lagsy[[2]],lagsy[[3]],lagsy[[4]],lagsy[[5]], X2factor(), X3factor(), X4factor(), X5factor())}                 
#         }

               
               
               

          
        #print(class(lagsy[[1]]))
        
        
        
        #print(head(predata))
        
        m <- validateDates(predata)
        #xts_dataframe(m)
        #print(class(m))
        #print(time(m))
        #q <- data.frame(dates=toString(m))
        #print((names(m)
        gshaped <- reshape(m, idvar="Date", 
                        times=names(m)[names(m) != 'Date'], 
                        timevar="Type",
                        varying=list(names(m)[names(m) != 'Date']),
                        v.names="Value",
                        direction="long")
        

        
        list(m, gshaped)
        
        #m
        #AAPL

      
      
      
       
      
    } else{
      NA
  }
  })
  

})

