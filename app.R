library(tidyverse)
library(data.table)
library(DT)
library(shiny)
library(shinydashboard)
library(randomForest)
library(caret)
library(plotly)

#read in brfss data once with fast read, then remove column number column
brfss2013 <- tbl_df(fread("brfss2013.csv", header = T, sep = ','))   #reads in big ole csv file

set.seed(31594) #set seed for machine learning shenanigans

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "BRFSS (...from 2013)"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "introduction"),
                        menuItem("Data Exploration", tabName = "eda"),
                        menuItem("Clustering", tabName = "cluster"),
                        menuItem("Data Modeling", tabName = "modeling"),
                        menuItem("Data Table", tabName = "table")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # Introduction Tab
                        tabItem(tabName = "introduction",
                                fluidRow(
                                  h3("Intro to the BRFSS App"),
                                  "The Behavioral Risk Factor Surveillance System is a major project undertaken by the CDC every year to understand the relationships
                                  between factors such as dieting, drug use, and social perceptions and health outcomes such as heart risk and obesity. More information
                                  can be found on the BRFSS website by clicking", a("here.", href="https://www.cdc.gov/brfss/about/index.htm"),
                                  br(),
                                  br(),
                                  "Fortunately, I was entrusted with the 2013 edition of the BRFSS dataset while taking a course at Duke, in order to practice exploratory data
                                  analysis. The purpose of this app will be to utilize several new abilities taught in my data science course to do a little
                                  more than basic EDA. While the original dataset has 330 variables, since most subscales were calculated after interviews and some questions are repeated, I subsetted the
                                  variables to be included in this app, leaving 61 for analysis. A description of the full scope of the survey and its variables can be found", a("here.", href="https://www.cdc.gov/brfss/annual_data/annual_2013.html"),
                                  br(),
                                  br(),
                                  "There are four panels to the left that will let us do different things with our data set.",
                                  br(),
                                  br(),
                                  "The Data Exploration tab will let you create a few tables and graphs with the data. Since the data set is ", em("a small 492,773 observations"),
                                  "you'll have the ability to subset this dataset by State. Each state has several thousand participants apiece, so you'll be able to 
                                  look at descriptive information of any state with tables and graphs.",
                                  br(),
                                  br(),
                                  "The Clustering tab will let you visualize and set clusters for subsets of data. Good luck!",
                                  br(),
                                  br(),
                                  "The Data modeling tab will let you, well, model the data with supervised learning! Here you'll be able 
                                   to specify an outcome of interest, how many variables to use, and for seeing how accurate models generated were based on RMSE. On the menu for what methods to use, simple linear regression
                                   (yes this is considered a machine learning method, I wasn't sure but since this project is open ended
                                   I had a cool programming idea for mathJax and ran with it) and random forests are on the house today.",
                                  br(),
                                  br(),
                                  "The Data Table tab will give a nice table to look at, with the ability to subset the data by State, by gender, and by age group.",
                                  br(),
                                  br(),
                                  "I should also mention that due to git's lovely limitations, I have filtered the data set to only include data from North Carolina, California, Florida, and Guam."
                                )),
                        
                        #Data Exploration Tab
                        tabItem(tabName = "eda",
                                h4("Summary of each state's participant count"),
                                dataTableOutput("EDABigTable"),
                                br(),
                                uiOutput("edatitle"),
                                h3("Select a State/Region to analyze"),
                                selectizeInput("state1", "State", selected = "North Carolina", choices = levels(as.factor(brfss2013$State))),
                                h3("Select a variable to examine"),
                                selectizeInput("edavar1", "Variable One", selected = "Age_Group", choices = attributes(brfss2013)$names[c(2:61)]),
                                checkboxInput("edavar2check", "A two-way table, kind user?"),
                                conditionalPanel(condition = "input.edavar2check",
                                                 h3("Select another variable to examine"),
                                                 uiOutput('edavar2ui')),
                                downloadButton('summarysave', "Save Table Data"),
                                tableOutput("basicsummary"),
                                br(),
                                selectizeInput("edagraphtype", "Select graph type", choices = c("scatter", "histogram"), selected = "histogram"),
                                h3("Click the camera icon to save the graph"),
                                plotlyOutput("basicgraph")
                        ),
                        
                        #Clustering Tab
                        tabItem(tabName = "cluster",
                                h3("Select a State/Region to analyze"),
                                selectizeInput("state2", "State", selected = "North Carolina", choices = levels(as.factor(brfss2013$State))),
                                uiOutput('clusterxchoice'),
                                uiOutput('clusterychoice'),
                                
                                numericInput('clusters', 'How many clusters, chief?', 3,
                                             min = 1, max = 9),
                                downloadButton("downloadPlotC", "Download Cluster Plot"),
                                downloadButton("downloadDataC", "Download Cluster Data"),
                                plotOutput('clusterplot'),
                                br(),
                                downloadButton("downloadDendoC", "Download Dendogram"),
                                downloadButton("downloadDendoDataC", "Download Dendogram Data"),
                                plotOutput('dendo')
                        ),
                        
                        #Modeling data tab
                        tabItem(tabName = "modeling",
                                h3("Select a State/Region to analyze"),
                                selectizeInput("state3", "State", selected = "North Carolina", choices = levels(as.factor(brfss2013$State))),
                                h4("Simple Linear Regression Modeling"),
                                uiOutput('linearregressionchoiceout'),
                                selectizeInput('linpredictor', 'Choose Predictor Variable', choices = names(brfss2013 %>% select(-State))),
                                withMathJax(),
                                uiOutput('regMathJax'),
                                uiOutput('regoutputchoice'),
                                verbatimTextOutput('regoutput'),
                                br(),
                                uiOutput('regpredictinput'),
                                verbatimTextOutput('regpredictfit'),
                                h4("Random Forest Modeling"),
                                uiOutput('randomforestchoiceout'),
                                numericInput('treenum', 'Choose number of trees for Random Forest model (10 to 500)', min = 10, max = 500, value = 10),
                                uiOutput('rfoutputchoice'),
                                verbatimTextOutput('rfoutput'),
                                verbatimTextOutput('rftestresults'),
                                uiOutput('bmipredictinput'),
                                uiOutput('phypredictinput'),
                                uiOutput('menpredictinput'),
                                uiOutput('slppredictinput'),
                                uiOutput('alcpredictinput'),
                                uiOutput('frupredictinput'),
                                uiOutput('benpredictinput'),
                                uiOutput('grnpredictinput'),
                                uiOutput('orgpredictinput'),
                                tableOutput('rfpredictresults')
                                
                        ),
                        
                        #Data Table Tab
                        tabItem(tabName = "table",
                                h3("Select a State/Region to analyze"),
                                selectizeInput("state4", "State", selected = "North Carolina", choices = levels(as.factor(brfss2013$State))),
                                checkboxInput("sex", h4("Filter by Sex?")),
                                conditionalPanel(condition = "input.sex",
                                                 uiOutput('sexchoice')),
                                downloadButton("downloadDatatab", "Download Data Filtered by State and/or Sex"),
                                downloadButton("downloadFull", "Download All Data"),
                                div(style = 'overflow-x: scroll', dataTableOutput("tabtable"))
                        )
                      )
                    )
)


server <- function(input, output, session) {
  
  withMathJax() #for real math hours
  
  #####################DATA EXPLORATION TAB FUNCTIONS####
  
  #data read in for eda tab
  getedaData1 <- reactive({
    newedaData <- brfss2013 %>% filter(State == input$state1)  #subsets data by State
  })
  
  numericvalues <- names(brfss2013[,sapply(brfss2013,is.numeric)])
  charactervalues <- names(brfss2013[,sapply(brfss2013,is.character)])
  
  #data read in for Statewide participation table
  EDAFullCount <- reactive({brfss2013 %>% group_by(State) %>% summarize(count = n())})
  output$EDABigTable <- renderDataTable({
    EDAFullCount()
  })
  
  #data for title of Data Exploration Tab
  output$edatitle <- renderUI({
    text <- h2(paste0("Graphs and Summaries for ", input$state1))
  })   
  
  #UI options for second variable to consider
  output$edavar2ui <- renderUI({
    selectInput("edavar2", "Variable Two", selected = "Sex", choices = attributes(brfss2013)$names[c(2:61)])
  })
  
  #creates tables of one and two var contingencies
  
  output$basicsummary <- renderTable({
    
    #tables to make with two variables
    if(input$edavar2check){
      #table to make if first variable choice is numeric and second is character: summary tables for each category
      if ((input$edavar1 %in% numericvalues) & (input$edavar2 %in% charactervalues)){
        getedaData2 <- getedaData1()[,c(input$edavar1, input$edavar2)]
        getedaData3 <- unlist(tapply(getedaData2[[1]], getedaData2[[2]], summary))
        return(rownames_to_column(as.data.frame(getedaData3)))
      }
      
      #table to make if first variable choice is character and second is numeric: summary tables for each category
      else if ((input$edavar1 %in% charactervalues) & (input$edavar2 %in% numericvalues)){
        getedaData2 <- getedaData1()[,c(input$edavar1, input$edavar2)]
        getedaData3 <- unlist(tapply(getedaData2[[2]], getedaData2[[1]], summary))
        return(rownames_to_column(as.data.frame(getedaData3)))
      }
      
      
      #table to make if both variables are numeric: summary tables of both variables separately
      else if((input$edavar1 %in% numericvalues) & (input$edavar2 %in% numericvalues)){
        p <- summary(getedaData1()[,input$edavar1])
        return(cbind(p, summary(getedaData1()[,input$edavar2])))
      }
      
      #table to make if both variables are character: contingency table with two variables
      else{
        return(with(getedaData1(), table(get(input$edavar1), get(input$edavar2))))
      }
      
    }
    else{
      #table to make if single variable choice is numeric
      if(input$edavar1 %in% numericvalues){
        return(summary(getedaData1()[,input$edavar1]))
      }
      #table to make if single variable choice is character
      else{return(with(getedaData1(), table(get(input$edavar1))))}
    }
  })
  
  #kinda messy syntax, but basically saves the same data used in the table to a csv after button push
  output$summarysave <- downloadHandler(
    filename = function() {
      paste("summarydata.csv", sep = "")
    },
    content = function(file){
      if(input$edavar2check){
        if ((input$edavar1 %in% numericvalues) & (input$edavar2 %in% charactervalues)){
          getedaData2 <- getedaData1()[,c(input$edavar1, input$edavar2)]
          getedaData3 <- unlist(tapply(getedaData2[[1]], getedaData2[[2]], summary))
        write.csv(rownames_to_column(as.data.frame(getedaData3)), file, row.names = FALSE)
      }
        else if ((input$edavar1 %in% charactervalues) & (input$edavar2 %in% numericvalues)){
          getedaData2 <- getedaData1()[,c(input$edavar1, input$edavar2)]
          getedaData3 <- unlist(tapply(getedaData2[[2]], getedaData2[[1]], summary))
        write.csv(rownames_to_column(as.data.frame(getedaData3)), file, row.names = FALSE)
      }
        else if((input$edavar1 %in% numericvalues) & (input$edavar2 %in% numericvalues)){
          p <- summary(getedaData1()[,input$edavar1])
          write.csv((cbind(p, summary(getedaData1()[,input$edavar2]))), file, row.names = FALSE)
        }
        else {write.csv(with(getedaData1(), table(get(input$edavar1), get(input$edavar2))), file, row.names = FALSE)}
      }
      else{
        if(input$edavar1 %in% numericvalues){
          write.csv(summary(getedaData1()[,input$edavar1]), file, row.names = FALSE)
        }
        else{write.csv(getedaData1()[,input$edavar1], file, row.names = FALSE)}
      }
    }
  )
    
  
  #creates cool graph with Plotly
  
  graph <- reactive({
    if (input$edavar2check){
      if ((input$edavar1 %in% numericvalues) & (input$edavar2 %in% charactervalues)){
          p <- plot_ly(data = getedaData1(), x = ~get(input$edavar1), color = ~get(input$edavar2), type = input$edagraphtype)
      }
      else if ((input$edavar1 %in% charactervalues) & (input$edavar2 %in% numericvalues)){
        p <- plot_ly(data = getedaData1(), x = ~get(input$edavar2), color = ~get(input$edavar1), type = input$edagraphtype)
      }
      else if ((input$edavar1 %in% numericvalues) & (input$edavar2 %in% numericvalues)){
        p <- plot_ly(data = getedaData1(), x = ~get(input$edavar1), y = ~get(input$edavar2), type = input$edagraphtype)
      }
      else{p <- plot_ly(data = getedaData1(), x = ~get(input$edavar2), color = ~get(input$edavar1), type = input$edagraphtype)}
    }
    else{p <- plot_ly(data = getedaData1(), x = ~get(input$edavar1), type = input$edagraphtype)}
  })
  
  output$basicgraph <- renderPlotly({
    graph()
  })
  
  
  #######################CLUSTERING tab functions####
  
  #for UI
  
  #Ensures X and Y variables are only numeric, otherwise clustering doesn't seem to want to work
  output$clusterxchoice <- renderUI({
    # we only want to show numeric cols
    brfssnumeric <- na.omit(brfss2013[,sapply(brfss2013,is.numeric)])
    colnames <- names(brfssnumeric)
    selectInput('clusterx', 'X Variable', colnames)
  })
  
  output$clusterychoice <- renderUI({
    # we only want to show numeric cols
    brfssnumeric <- na.omit(brfss2013[,sapply(brfss2013,is.numeric)])
    colnames <- names(brfssnumeric)
    selectInput('clustery', 'Y Variable', colnames,
                selected=colnames[2])
  })  
  
  #filter data by State
  subsetData1 <- reactive({
    newedaData <- brfss2013 %>% filter(State == input$state2)
  })
  
  #filter filtered data by user variables
  subsetData2 <- reactive({
    subsetData1()[, c(input$clusterx, input$clustery)]
  })
  
  #filter filtered filtered data to exclude missings
  subsetData3 <- reactive({
    newData <- na.omit(subsetData2())
  })
  
  #Real cluster analysis hours
  clusters <- reactive({
    kmeans(subsetData3(), input$clusters)
  })
  
  #Display cluster, settings based on code used for cluster lecture
  output$clusterplot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(subsetData3(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  #do hierarchical cluster for dendogram
  hierClust <- reactive({
    data <- hclust(dist(data.frame(subsetData3()[,1], subsetData3()[,2])))
  })
  
  #create dendogram
  output$dendo <- renderPlot({
    plot(hierClust(), main="dendrogram")
  })
  
  #save clustered diagram
  output$downloadPlotC <- downloadHandler(
    filename = function() {
      paste("clusterplot.png", sep = "")
    },
    content = function(file){
      ggsave(file, plot = plot(subsetData3(),
                               col = clusters()$cluster,
                               pch = 20, cex = 3)
      )
    }
  )
  
  #save clustered diagram data
  output$downloadDataC <- downloadHandler(
    filename = function() {
      paste("clusterdata.csv", sep = "")
    },
    content = function(file){
      write.csv(unlist(clusters()), file, row.names = TRUE)
    }
  )
  
  #save dendogram
  output$downloadDendoC <- downloadHandler(
    filename = function() {
      paste("dendogram.png", sep = "")
    },
    content = function(file){
      ggsave(file, plot = plot(hierClust(), main="dendrogram"))
    }
  )
  
  #save dendogram data
  output$downloadDendoDataC <- downloadHandler(
    filename = function() {
      paste("dendodata.csv", sep = "")
    },
    content = function(file){
      write.csv(unlist(hierClust()$merge), file, row.names = TRUE)
    }
  )
  
  ########################DATA MODELING tab functions####
  
  #####PART 1: Simple Linear Regression
  
  #subsets data by State, removing State from options
  getregData <- reactive({
    newregData <- brfss2013 %>% filter(State == input$state3) 
  })
  
  #next two reactives subset data further, removing observations with missing values in response varaible
  outcome <- reactive({
    hold <- paste0(input$linchoice)
    
  })
  
  #show options for simple linear regression
  output$linearregressionchoiceout <- renderUI({
    brfssnumeric <- na.omit(brfss2013[,sapply(brfss2013,is.numeric)])
    colnames <- names(brfssnumeric)
    selectInput('linchoice', 'Choose Outcome variable', colnames)
  })
  
  #create formula portion of lm function here
  regressionformula <- reactive({
    formula <- paste0(input$linchoice, " ~ ", input$linpredictor)
  })
  
  #run regression model
  regressionmodel <- reactive({
    #linear regression model
    model <- lm(regressionformula(), data = getregData())
  })
  
  #Regression Output
  #Output a mathJax string of the equation at hand
  regressionequation <- reactive({
    for(a in 1:length(names(regressionmodel()$coefficients))){
      if (a == 1){
        string <- paste0('$$', input$linchoice, ' \\approx ', round(regressionmodel()$coefficients[a], 2))
      }
      else if (a > 1 & a < length(names(regressionmodel()$coefficients))){
        string <- paste0(string, ' + ', round(regressionmodel()$coefficients[a], 2), ' * ', 'X', (a-1), '_', names(regressionmodel()$coefficients[a]) )
      }
      else{
        return(paste0(string, ' + ', round(regressionmodel()$coefficients[a], 2), ' * ', 'X', (a-1), '_', names(regressionmodel()$coefficients[a]), "$$"))
      }
    }
    
  })
  
  output$regMathJax <- renderUI({
    withMathJax(
      helpText(regressionequation())
    )
  })
  
  #Select attribute of regression model to output
  output$regoutputchoice <- renderUI({
    selectInput("regoutput", "Select the Regression Model Attribute you'd like to view:", choices = attributes(regressionmodel())$names)
  })
  
  output$regoutput <- renderPrint({
    regressionmodel()[[input$regoutput]]
  })
  
  
  
  #for simple linear regression prediction
  
  regcheckdata <- reactive({
    hold2 <- unique(getregData()[,input$linpredictor])
  })
  
  output$regpredictinput <- renderUI({
    
    
    if (input$linpredictor %in% names(getregData()[,sapply(getregData(),is.character)])){
      selectInput("charpredict", "Enter prediction category", choices = regcheckdata())
    }
    else {
      numericInput("numpredict", "Enter value for prediction", value = 3, min = 0, max = 25)
    }
  })
  
  output$regpredictfit <- renderPrint({
    
    if(input$linpredictor %in% names(getregData()[,sapply(getregData(),is.character)])){
      charframe <- data.frame(var =  input$charpredict)
      colnames(charframe) <- input$linpredictor      
      predict(regressionmodel(), newdata = charframe, se.fit = TRUE)
    }
    
    else {
      numframe <- data.frame(var = input$numpredict)
      colnames(numframe) <- input$linpredictor      
      predict(regressionmodel(), newdata = numframe, se.fit = TRUE)
    }    
  })
  
  ##### PART 2: Random Forest algorithm time!
  
  getrfData <- reactive({
    dataset <- getregData()[,sapply(getregData(),is.numeric)] #stick to numeric columns for now
    dataset <- na.omit(dataset[, colSums(is.na(dataset)) != nrow(dataset)])     #eliminate any columns that are all missings
  })
  
  output$randomforestchoiceout <- renderUI({
    colnames <- names(getrfData())
    selectInput('rfchoice', 'Choose Outcome variable', colnames)
  })
  
  #will put 80% of the rows in train
  rftrainvalues <- reactive({
    trainvalues <- sample(1:nrow(getrfData()), size = nrow(getrfData())*0.8) 
  })
  
  #will put 20% of the rows in test
  rftestvalues <- reactive({
    testvalues <- dplyr::setdiff(1:nrow(getrfData()), rftrainvalues()) 
  })
  
  #creates a training set
  rftrainset <- reactive({
    trainset <- getrfData()[rftrainvalues(), ]   
  })
  
  #creates a testing set
  rftestset <- reactive({
    testset <- getrfData()[rftestvalues(), ] 
  })
  
  #create formula for use in train function
  forestformula <- reactive({
    formula <- paste0(input$rfchoice, " ~ .")
  })
  
  #train using random forest
  randomforesthours <- reactive({
    rf_train <- train(as.formula(forestformula()), data = rftrainset(), method = "rf", 
                      ntree = input$treenum,
                      importance = TRUE, trControl = trainControl(method = "cv", number = 10))      
  })
  
  #use test set to make predictions with random forest model
  randomforesttestpredict <- reactive({
    rftestpredict <- as.tibble(predict(randomforesthours(), newdata = rftestset()))
  })
  
  #hold only values from test set from the inputted variable of interest
  rfoutcomeindex <- reactive({
    testsetoutcome <- rftestset()[, input$rfchoice]
  })
  
  #output attributes of Random Forest model for viewing
  
  output$rfoutputchoice <- renderUI({
    selectInput("rfoutput", "Select the Random Forest Model Attribute you'd like to view:", choices = attributes(randomforesthours())$names)
  })
  
  output$rfoutput <- renderPrint({
    randomforesthours()[[input$rfoutput]]
  })
  
  #The formula sqrt(mean((randomforesttestpredict()-rfoutcomeindex())^2)) refused to work due to some strange coercion error, so I
  #bit the bullet and made a for loop to handle this. This calculates Root MSE of test data and predicted test data
  rfRMSE <- reactive({
    for (a in 1:length(randomforesttestpredict())){
      diff = randomforesttestpredict()[a] - rfoutcomeindex()[a]
      if (a == 1){
        sumofdiff <- diff^2
      }
      else{sumofdiff <- sumofdiff + diff}
      if (a == length(randomforesttestpredict())){
        meandiff <- mean(sumofdiff[,1])
        return(sqrt(meandiff))
      }
    }
    
  })
  
  output$rftestresults <- renderPrint({
    data.frame(RMSE_from_test_set = rfRMSE())
  })
  
  
  
  #predict using random forest
  
  rfimportance <- reactive({
    sig <- rownames_to_column(varImp(randomforesthours())$importance)    #save importance of each variable with names in accessible column
  })
  
  #creates inputs for each variable prediction the user specifies
  #step 1: save value of overall importance for a given variable
  #step 2: create text to be shown in textInput into separate object, rounding the importance value as well
  #step 3: check if the outcome variable is the same as the output's variable of interest. If not, output textInput for prediction
  #step 4: if it is, output text to prevent input for prediction (No sense in predicting BMI using BMI, for example)
  
  output$bmipredictinput <- renderUI({
    bmi <- rfimportance() %>% filter(rowname == "BMI") %>% select(Overall)
    bmitext <- paste("Input numeric prediction value for BMI. (Predictive importance:", round(bmi, 2), ")")
    if (input$rfchoice != "BMI"){
      textInput("bmipredict", bmitext, value = 0)
    }
    else{textInput("bmipredict", paste(bmitext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$phypredictinput <- renderUI({
    phyhealth <- rfimportance() %>% filter(rowname == "Physical_Health") %>% select(Overall)
    phytext <- paste("Input numeric prediction value for Physical_Health. (Predictive importance:", round(phyhealth, 2), ")")
    if (input$rfchoice != "Physical_Health"){
      textInput("phypredict", phytext, value = 0)
    }
    else{textInput("phypredict", paste(phytext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$menpredictinput <- renderUI({
    menhealth <- rfimportance() %>% filter(rowname == "Mental_Health") %>% select(Overall)
    mentext <- paste("Input numeric prediction value for Mental_Health. (Predictive importance:", round(menhealth, 2), ")")
    if (input$rfchoice != "Mental_Health"){
      textInput("menpredict", mentext, value = 0)
    }
    else{textInput("menpredict", paste(mentext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$slppredictinput <- renderUI({
    sleep_av <- rfimportance() %>% filter(rowname == "Sleep_Average") %>% select(Overall)
    slptext <- paste("Input numeric prediction value for Sleep_Average. (Predictive importance:", round(sleep_av, 2), ")")
    if (input$rfchoice != "Sleep_Average"){
      textInput("slppredict", slptext, value = 0)
    }
    else{textInput("slppredict", paste(slptext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$alcpredictinput <- renderUI({
    alcohol <- rfimportance() %>% filter(rowname == "Monthly_Alcohol") %>% select(Overall)
    alctext <- paste("Input numeric prediction value for Monthly_Alcohol. (Predictive importance:", round(alcohol, 2), ")")
    if (input$rfchoice != "Monthly_Alcohol"){
      textInput("alcpredict", alctext, value = 0)
    }
    else{textInput("alcpredict", paste(alctext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$frupredictinput <- renderUI({
    fruit <- rfimportance() %>% filter(rowname == "Daily_fruit_juice") %>% select(Overall)
    fruittext <- paste("Input numeric prediction value for Daily_fruit_juice. (Predictive importance:", round(fruit, 2), ")")
    if (input$rfchoice != "Daily_fruit_juice"){
      textInput("fruitpredict", fruittext, value = 0)
    }
    else{textInput("fruitpredict", paste(fruittext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$benpredictinput <- renderUI({
    beans <- rfimportance() %>% filter(rowname == "Daily_Beans") %>% select(Overall)
    beanstext <- paste("Input numeric prediction value for Daily_Beans. (Predictive importance:", round(beans, 2), ")")
    if (input$rfchoice != "Daily_Beans"){
      textInput("beanspredict", beanstext, value = 0)
    }
    else{textInput("beanspredict", paste(beanstext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$grnpredictinput <- renderUI({
    greens <- rfimportance() %>% filter(rowname == "Daily_Greens") %>% select(Overall)
    greentext <- paste("Input numeric prediction value for Daily_Greens. (Predictive importance:", round(greens, 2), ")")
    if (input$rfchoice != "Daily_Greens"){
      textInput("greenpredict", greentext, value = 0)
    }
    else{textInput("greenpredict", paste(greentext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  output$orgpredictinput <- renderUI({
    oranges <- rfimportance() %>% filter(rowname == "Daily_orange") %>% select(Overall)
    orangetext <- paste("Input numeric prediction value for Daily_orange. (Predictive importance:", round(oranges, 2), ")")
    if (input$rfchoice != "Daily_orange"){
      textInput("orangepredict", orangetext, value = 0)
    }
    else{textInput("orangepredict", paste(orangetext, "(Outcome variable will not affect prediction)"), value = 999)}
  })
  
  #create dataset from inputs. Note that this can still take a character input and raise an error, however, checking ranges of numeric
  #inputs was a little too involved for me, so I stuck to numeric inputs
  rfpredictionframe <- reactive({
    rfframe <- data.frame(BMI = as.numeric(input$bmipredict), Physical_Health = as.numeric(input$phypredict), 
                          Mental_Health = as.numeric(input$menpredict),Sleep_Average = as.numeric(input$slppredict), 
                          Monthly_Alcohol = as.numeric(input$alcpredict), Daily_fruit_juice = as.numeric(input$fruitpredict), 
                          Daily_Beans = as.numeric(input$beanspredict), Daily_Greens = as.numeric(input$greenpredict), 
                          Daily_orange = as.numeric(input$orangepredict)
    )
  })
  
  #creates table of predicted value from inputted prediction value
  rfpredictedvalue <- reactive({
    prediction <- data.frame(Predicted_Value = predict(randomforesthours(), newdata = rfpredictionframe()))
  })
  
  output$rfpredictresults <- renderTable({
    rfpredictedvalue()
  })
  
  
  ######################DATA TABLE tab functions####
  
  #filter data based on state
  
  gettabData1 <- reactive({
    newtabData <- brfss2013 %>% filter(State == input$state4)  #subsets data by State
  })
  
  output$sexchoice <- renderUI({
    sexchoices <- as.vector( levels(as.factor(brfss2013$Sex)))
    selectInput("choices","Filter by which sex?", choices=sexchoices)    
  }) 
  
  gettabData2 <- reactive({
    subset(gettabData1(), Sex %in% input$choices)
  })
  
  
  #output the table
  output$tabtable <- renderDataTable({
    if (input$sex == FALSE){
      gettabData1()
    }
    else{gettabData2()}
    
  })
  
  #save the filtered data from the table tab
  
  output$downloadDatatab <- downloadHandler(
    filename = function() {
      paste("brfss2013filtereddata.csv", sep = "")
    },
    #save data filtered by sex if it was filtered by 
    content = function(file){
      if (input$sex){
        write.csv(gettabData1(), file, row.names = FALSE)
      }
      else{write.csv(gettabData2(), file, row.names = FALSE)}
    }
  )
  
  #save the full dataset from the table tab
  output$downloadFull <- downloadHandler(
    filename = function() {
      paste("brfss2013ALLDATA.csv", sep = "")
    },
    #save data filtered by sex if it was filtered by 
    content = function(file){
      write.csv(brfss2013, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)