library(tidyverse)
library(data.table)
library(DT)
library(shiny)
library(shinydashboard)

#read in brfss data once with fast read, then remove column number column
brfss2013 <- tbl_df(fread("./brfss2013.csv", header = T, sep = ','))   #reads in big ole csv file


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
                "you'll have the ability to subset this dataset by State. Each state has several thousand participants apiece, so you'll be able to not only
                look at descriptive information of each state with tables and graphs, but you can compare two states to each other on some set of relationships as well",
                br(),
                br(),
                "The Clustering tab will let you visualize and set clusters for subsets of data. Good luck!",
                br(),
                br(),
                "The Data modeling tab will let you, well, model the data with supervised learning! Here you'll be able to specify an outcome of interest, how many variables to use, and for seeing
                how accurate the best three models were in generating a model. On the menu for what methods to use, random forests and", em("k"), "nearest-neighbors are on the house today.",
                br(),
                br(),
                "The Data Table tab will give a nice table to look at, with the ability to subset the data by State, by gender, and by age group."
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
              tableOutput("basicsummary"),
              br(),
              plotOutput("basicgraph")
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
             selectizeInput("regressiontype", "Select Regression Type", selected = 'Linear', choices = c('Linear', 'Logistic')),
             
             #selecting response and predictor for Linear regression
             conditionalPanel(condition = "input.regressiontype == 'Linear'", 
                              uiOutput('linearregressionchoiceout'),
                              selectizeInput('linpredictor', 'Linear Regression Predictor', choices = names(brfss2013 %>% select(-State)))),

             
             #selecting response and predictor for Logistic regression
             conditionalPanel(condition = "input.regressiontype == 'Logistic'",
                              uiOutput('logisticregressionchoiceout'),
                              selectizeInput('logpredictor', 'Logistic Regression Predictor', choices = names(brfss2013 %>% select(-State)))),
  
             textOutput('regressionequation')
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
    if (input$edavar2check){
      with(getedaData1(), table(get(input$edavar1), get(input$edavar2)))
    }
    else{with(getedaData1(), table(get(input$edavar1)))}
  })
  
  #checks whether inputted variables are character/numeric
  
  edacheck <- reactive({
    check <- getedaData1()
  })
  
  #creates graphs of one and two var relationships
  
  edag <- reactive({
    if (input$edavar2check){
      g <- ggplot(getedaData1, aes(x = input$edavar1, y = input$edavar2))
    }
    else{
      g <- ggplot(getedaData1, aes(x = input$edavar1))
    }
  })
  
  
  output$basicgraph <- renderPlot({
    if (input$edavar2check){
      plot(getedaData1)
    }
    else{
      plot(getedaData1)
    }
  })

  
########################DATA MODELING tab functions####
  
  #subsets data by State, removing State from options
  getregData <- reactive({
    newregData <- brfss2013 %>% filter(State == input$state2) %>% select(-State)  
  })
  
  #next two reactives subset data further, removing observations with missing values in response varaible
  outcome <- reactive({
    if (input$regressiontype == 'Linear'){
    hold <- paste0(input$linchoice)
    }
    else{hold <- paste0(input$logchoice)}
  })
  
  getregData2 <- reactive({
    newregData2 <- na.omit(getregData()$outcome())
  })
  
  #show options for simple linear regression
  output$linearregressionchoiceout <- renderUI({
    brfssnumeric <- na.omit(brfss2013[,sapply(brfss2013,is.numeric)])
    colnames <- names(brfssnumeric)
    selectInput('linchoice', 'Linear Regression Outcome', colnames)
  })
  
  output$logisticregressionchoiceout <- renderUI({
    brfsscharacter <- na.omit(brfss2013[,sapply(brfss2013,is.character)])
    colnames <- names(brfsscharacter %>% select(-State))
    selectInput('logchoice', 'Logistic Regression Outcome', colnames)
  })
  
  
  regressionformula <- reactive({
    #set regression formula to linear default if Linear option chosen
    if (input$regressiontype == 'Linear'){
        formula <- paste0(input$linchoice, " ~ ", input$linpredictor)
    }
    
    #regression formula to binomial if Logistic option chosen
    else{formula <- paste0(input$logchoice, "~ ", input$logpredictor)}
    
    
    
  })
  #run regression model
  regressionmodel <- reactive({
    #linear regression model
    if (input$regressiontype == 'Linear'){
        model <- lm(regressionformula(), data = getregData())
    }
    
    #logistic regression model
    else{model <- lm(regressionformula(), data = getregData(), family = "binomial")}
  })
  
  output$regressionequation <- renderText({
    if (input$regressiontype == 'Linear'){
      for(a in 1:length(names(regressionmodel()$coefficients))){
        if (a == 1){
          string <- paste(names(regressionmodel()$coefficients[a]), round(regressionmodel()$coefficients[a], 2))
        }
        else if (a > 1 & a < length(names(regressionmodel()$coefficients))){
          string <- paste(string, "+", names(regressionmodel()$coefficients[a]), round(regressionmodel()$coefficients[a], 2))
        }
        else{
          return(paste(string, "+", names(regressionmodel()$coefficients[a]), round(regressionmodel()$coefficients[a])))
        }
      }
    }
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
      paste(input$clusterplot, ".png", sep = "")
    },
    content = function(file){
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = output$clusterplot(), device = device)
    }
  )
  
  #save clustered diagram data
  output$downloadDataC <- downloadHandler(
    filename = function() {
      paste(input$clusterdata, ".csv", sep = "")
    },
    content = function(file){
      write.csv(clusters(), file, row.names = FALSE)
    }
  )
  
  #save dendogram
  output$downloadDendoC <- downloadHandler(
    filename = function() {
      paste(input$dendogram, ".png", sep = "")
    },
    content = function(file){
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = output$dendo(), device = device)
    }
  )
  
  #save dendogram data
  output$downloadDendoDataC <- downloadHandler(
    filename = function() {
      paste(input$dendodata, ".csv", sep = "")
    },
    content = function(file){
      write.csv(hierClust(), file, row.names = FALSE)
    }
  )

  
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
      paste(input$tabledata, ".csv", sep = "")
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
      paste(input$tabledata, ".csv", sep = "")
    },
    #save data filtered by sex if it was filtered by 
    content = function(file){
        write.csv(brfss2013, file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)