#' @title K-Nearest Neighbour Classifier
#'
#' @description This is a KNN Classifier. It predicts any attribute(categorical) given a set of numeric input predictor values. Note that only numeric input predictors should be given.
#'              The k value can be chosen according to accuracies provided. The attribute to be predicted can be selected from the dropdown provided(select categorical attribute).
#'              This is because categorical attributes cannot be given as inputs here. A 'handsontable' is also provided to enter the input predictor values.
#'
#' @return NULL
#'
#' @examples if(interactive()){KCSKNNShiny()}
#'
#' @export

KCSKNNShiny <- function() {
  requireNamespace("shiny")
  requireNamespace("rhandsontable")
  requireNamespace("dplyr")
  requireNamespace("caret")
  requireNamespace("FNN")

  ui <- fluidPage(
    titlePanel(title = "K-Nearest Neighbour Classifier"),
    h5("This is a KNN Classifier interactive Shiny Application that predicts the class(categorical) attribute for numeric input predictors. Select
       the categorical attribute as the variable to be predicted"),
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "file",label = "Select file",multiple = FALSE),
        sliderInput(inputId = "slider",label = "what is the proportion of the training dataset?",min = 0.1,max = 0.9,value = 0.6,step = 0.1),
        sliderInput(inputId = "k",min = 1,max = 10,label = "Choose k value",value = 3),
        uiOutput("selectinput"),
        h5("You can download an example dataset 'RidingMowers.csv' by clicking the link below to try the application."),
        helpText( a("Click Here",href="https://sites.google.com/site/kartikeyab/home/business_analytics_ws/RidingMowers.csv?attredirects=0&d=1")
        )

      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data",tableOutput("Data")),
          tabPanel(title = "Accuracy",h4("This accuracy table can be used to get an idea about the optimum value of k. The trainig and testing datasets are used to give these accuracies which can be approximated for the whole dataset"),verbatimTextOutput("accuracy")),
          tabPanel(title="Prediction",
                   tags$h2("Please edit the predictor values in the table given below"),
                   tags$br(),
                   rHandsontableOutput("table"),
                   tags$br(),
                   tags$h3("The prediction is: "),
                   verbatimTextOutput("answer"))
        )
      )
    ))


  server <- function(input, output, session) {

    data<-reactive({
      file1<-input$file
      if(is.null(file1)){return()}
      read.table(file1$datapath,sep=",",header=TRUE)
    })

    output$Data<-renderTable({
      data()
    })

    output$selectinput<-renderUI({
      if(is.null(data())){return()}
      else
        selectInput(inputId = "var",label = "select the variable to be predicted(categorical variable)",multiple = F,choices = colnames(data()))
    })

    output$table<-renderRHandsontable({
      data1<-data()
      ds<-select(data1,c(-input$var))
      rhandsontable(ds[1, ])
    })

    output$answer<-renderPrint({
      data1<-data()
      rownames(data1)<-1:nrow(data1)
      train.index<-sample(rownames(data1),dim(data1)[1] * input$slider)
      valid.index<-setdiff(rownames(data1),train.index)
      train.df<-data1[train.index, ]
      valid.df<-data1[valid.index, ]
      train.norm.df<-train.df
      valid.norm.df<-valid.df
      data.norm.df<-data1
      a<-match(input$var,names(data()))
      norm.values<-preProcess(train.df[ ,-a],method=c("center","scale"))
      train.norm.df[ ,-a]<-predict(norm.values, train.df[ ,-a])
      valid.norm.df[ ,-a]<-predict(norm.values, valid.df[ ,-a])
      data.norm.df[ ,-a]<-predict(norm.values, data1[ ,-a])
      new.df<-as.data.frame(input$table$data)
      k<-colnames(data1)!=input$var
      colnames(new.df)<-colnames(data1[k])
      new.norm.df<-predict(norm.values,new.df)
      nn<-knn(train = data.norm.df[ ,-a],test = new.norm.df,cl = data.norm.df[ ,a],k=input$k)
      nn
    })


    output$accuracy<-renderPrint({
      data1<-data()
      rownames(data1)<-1:nrow(data1)
      train.index<-sample(rownames(data1),dim(data1)[1] * input$slider)
      valid.index<-setdiff(rownames(data1),train.index)
      train.df<-data1[train.index, ]
      valid.df<-data1[valid.index, ]
      train.norm.df<-train.df
      valid.norm.df<-valid.df
      data.norm.df<-data1
      a<-match(input$var,names(data()))
      norm.values<-preProcess(train.df[ ,-a],method=c("center","scale"))
      train.norm.df[ ,-a]<-predict(norm.values, train.df[ ,-a])
      valid.norm.df[ ,-a]<-predict(norm.values, valid.df[ ,-a])
      data.norm.df[ ,-a]<-predict(norm.values, data1[ ,-a])
      accuracy<-data.frame(k=seq(1,10,1),accuracy=rep(0,10))
      for(i in 1:10){
        knnpred<-knn(train = train.norm.df[ ,-a],test = valid.norm.df[ ,-a],cl=train.norm.df[ ,a],k=i)
        accuracy[i,2]<-confusionMatrix(knnpred,valid.norm.df[ ,a])$overall[1]}
      accuracy
    })

  }

  shinyApp(ui, server)
}
