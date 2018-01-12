library(WordR)
library(ReporteRs)
library(ggplot2)
library(shiny)
#load(url("http://www.stats.gla.ac.uk/~levers/rp/trump.RData"))
#View (trump)
attach(trump)
wkday <- function (x){
  if(x=="Sunday"){day = 6}
  if(x=="Monday"){day = 0}
  if(x=="Tuesday"){day = 1}
  if(x=="Wednesday"){day = 2}
  if(x=="Thursday"){day = 3}
  if(x=="Friday"){day = 4}
  if(x=="Saturday"){day = 5}
  day
}


text.distance <- function(x, y) {
  words.x <- strsplit(x, " ")[[1]]
  words.y <- strsplit(y, " ")[[1]]
  1-length(intersect(words.x, words.y))/length(union(words.x, words.y))
}

hour_diff <- function (x, y){
  hr_week <- 51*(wkday(x)*24+y)
  for (i in 1:length(trump$hour_in_week)){
    trump$hour_diff[i] <- abs(trump$hour_in_week[i] - hr_week)
  }
  return(trump$hour_diff)
}

cont_diff <- function(x){
  for (i in 1:length(trump$text)){
    trump$cont_diff[i] <- text.distance(trump$text[i],x)
  }
  return(trump$cont_diff)
} 
totalD <- function (d,h,t){
  hour_diff <- hour_diff(d,h)
  cont_diff <- cont_diff(t)
  for (i in 1:length(cont_diff)){
    trump$tD[i] <- cont_diff[i]*(1/15) + hour_diff[i]*(1/15) 
  }
  return(trump$tD)
}
func <- function (x){
  trump_td <- order(x)
  prob <- round((sum(trump_td[1:15]))/15)
  prob
}

###########################
##### Shiny App Code ######
###########################
ui = fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Predicting Trump's Tweet using k=15 nearest neighbor"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      div(
        
        id = "form",
        
        textInput("dow", "Day of the Week:", ""),
        numericInput("time", "Time of the day:", ""),
        textInput("tweet", "Tweet:", ""),
        actionButton("submit", "Submit", class = "btn-primary"),
        downloadButton('downloadReport')
      )
    ),
    mainPanel(
      plotOutput('nearPlot'),
      dataTableOutput('neighbors')
    )
    )
    
  )
  

server <- function(input, output) {
  observeEvent(input$submit, {
    #shinyjs::hide("form")
    if(!is.null(input$file1)){
      trump=input$file1
    }
    for (i in 1:length(dow)){
      trump$hour_in_week[i] <- 51*(wkday(toString(dow[i]))*24+hour[i])
    }
    ft <- totalD(input$dow, input$time, input$tweet)
    ans <- func(ft)
    print(ans)
    if (ans < 1){
      showModal(modalDialog(
        title = "Tweet Status",
        "Not a Trump tweet",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      showModal(modalDialog(
        title = "Tweet status",
        "This is a Trump tweet",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    output$nearPlot <- renderPlot({
      plot(order(ft)[1:15])
    })
    output$neighbors <- renderDataTable({
      data.frame(order(ft)[1:15])
    })
    
  })
  
  output$downloadReport <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(Plot = 'nearPlot', neighbors = 'neighbors')
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })

}

shinyApp(ui, server)