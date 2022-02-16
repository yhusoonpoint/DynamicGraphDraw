# Libraries importing
library(shiny)
library(readxl)

#function return list of x  or y axis
getValues <- function(columnName, getWhat) 
{
  # import data
  df <- read_excel("U:\\Computer Science\\Year 2\\CE201-5-FY  Team Project Challenge\\IndividualProject\\ExamResults.xlsx", range = cell_rows(3:103))
  df <- data.frame(df) # converting the data to data frame to read it properly
  
  
  
  if(getWhat == "cn")
  {
    #gets the column name and removes the first element
    cn <- colnames(df)
    cn <- cn[-1]
    return(cn)
  }
  
  
  againstValues <- rep(0, 100)
  rowAvg <- rep(0, 100)
  startColomn <- 1
  avgmarks <- 1
  avgCount <- 0
  id <- 1
  x <- 1
  ss<- 1
  marks <- 1
  
  # adding the column of the required module to a list
  for(ag in columnName)
  {
    againstValues[id] = ag
    id = id + 1
  }
  id = 1
  
  ##looping through all the rows
  for(id in id:100)
  {
    ##splitting each row into column value
    for(rowValue in df[id,])
    {
      if(startColomn > 1)
      {
        if(rowValue != 'NULL')
        {
          rowAvg[id] = rowAvg[id] + as.numeric(rowValue)
          avgCount= avgCount + 1
        }
      }
      startColomn = startColomn + 1
    }
    if(againstValues[id] != 'NULL')
    {
      rowAvg[id] = rowAvg[id] - as.numeric(againstValues[id])
      avgCount = avgCount - 1
    }
    rowAvg[id] = rowAvg[id] / avgCount
    avgCount = 0
    startColomn = 1
  }
  id = 1
  
  ##checks which values need to be ignored and creates a fresh list
  for(each in againstValues)
  {
    if(each != 'NULL')
    {
      avgmarks[ss] = rowAvg[id]
      marks[ss] = as.numeric(each)
      ss = ss + 1
    }
    id = id + 1
  }
  
  

  if(getWhat == "x")
  {
    return(avgmarks) ##if x values are required, this is sent
  }
  else
  {
    return(marks)
  }
  
}

#displaying ui and creating combo box
ui <- basicPage( 
  selectInput(inputId = "top",
              label = "",
              "CSE101"),
  
  plotOutput("plot1"),
  selectInput(inputId = "bottom",
              label = "",
              "CSE102"),
  plotOutput("plot2"),
  verbatimTextOutput("info")
)

server <- 
  function(input, output, session) 
  {
    
    observe({
      updateSelectInput(session, "top", choices = getValues(NULL, "cn"))
      updateSelectInput(session, "bottom", choices = getValues(NULL, "cn"))
    })
    
    
    
    #plotting graph
    output$plot1 <- renderPlot(
      {
        x <- getValues(get(input$top,df), "x")
        y <- getValues(get(input$top,df), "y")
        plot(x,y,
             main='Module Performance Analyser',
             xlab='Average marks across all other modules',ylab= paste('Marks in ', toString(input$top), sep = " "),
             xlim=c(0,100), ylim=c(0,100),
             pch = 16, col = "dodgerblue1", cex = 1.5)
        ## fitting line of regression within the plot area
        clip(39,68, -100, 100)
        abline(lm(y~x),col='red')
      })
    #plotting second graph
    output$plot2 <- renderPlot(
      {
        x <- getValues(get(input$bottom,df), "x")
        y <- getValues(get(input$bottom,df), "y")
        plot(x,y,
             main='Module Performance Analyser',
             xlab='Average marks across all other modules',ylab= paste('Marks in ', toString(input$bottom), sep = " "),
             xlim=c(0,100), ylim=c(0,100),
             pch = 16, col = "dodgerblue1", cex = 1.5)
        clip(39,68, -100, 100)
        abline(lm(y~x),col='red')
    })
  }

shinyApp(ui, server) #shows gui