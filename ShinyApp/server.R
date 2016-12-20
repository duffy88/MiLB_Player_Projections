# Server for MiLB Player Projections 
# 
# By Doug Duffy 2016

# Load Packages
library(shiny)
library(DT)

# Load prepared dataset
milbShiny <- readRDS("data/milbShiny.rds")
milbShinyP <- readRDS("data/milbShinyP.rds")

targetLev <- c("AAA","AA","A+","A","A-","Rk+","Rk","FRk")




server <- function(input, output, session) {
  
  # Dynamically Render List of MLB Affiliates
  output$aff <- renderUI({
    
      selectInput("aff", "MLB Affiliate :", 
                  c("All Teams",sort(unique(milbShiny$Aff))),
                  selected = "All Teams"
      )
    
    
  })
  
  
  # Dynamically Render List of MiLB Levels
  output$lev <- renderUI({
      selectInput("lev", "MiLB Level :", 
                  c("All Levels",unique(milbShiny$Lev)[match(targetLev, unique(milbShiny$Lev))]),
                  selected = "All Levels"
      )
    
    
  })
  
  
  
  data <- reactive({
    if(input$aff=="All Teams" & input$lev =="All Levels"){
      df <- milbShiny[ milbShiny$Year >= input$year[1] &
                         milbShiny$Year <= input$year[2]  , ]
      return(df)
    }else if(input$aff=="All Teams"){
      df <- milbShiny[ milbShiny$Year >= input$year[1] &
                         milbShiny$Year <= input$year[2] &
                         milbShiny$Lev ==input$lev, ]
      return(df)
    }else if(input$lev=="All Levels"){
      df <- milbShiny[ milbShiny$Year >= input$year[1] &
                         milbShiny$Year <= input$year[2] &
                         milbShiny$Aff ==input$aff, ]
      return(df)
    }else {
      df <- milbShiny[ milbShiny$Year >= input$year[1] &
                         milbShiny$Year <= input$year[2] &
                         milbShiny$Aff ==input$aff &
                         milbShiny$Lev ==input$lev , ]
      return(df)
    }
    
    
  })
  
  
  dataP <- reactive({
    if(input$aff=="All Teams" & input$lev =="All Levels"){
      df <- milbShinyP[ milbShinyP$Year >= input$year[1] &
                         milbShinyP$Year <= input$year[2]  , ]
      return(df)
    }else if(input$aff=="All Teams"){
      df <- milbShinyP[ milbShinyP$Year >= input$year[1] &
                         milbShinyP$Year <= input$year[2] &
                         milbShinyP$Lev ==input$lev, ]
      return(df)
    }else if(input$lev=="All Levels"){
      df <- milbShinyP[ milbShinyP$Year >= input$year[1] &
                         milbShinyP$Year <= input$year[2] &
                         milbShinyP$Aff ==input$aff, ]
      return(df)
    }else {
      df <- milbShinyP[ milbShinyP$Year >= input$year[1] &
                         milbShinyP$Year <= input$year[2] &
                         milbShinyP$Aff ==input$aff &
                         milbShinyP$Lev ==input$lev , ]
      return(df)
    }
    
    
  })
  
  # Generate an HTML table view of the data
  output$HitterTable <- renderDataTable({
    if(!is.null(data())){
      datatable(data(), escape = FALSE, rownames=FALSE)%>%
        formatStyle("Name",   `font-size` = '14px', 'text-align'= 'left')%>%
       formatStyle("Pred oWAR",   fontWeight = 'bold', `font-size` = '20px')%>%
        formatStyle(names(data()),   `font-size` = '14px', 'text-align'= 'center')
      
    }
    
  }, escape = FALSE, rownames=FALSE)
  #%>%
   # formatStyle('Sepal.Length',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
  
  # Generate an HTML table view of the data
  output$PitcherTable <- renderDataTable({
    if(!is.null(dataP())){
      datatable(dataP(), escape = FALSE, rownames=FALSE)%>%
        formatStyle("Name",   `font-size` = '14px', 'text-align'= 'left')%>%
        formatStyle("Pred WAR",   fontWeight = 'bold', `font-size` = '20px')%>%
        formatStyle(names(dataP()),   `font-size` = '14px', 'text-align'= 'center')
      
      
    }
    
  })
  
  #Dynamically render table
  output$hit <- renderUI({
    if(!is.null(data()) & !is.null(dataP())){
      mainPanel(width = 1000,
                tabsetPanel(
                  tabPanel("Hitters", dataTableOutput("HitterTable")),
                  tabPanel("Pitchers", dataTableOutput("PitcherTable"))
                )
      )
    }
  })
  
}


