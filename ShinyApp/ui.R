# UI for MiLB Player Projections 
# 
# By Doug Duffy 2016

# Load packages
library(shiny)
library(DT)
shinyUI(fluidPage(
  # Year slider input
  absolutePanel(top = 10, left = 15, width = 200,
                sliderInput("year", "Year : ",
                            1992, 2016,
                            value = range(c(2016,2016)), step =1, sep =""
                )
                
                ),
  absolutePanel(top = 10, left = 250, width = 200,
                uiOutput("aff")
  ),
  absolutePanel(top = 10, left = 500, width = 200,
                uiOutput("lev")
  ),
  absolutePanel(top=100, left = 15, width= 1000,height = 500,
                
                uiOutput("hit")
                
  ),
  
  absolutePanel(top = 15, left = 815, width = 200,
                div(style="background:white; opacity:0.7",
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Logo.png", width = "40px", height= "35px")),
                    a( href = "http://dougduffy.com/",target = "_blank",
                       img(src = "Header.png",width = "155px", height= "35px")),
                    # Div for main styling
                    
                    div(style = "text-align:right; background-color:white; opacity:0.7;font-size: 12px;padding-right:4px", 
                        "Source :", 
                        a( href = "http://www.baseball-reference.com/",
                           style ="text-decoration:none",target = "_blank",
                           "Baseball-Reference"))
                )
  )
  
)
  
  
)
