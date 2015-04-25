library(shiny)

# Define UI for application that draws a histogram

shinyUI(fluidPage(


  h3("Rainfall and Temperature",position="center"),
  
 
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", 
                  label = "Choose a dataset:",
                  choices = c("Temperature", "Rainfall"),
                  selected = "Rainfall"),
    
    br(),
    
    h5("Click on the download button to download"),
    h5("the data (the data is large and may take time)"),
    
    br(),
    
    radioButtons("type","Download Format:",
                 choices=c("Excel (CSV)", "Text (TSV)","Text (Space Separated)")),
    

  
  downloadButton('downloadData','Download')
  
    ),
    
    mainPanel(
      h2(strong(em("African Rainfall and Temperature Climatology"))),

      
      p(strong(em('This app calculates monthly climatology (long term mean) and trend of temperature and rainfall over any selected rectangular region. '))),
      
      p(em("Click two opposite corners of a region over the landmass of Africa to get the area average monthly temperature or precipitation and its trend. You can select temperature or rainfall from the 'choose a dataset' input control on the top left of the sidebar. Please make sure that your rectangular region lies completely on the landmass.You can see the results for the region you selected by clicking the tabPanels at the bottom. You can also download the data if you are interested.")),
      
      p(em("The rainfall and temperature data cover the time period from 1980-2013 and they are from the Climate Research Unit (CRU).")),
      h6(em("Kind thanks!!")),
      br(),
      
      
      
      plotOutput("distPlot",clickId = "plot_click",width = "100%", height = "400px"),

      
      tabsetPanel(type="tab",
                  tabPanel("Climatology Plot of the Selected Region",
                           plotOutput("distPlot2",width = "100%", height = "400px")
                  ),
                  tabPanel("Monthly Data Over the Selected Region",
                           tableOutput("dat")),
                  tabPanel("Trend over the Selected Region",
                           plotOutput("trendaf",width = "100%", height = "400px")),
                  
                  tabPanel("Selected Region Coordinates",
                           h5('The Approximate Rectangular Region You Selected in (Longitude1 Latitude1 Longitude2 Latitude2) is: '),
                           textOutput("rang")))
    ))))