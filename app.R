##############################################################################
# Shiny App to display Ebed data as Maps

# Choose to display raster using renderplot() and not leaflet() as more control on legend.
# Using leaflet() would allow to zoom more closely on zones to select pixels.

# Work to do :
# - Instead of loading .tif of zones, load EBED_v2017 and crop it with extent chosen by user. (Could take too long to crop)

# To deploy app on .io (Rstudio server)
# rsconnect::deployApp('C:/Users/thoralf/Documents/Sat_Data/2017.0/EBED')

# FT - 12/12/2019
##############################################################################

library(shiny)
library(shinybusy)
library(ggplot2)
library(raster)
library(leaflet)
library(rgdal)
library(mapview)

monthStart <- function(x) {#Function to reset slider input date to start of the month to fit with number of layers
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

einstein_values <- c(0.001,0.01,0.1,1,10,100)
einstein_values_log <- log10(einstein_values)

ebedmean <- raster('EBED_mean_v2017.tif')

## Creating Vector of data matching slider
start_date <- as.Date("2002-07-01")
end_date <- as.Date("2017-02-01")
da <- seq(start_date,end_date,by='month')
##

#pal <- colorNumeric(palette=rainbow(100), values(ebed_ras), na.color = "transparent")

ui <- fluidPage(
    titlePanel("EBED v.2017"),
    sidebarPanel(
      radioButtons("zoneInput", "Zone",choices = c("TasmanBay","NDunedin"),selected = "TasmanBay"),
      sliderInput(inputId ="slider", "Time", min = as.Date(start_date),max =as.Date(end_date),value=as.Date("2006-07-01"),timeFormat="%b %Y")
      #leafletOutput("sesync_map")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",  
                           h3("EBED mean overall the 2002-07 to 2017-02 dataset"),
                           plotOutput("meanplot",width='50%'),
                           h3("Monthly EBED mean"),
                           h5("Use the slider to display a cerain monthly mean."),
                           h5("Click on a pixel to extract Time Series from it."),
                           h5("Then go to the STL Decomposition section to see Seasonal/Trend components  (Take 30s)."),
                           plotOutput("coolplot",click=clickOpts(id = 'plot_click', clip = TRUE))),#outputplot),
                 
                  tabPanel("STL Decomposition", 
                           plotOutput("tsplot",width='100%'),
                           h5("If gap in Time Series, STL does not work."),
                           plotOutput("stlplot",width='100%'))
      
    ),
    add_busy_spinner(timeout=1000,color='blue')
    
  )
)


server <- function(input, output) {
  
  # observe({print(input$slider)}) #observe function to print reactive variables
  
  ## CHANGE HERE -- Correct bug of refresheing monthly plot not keeping the point on map and STL decomp
  # https://stackoverflow.com/questions/30991900/avoid-double-refresh-of-plot-in-shiny
  ## Set up buffert, to keep the click - input$plot_click$'x' for longitude in plot.  
  click_saved <- reactiveValues(singleclick = NULL)
  
  ## CHANGE HERE
  ## Save the click, once it occurs.
  observeEvent(eventExpr = input$plot_click, handlerExpr = { click_saved$singleclick <- input$plot_click })
  

  
  nlayer <- reactive({ #Reactive variable, link vector of date (for slider) to index of layer
    which(da==monthStart(input$slider))#Need to reset to first of the month
  })
  observe({print(nlayer())})
  
  ebed_ras <- reactive({ #Reactive variable
    stack(paste0("EBED_",input$zoneInput,"_v2017.tif"))
  })
  
  output$coolplot <- renderPlot({
    plot(log10(ebed_ras()[[nlayer()]]),col=rainbow(100),legend=FALSE,zlim=c(einstein_values_log[1],einstein_values_log[length(einstein_values_log)]))
    plot(log10(ebed_ras()[[nlayer()]]),zlim=c(einstein_values_log[1],einstein_values_log[length(einstein_values_log)]),col=rainbow(100), legend.only=TRUE,legend.width=1, legend.shrink=0.75,
         axis.args=list(at=einstein_values_log,
                        labels=einstein_values, 
                        cex.axis=0.6),
         legend.args=list(text='Ebed E/m2/d', side=4, font=2, line=2.5, cex=0.8))
         #rect(input$plot_click$'x',input$plot_click$'y',input$plot_click$'x',input$plot_click$'y',lwd=6,col='black') #Add rect in position of selected pixel (Bug as renderplot regenerate)
         rect(click_saved$singleclick$'x',click_saved$singleclick$'y',click_saved$singleclick$'x',click_saved$singleclick$'y',lwd=6,col='black') #Add rect in position of selected pixel (Bug as renderplot regenerate)
         
  })
  
  output$meanplot <- renderPlot({
    plot(log10(ebedmean),col=rainbow(100),legend=FALSE,zlim=c(einstein_values_log[1],einstein_values_log[length(einstein_values_log)]))
    plot(log10(ebedmean),zlim=c(einstein_values_log[1],einstein_values_log[length(einstein_values_log)]),col=rainbow(100), legend.only=TRUE,legend.width=1, legend.shrink=0.75,
         axis.args=list(at=einstein_values_log,
                        labels=einstein_values, 
                        cex.axis=0.6),
         legend.args=list(text='Ebed E/m2/d', side=4, font=2, line=2.5, cex=0.8))
    
    rect(extent(ebed_ras())[1],extent(ebed_ras())[3],extent(ebed_ras())[2],extent(ebed_ras())[4])
    text(extent(ebed_ras())[1],extent(ebed_ras())[3],labels=paste0(input$zoneInput),pos= 2)
    
  })
  #observe({print(input$plot_click$'x')})

  ebed_pix <- reactive({ #Reactive variable
    #if (is.null(input$plot_click)) {#Remove bug from click not existing at first
    if (is.null(click_saved$singleclick)) {#Remove bug from click not existing at first
        
      return(NULL)
    }
    
    #extract(ebed_ras(),cbind(input$plot_click$'x',input$plot_click$'y')) #Take a few seconds, add downloading bar?
    extract(ebed_ras(),cbind(click_saved$singleclick$'x',click_saved$singleclick$'y'))
    })
  
  output$tsplot <- renderPlot({
    if (is.null(ebed_pix())) {
      return()
    }
    plot(seq(1:176),ebed_pix()[1,],type='l',xlab='Time(months)',ylab='Ebed (E/m2/d)')

  })

## STL Decomposition
  
  # Time Series in ts object
  ebed_pix_ts <- reactive({ 
    ts(data=ebed_pix()[1,],start=c(2002,7),end=c(2017,2),frequency=12)
  })
  
  # STL and plot
  output$stlplot <- renderPlot({
    if (is.null(ebed_pix_ts())) {
      return()
    }
    decomp <- stl(ebed_pix_ts(),s.window=12,t.window=35) #t.window:span of the loess for trend, which number to choose?
    plot(decomp)    
  })
##
  
  
  # output[["sesync_map"]] <- renderLeaflet({
  #   leaflet() %>% addRasterImage(log10(ebed_ras[[input$layer]]),colors = pal,project=T)%>%
  #   addLegend(position='bottomright',pal = pal, values = values(ebed_ras))#values = values(ebed_ras),title = "EBED")
  # })
}


shinyApp(ui = ui, server = server)#No Code After this line