

### Shiny Packages and Data Load
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(ggplot2))
suppressMessages(library(gganimate))
suppressMessages(library(crosstalk))
suppressMessages(library(plotly))
suppressMessages(library(ggdark))
suppressMessages(library(DT))
suppressMessages(library(scales))
suppressMessages(library(ggthemes))
suppressMessages(library(RMySQL))
suppressMessages(library(shinythemes))
suppressMessages(library(dplyr))
suppressMessages(library(rsconnect))
creds <- read.csv("creds.csv")
user <- as.character(creds[,2:2])
password <- as.character(creds[,3:3])



### Get data from DB.  Creds read from local file
cn <- dbConnect(RMySQL::MySQL(), 
                
                username = user, 
                password = password, 
                host = "bike-spotter.ch4frojqjmlp.us-west-2.rds.amazonaws.com", 
                port = 3306,
                dbname = "bikes"
)

query <- "SELECT * FROM model_price_view"
df_model_price <- dbGetQuery(cn, query) %>% 
    select(-row_names)
dbDisconnect(cn)

### User Experience
ui <- bootstrapPage(theme = shinytheme("flatly"),
                    HTML('<meta name="viewport" content="width=1024">'),                   
                    h1("Bike Spotter", 
                       style = "font-family: 'Century Gothic',
        font-weight: 400; line-height: 1.1; 
        color: #FFFFFF;
        text-align:center;
        background-color:#2C3E50;
        height:50px;
        bottom-padding: 25px;
        margin: 0px;"),
                    
                    fluidRow( 
                        div("",style="height:35px")
                    ),
                    
                    fluidRow( 
                        
                        
                        column(12,
                               div(
                                   p("Using data from pinkbike.com ads, we can explore how different bike models are priced and how asking-prices change over time.
                                   This tool can help you quickly spot potential deals where bikes are priced below average.  
                                   Dataset does not include every ad on pinkbike, only the most popular models are captured within the United States", style = "font-size:140%"),
                                   style="display: width:100%; align = center; margin-left:15%;margin-right:15%"))
                        
                    ),
                    
                    fluidRow( 
                        div("",style="height:10px")
                    ),
                    
                    fluidRow( 
                        column(3,""),
                        column(3,align="center",
                               
                               pickerInput("select1", 
                                           h3("Select Brand", style = "font-size: 2vh"), 
                                           choices = unique(df_model_price$brand),
                                           selected = "Santa_Cruz")),
                        
                        column(3,align="center",
                               
                               pickerInput("select2", 
                                           h3("Select Model",style = "font-size:2vh"),
                                           choices = NULL,
                                           options = list(`actions-box` = TRUE),
                                           multiple = TRUE
                               )),
                        
                        column(3,"")
                    ),
                    
                    fluidRow( 
                        div("",style="height:35px")
                    ),
                    
                    # Output: Histogram ----
                    fluidRow(  
                        column(12,
                               div(p("Filter the data table by click-dragging over a point on the scatter plot.   Double-click plot to reset selection")
                                   ,style="font-size:100%; display: width:100%; align = left; margin-left:7%;margin-right:7%"))),
                    
                    fluidRow( 
                        
                        column(12, div(plotlyOutput("scatter"),style="display: width:100%; align = center; margin-left:7%; margin-right:7%;"))
                        
                    ),
                    fluidRow( 
                        div("",style="height:35px")
                    ),
                    fluidRow(
                        column(12, div(DTOutput('table'),style="margin-left:5%; margin-right:5%")),
                    )
                    
                    
                    
)

### Server 

server <- function(input, output,session) {
    
    
    filter_model <- reactive({
        filter(df_model_price, brand == input$select1)
    })
    
    observeEvent(filter_model(), {
        choices <- unique(filter_model()$model)
        updatePickerInput(session,"select2", choices = choices, selected = choices) 
    })
    
    
    
    data <- reactive({
        df_model_price %>% filter(model %in% input$select2)
    })
    
    shared_data <- highlight_key(data, key = ~url)
    
    
    output$scatter <- renderPlotly({
        
        shiny::validate(
            need(!is.null(input$select2), "Please make a selection...")
        )
        
        model_year <- reorder(shared_data$data()$year, desc(shared_data$data()$year))
        
        
        p <- ggplot(shared_data,aes(x = model_year, y = price, label = model)) + 
            geom_jitter(color = "#5DBC9B", size = 1)+
            stat_summary(fun.y=mean, geom="line",lwd=1,aes(group=2),color = "#2C3E50")+
            theme_fivethirtyeight()+
            theme(
                plot.title = element_text(color="#2C3E50", size=14, family="Lato"),
                plot.subtitle = element_text(color="#2C3E50", size=7, family="Lato"))+
            scale_y_continuous(labels = dollar)+
            labs(title = "Asking Price by Brand and Model")+
            xlab("Model Year")+
            ylab("Asking Price") 
        
        
        ggplotly(p, tooltip = c("model","price"))%>% 
            highlight(on = "plotly_selected", off = "plotly_deselect", persistent = FALSE) 
        
        
    })
    
    
    output$table <- DT::renderDataTable({
        
        shiny::validate(
            need(!is.null(input$select2), "Please make a selection...")
        )
        
        DT::datatable({
            shared_data$data(withSelection = TRUE) %>% filter(selected_ | is.na(selected_)) %>% 
                mutate(url = paste0("<a href='",url,"'",'target="_blank"',">",url,"</a>"))
        },escape = FALSE,extensions = 'Responsive',options = 
            list(searching = FALSE,paging = FALSE,
                 language = list(
                     zeroRecords = "Double click the scatter plot to update this table with your new selection")              
            ))})
    
    
}

### Execute
shinyApp(ui, server)