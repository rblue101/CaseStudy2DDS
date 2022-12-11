library(shiny)
library(tidyverse)

CaseStudy2_data <- read_csv("CaseStudy2-data.csv")
dataset = CaseStudy2_data
color = c('red','green','yellow')

ui = fluidPage(
    
    # app title
    titlePanel("Employee Attrition and Monthly Income"),
    
    # sidebar layout with input/output definitions
    sidebarLayout(
        
        # side bar panel for graphic parameters
        sidebarPanel(
            
            # selectInput for choosing variables
            selectInput(
                inputId = "data",
                label = "Predictor Variables",
                choices = list(
                    'MonthlyIncome',
                    'JobLevel',
                    'OverTime'
                )
            ),
            selectInput(
                inputId = "groups",
                label = "Display by Groups",
                choices = list(
                    'Attrition',
                    'Gender'
                )
            ),
            # input: slider for no. of bins
            
            
            # select discrete/categorical variable for grouping plots
            
            
            # select colors for plot display
            
            plotOutput(
                outputId = "aplot"
            ),
        ),
        
        # main panel for displaying plot
        mainPanel(
            
            
            # histogram output
            plotOutput(
                outputId = "histplot"
            ),
            plotOutput(
                outputId = "pplot"
            )
            
        )
        
    ),
    
)

# server function for creating app

server  = function(input,output){
    
    # renderPlot function is used to map the histogram inputs to main panel outputs
    # this plot is "reactive," i.e. changes when inputs (e.g. bins) are altered
    output$histplot = renderPlot({
        
        
        
        # creating histogram for output
        dataset |> ggplot(aes_string(x = input$data,fill=input$groups))+
            geom_histogram(stats="identity")+
            xlab(input$data)+
            ggtitle(paste("Histogram of",
                          input$data,
                          "faceted by",
                          sep=" ")
            )
    })
    output$pplot = renderPlot({
        
        
        # creating histogram for output
        dataset |> ggplot(aes_string(x = input$data,fill=input$groups))+
            geom_bar(position="fill")+
            xlab(input$data)+
            scale_y_continuous(labels = scales::percent)+
            ggtitle(paste("Histogram of",
                          input$data,
                          "faceted by",
                          sep=" ")
            )
    })
    
    output$aplot=renderPlot({
        dataset %>% ggplot(aes(x=Attrition,fill=Attrition))+ geom_bar()+
            ggtitle("Attrition Count") +
            xlab("Attrition")+ylab("Count")
    })
    
    output$value <- renderPrint({
        
    })
    
}

shinyApp(ui = ui, server = server)