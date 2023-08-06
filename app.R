# Load R packages
library(shiny)
library(shinythemes)
library("plotrix") 
source("functions.R")
library(Cairo)
library(grDevices)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",
                  "My first app",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Inputs:"),
                             sliderInput("num_attrs", 
                                          "No. of attributes", 3, min=3, max=10,
                                          step=1),
                             numericInput("attribute_scale", 
                                         "Max. attribute value", 5, min=1, 
                                         max=10, step=1),
                             textInput("graph_color", "Select graph color",
                                       value="Blue"),
                             uiOutput("go_buttons"),
                             width=4
                             
                           ), 
                           mainPanel(actionButton("action", "An action button"),
                                     downloadLink("downloadPlot", "Download Plot"),
                                     textOutput("txtout"),
                                     plotOutput("plot")) 
                  ), 
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  )
                ) 


server <- function(input, output) {
  
  output$txtout <- renderText({
    paste(input[[paste0("attr", 5)]])
  })
  
  output$go_buttons <- renderUI({
    buttons <- as.list(1:input$num_attrs)
    buttons <- lapply(buttons, function(i)
      fluidRow(
        div(style="display:inline-block; margin-left: 15px",
            textInput(paste0("attr_lab",i),paste("Attribute name:", i),
                      placeholder="Example", width=200)),
        div(style="display:inline-block; margin-left: 15px", 
            numericInput(paste0("attr_val",i), paste("Attribute value:", i),
                         value=input$attribute_scale, min=1,
                         max=input$attribute_scale, width=130))
      )
    )
  })
  observeEvent(input$action, {
    attr_vals <- c()
    attr_labels <- c()
    scale_max <- input$attribute_scale
    graph_col = input$graph_color
    for (index in 1:input$num_attrs) {
      attr_vals[index] = input[[paste0("attr_val", index)]]
      attr_labels[index] = input[[paste0("attr_lab", index)]]
    }
    output$plot <- renderPlot({
      create_plot(scale_max, attr_vals, attr_labels, graph_col)
    }, height = 800, width = 800)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste("kkk.pdf")},
    
    content = function(file){
      cairo_pdf(filename = file,
                width = 15, height = 15, pointsize = 18, family = "sans", 
                bg = "transparent", antialias = "subpixel",
                fallback_resolution = 300)
      attr_vals <- c()
      attr_labels <- c()
      scale_max <- input$attribute_scale
      for (index in 1:input$num_attrs) {
        attr_vals[index] = input[[paste0("attr_val", index)]]
        attr_labels[index] = input[[paste0("attr_lab", index)]]
      }
      create_plot(scale_max, attr_vals, attr_labels)
      dev.off()
    },
    
    contentType = "application/pdf"
  )
}


# Create Shiny object
shinyApp(ui = ui, server = server)
