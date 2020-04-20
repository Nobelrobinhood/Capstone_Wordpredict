#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(ggplot2)
library(RColorBrewer)
library(markdown)
library(quanteda)

source("model.r")

# 1. SERVER --------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  predicted_text1 <- reactive(next_word(input$text_in)[1])
  output$text_out_id1 <- predicted_text1
  observeEvent(input$button_add1, { 
    updateTextInput(session, "text_in",
                    value = paste(input$text_in, predicted_text1()))
  })
  
  predicted_text2 <- reactive(next_word(input$text_in)[2])
  output$text_out_id2 <- predicted_text2
  observeEvent(input$button_add2, { 
    updateTextInput(session, "text_in",
  value = paste(input$text_in, predicted_text2()))
  })
  
  predicted_text3 <- reactive(next_word(input$text_in)[3])
  output$text_out_id3 <- predicted_text3
  observeEvent(input$button_add3, { 
    updateTextInput(session, "text_in",
                    value = paste(input$text_in, predicted_text3()))
  })
  
  predicted_text4 <- reactive(next_word(input$text_in)[4])
  output$text_out_id4 <- predicted_text4
  observeEvent(input$button_add4, { 
    updateTextInput(session, "text_in",
                    value = paste(input$text_in, predicted_text4()))
  })
  
  predicted_text5 <- reactive(next_word(input$text_in)[5])
  output$text_out_id5 <- predicted_text5
  observeEvent(input$button_add5, { 
    updateTextInput(session, "text_in",
                    value = paste(input$text_in, predicted_text5()))
  })
  
  # Clear
  observeEvent(input$button_clear, {
    updateTextInput(session, "text_in",  value = "")
  })
  
  select_table <- reactive({
    eval(parse(text = input$ngrams))
  })
  output$ngrams_table <- renderDataTable(
    select_table(), options = list(pageLength = 10)
  )
  
  select_table2 <- reactive({
    eval(parse(text = input$ngrams2))
  })
  
  output$ggplot_ngram <- renderPlot({
    df <- head(select_table2() , 20)
    my_color <- brewer.pal(4, "Dark2")[c(1:4)[ngrams_names == input$ngrams2]]
    if(input$ngrams2 != "unigrams"){
      df$words <- paste(df$sentence, df$prediction, sep = "_")
    }
    ggplot(df, aes(reorder(words, probability), probability)) +
      geom_bar(stat = "identity", fill = my_color) +
      coord_flip() +
      xlab("") +
      ylab("") +
      theme_minimal()
  })
  
})

# 2. UI ------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  theme = shinytheme("superhero"),
  tags$hr(),
  titlePanel("WOORDVOORSPEL APP"),
  tags$hr(),
  
  mainPanel(tabsetPanel(
    #---------------------------------------------------------------------------
    tabPanel("Prediction",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$p(""),
          tags$h3("Predicted next word:"),
          flowLayout(
            actionButton("button_add1", label = textOutput("text_out_id1")),
            actionButton("button_add2", label = textOutput("text_out_id2")),
            actionButton("button_add3", label = textOutput("text_out_id3")),
            actionButton("button_add4", label = textOutput("text_out_id4")),
            actionButton("button_add5", label = textOutput("text_out_id5")),
            "",
            actionButton("button_clear",
                         label = "Clear",
                         icon = icon("refresh"))
          )
        ),
        mainPanel(
          tags$p(""),
          tags$h3("Please, enter your text in english:"),
          h4(tags$textarea(id = "text_in", rows = 10, cols = 40, "")))
     )),
    #---------------------------------------------------------------------------
    tabPanel("N-grams tables",
      sidebarLayout(
        sidebarPanel(
          selectInput("ngrams",
                      "N-gram table:",
                      ngrams_names)
        ),
        mainPanel(dataTableOutput("ngrams_table"))
      )
    ),
    #---------------------------------------------------------------------------
    tabPanel("N-grams Plot",
      sidebarLayout(
        sidebarPanel(
          selectInput("ngrams2",
                      "N-gram table:",
                      ngrams_names)
        ),
        mainPanel(plotOutput("ggplot_ngram"))
      )
    ),
    #---------------------------------------------------------------------------
    tabPanel('About', includeMarkdown('README.md'))
  ))
))

# 2. APP -----------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
