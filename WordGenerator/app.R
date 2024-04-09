library(shiny)
library(ggplot2)
library(word2vec)


ui <- fluidPage(
  titlePanel("Word Similarity Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_word", "Enter a word:", ""),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(
      plotOutput("similar_words_plot"),
      tableOutput("similar_words_table")
      )
  )
)


server <- function(input, output) {
  observeEvent(input$predict_button, {

    data <- read.csv("data.csv")
    

    data <- as.data.frame(data)
    

    data$lyrics <- tolower(data$lyrics)
    

    model <- word2vec(x = data$lyrics, 
                      type = "cbow",
                      dim = 100,
                      window = 3,
                      iter = 10,
                      negative = 10,
                      min_count = 10)
    

    input_word <- input$input_word
    

    similar_words <- predict(model, input_word, type = "nearest", top_n = 10)
    

    similar_words_df <- similar_words[[1]]
    
    
    similar_words_df <- similar_words_df[order(similar_words_df$similarity, decreasing = TRUE),]
    

    output$similar_words_plot <- renderPlot({
      ggplot(similar_words_df, aes(x = similar_words_df[,2], y = similar_words_df[,3])) +
        geom_bar(stat = "identity", fill="green") +
        labs(title = paste("Top Similar Words to '", input_word, "'"), x = "Word", y = "Similarity Score") +
        theme_classic()+ 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
              axis.text.y = element_text(size = 15),
              plot.title = element_text(face = "bold", size = 25))
 
      })
 
    output$similar_words_table <- renderTable({
      similar_words_df
    })  
  
 })
  
  
  
  output$instruction <- renderText({
    "Enter a word and click 'Predict' to find the closest words."
  })
}


shinyApp(ui = ui, server = server)
