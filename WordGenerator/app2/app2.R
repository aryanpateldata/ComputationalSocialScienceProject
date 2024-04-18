library(shiny)
library(ggplot2)
library(readr)



model <- readRDS("/Users/aryanpatel/Documents/Aryan:Classes:SOC360/ComputionalProjectAryanandGurnit/CompProject/WordGenerator/app2/word_embeddings_model.rds")

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
    input_word <- input$input_word
    
    # Find similar words using pre-trained model
    similar_words <- predict(model, input_word, type = "nearest", top_n = 10)
    similar_words_df <- similar_words[[1]]
    similar_words_df <- similar_words_df[order(similar_words_df$similarity, decreasing = TRUE),]
    
    output$similar_words_plot <- renderPlot({
      ggplot(similar_words_df, aes(x = X2, y = X3)) +
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
