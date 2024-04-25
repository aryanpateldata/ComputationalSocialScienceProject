# Libraries required for Shiny app
library(shiny)
library(ggplot2)
library(readr)
library(gtsummary)
library(dplyr)
library(gt)
library(scales)

# Load similarity matrix
similarity_matrix_url <- "https://github.com/apat010/ComputationalSocialScienceProject/raw/main/WordGenerator/similarity_matrix.rds"
similarity_matrix <- readRDS(url(similarity_matrix_url))

# UI layout
ui <- fluidPage(
  titlePanel("Word Similarity Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_word", "Enter a word:", ""),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(
      htmlOutput("display_image"),
      plotOutput("similar_words_plot"),
      uiOutput("similar_words_table")
    )
  )
)

# Server logic
server <- function(input, output) {
  observeEvent(input$predict_button, {
    input_word <- input$input_word
    
    # Check if the input word exists in the vocabulary
    if (!(input_word %in% rownames(similarity_matrix))) {
      output$similar_words_plot <- renderPlot({
        ggplot() +
          geom_bar(stat = "identity", fill = "red", aes(x = "", y = 0)) +
          labs(title = "The Word you have entered, the model has not been trained on yet.", x = "", y = "") +
          theme_void()
      })
      
      output$similar_words_table <- renderTable({
        data.frame(Word = character(0), Similarity = numeric(0))
      })
      
      return()
    }
    
    # Get similar words and their similarities from the similarity matrix
    similar_words_indices <- order(similarity_matrix[input_word, ], decreasing = TRUE)[2:11]  # Exclude the first similar word
    similar_words <- rownames(similarity_matrix)[similar_words_indices]
    similarity_scores <- similarity_matrix[input_word, similar_words_indices]
    
    similar_words_df <- data.frame(Word = similar_words, Similarity = similarity_scores)
    
    output$similar_words_plot <- renderPlot({
      ggplot(similar_words_df, aes(x = reorder(Word, -Similarity), y = Similarity)) +
        geom_bar(stat = "identity", fill = "green") +
        labs(title = paste("Top Similar Words to '", input_word, "'"), x = "Word", y = "Similarity Score") +
        theme_classic() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
              axis.text.y = element_text(size = 15),
              plot.title = element_text(face = "bold", size = 25))
    })
    
    output$similar_words_table <- renderUI({
      similar_words_df |>
        select(Word, Similarity) |>
        gt() |>
        tab_header(
          title = "Similarity Table",
          subtitle = "Table showing most similar words to the inputted word"
        ) |>
        
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_title(groups = "title")
        ) |>
        
        
        
        tab_style(
          style = list(cell_fill(color = "#33FF66"),
                       cell_text(weight = "bold")),
          locations = cells_body(columns = Similarity)
        ) |> 
        
        tab_style(
          style = cell_text(weight = "bold"), 
          locations = cells_body(columns = Word)
        ) |>
        
        
        tab_style(
          style = cell_text(weight = "bold"), 
          locations = cells_column_labels(columns = everything()))
      
      
    })
    
    # Display the image in the output
    output$display_image <- renderUI({
      img(src = "https://raw.githubusercontent.com/apat010/ComputationalSocialScienceProject/baebf0c7b551be07f80fb06f35c39eb54216a7a4/WordGenerator/Screenshot%202024-04-25%20at%2011.18.26%E2%80%AFAM.png", 
          alt = "Logo", 
          width = "75%")  # Adjust the size as needed
    })
    
  })
  
  output$instruction <- renderText({
    "Enter a word and click 'Predict' to find the closest words."
  })
}

shinyApp(ui = ui, server = server)
