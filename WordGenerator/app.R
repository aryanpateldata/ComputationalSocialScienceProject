
library(shiny)
library(ggplot2)
library(readr)
library(gtsummary)
library(dplyr)
library(gt)
library(scales)
library(ggthemes)


similarity_matrix_url <- "https://github.com/apat010/ComputationalSocialScienceProject/raw/main/WordGenerator/similarity_matrix.rds"
similarity_matrix <- readRDS(url(similarity_matrix_url))



ui <- fluidPage(
  titlePanel("Similar Word Generator"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_word", "Enter a word (**Must be in lowercase**):", ""),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(

      tags$img(src = "https://raw.githubusercontent.com/apat010/ComputationalSocialScienceProject/baebf0c7b551be07f80fb06f35c39eb54216a7a4/WordGenerator/Screenshot%202024-04-25%20at%2011.18.26%E2%80%AFAM.png",
               alt = "Logo",
               width = "75%"),  
      plotOutput("similar_words_plot", width = "1200px", height = "700px"),
      uiOutput("similar_words_table", width = "1200px", height = "700px")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$predict_button, {
    input_word <- input$input_word
    

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
    

    similar_words_indices <- order(similarity_matrix[input_word, ], decreasing = TRUE)[2:11]  
    similar_words <- rownames(similarity_matrix)[similar_words_indices]
    similarity_scores <- similarity_matrix[input_word, similar_words_indices]
    
    similar_words_df <- data.frame(Word = similar_words, Similarity = similarity_scores)
    
    output$similar_words_plot <- renderPlot({
      ggplot(similar_words_df, aes(x = reorder(Word, -Similarity), y = Similarity, fill = Similarity)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Top Similar Words to '", input_word, "'"), x = "Word", y = "Similarity Score") +
        theme_solarized_2() + 
        scale_fill_gradient(low = "lightgreen", high = "darkgreen") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
              axis.text.y = element_text(size = 15),
              plot.title = element_text(face = "bold", size = 30, colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA, size = 7), 
              axis.title.x = element_text(face = "bold", size = 20, color = "black"), 
              axis.title.y = element_text(face="bold", size = 20, color = "black"), 
              legend.title = element_text(face="bold", size = 15, color = "black"), 
              plot.margin = margin(t = 0, r = 0, b = 50, l = 0, unit = "pt"),
              axis.text.x.bottom = element_text(face="bold", size = 15, color = "black", angle = 90, hjust = 1), 
              axis.text.y.left = element_text(face="bold", size= 15, color = "black"), 
              legend.text = element_text(face="bold", size = 15, color = "black"),
              panel.grid.major = element_line(color = "black", size = 0.5),  
              panel.grid.minor = element_line(color = "black", size = 0.25))
    })
    
   
    green_gradient <- colorRampPalette(c("lightgreen", "darkgreen"))(10)
    
    output$similar_words_table <- renderUI({

      gt_table <- similar_words_df |>
        select(Word, Similarity) |>
        gt() |>
        tab_header(
          title = "Similarity Table",
          subtitle = "Table showing most similar words to the inputted word"
        )
      

      gt_table <- gt_table |>
        tab_style(
          style = cell_text(weight = "bold", font = "Times New Roman"),
          locations = cells_title(groups = "title")
        ) |>
        tab_style(
          style = list(cell_fill(color = "black"), cell_text(weight = "bold")),
          locations = cells_body(columns = Similarity)
        ) |>
        tab_style(
          style = cell_borders(sides = "all", color = "black", weight = px(4)),  
          locations = cells_body()
        ) |>
        tab_style(
          style = cell_borders(sides = "all", color = "black", weight = px(4)),  
          locations = cells_column_labels()
        ) |>
        tab_style(
          style = cell_borders(sides = "all", color = "black", weight = px(10)),  
          locations = cells_title(groups = "title")
        ) 
      

      for (i in seq_along(green_gradient)) {
        gt_table <- gt_table |>
          tab_style(
            style = list(cell_fill(color = green_gradient[i]), cell_text(color = "black")),
            locations = cells_body(columns = "Word", rows = i)
          )
      }
      

      gt_table <- gt_table |>
        tab_style(
          locations = cells_body(
            columns = `Similarity`,
            rows = `Similarity` > 0.3
          ),
          style = list(cell_text(color = 'darkred'))
        ) |>
        
        tab_style(
          locations = cells_body(
            columns = `Similarity`,
            rows = `Similarity` > 0.4
          ),
          style = list(cell_text(color = 'red'))
        ) |>
        
        tab_style(
          locations = cells_body(
            columns = `Similarity`,
            rows = `Similarity` > 0.5
          ),
          style = list(cell_text(color = 'pink'))
        ) |>
        
        tab_style(
          locations = cells_body(
            columns = `Similarity`,
            rows = `Similarity` > 0.6
          ),
          style = list(cell_text(color = "white"))
        ) |>
        
        
        tab_style(
          style = cell_text(weight = "bold", font = "Times New Roman"),
          locations = cells_body(columns = "Word")
        ) |>
        
        tab_style(
          style = cell_text(weight = "bold", font = "Times New Roman", color = "black"),
          locations = cells_column_labels(columns = everything())
        ) |>
        
        tab_style(
          style = list(cell_fill(color = "green"), cell_text(weight = "bold")),
          locations = cells_column_labels(columns = everything()))|>
        
        tab_style(
          style = list(cell_fill(color = "green"), cell_text(weight = "bold")),
          locations = cells_title(groups = "title")) 
  
   
      gt_table
    })
    
    
#w
    
  })
  
  output$instruction <- renderText({
    "Enter a word and click 'Predict' to find the closest words."
  })
}

shinyApp(ui = ui, server = server)
