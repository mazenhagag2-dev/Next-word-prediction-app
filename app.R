library(quanteda.textmodels)
library(ggplot2)
library(shiny)
library(quanteda)
library(stringr)
library(dplyr)

# Load corpus
corpus_data <- data_corpus_inaugural
all_text <- paste(as.character(corpus_data), collapse = " ")

# Clean text
tokens_data <- tokens(
  all_text,
  remove_punct = TRUE,
  remove_numbers = TRUE
)

words <- tolower(unlist(tokens_data))
dictionary <- unique(words)

# Bigram model
word1 <- words[-length(words)]
word2 <- words[-1]

bigrams <- data.frame(word1 = word1, word2 = word2)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(to right, #141E30, #243B55);
        color: white;
        font-family: Arial;
      }

      .title {
        text-align: center;
        font-size: 36px;
        font-weight: bold;
        color: #FFD700;
      }

      .box {
        background-color: rgba(255,255,255,0.15);
        padding: 30px;
        border-radius: 20px;
        margin-top: 20px;
      }

      .suggestion {
        display: inline-block;
        background: #00c6ff;
        color: white;
        padding: 10px 18px;
        margin: 5px;
        border-radius: 20px;
        font-size: 18px;
      }

      .form-control {
        font-size: 20px;
        border-radius: 10px;
      }
    "))
  ),
  
  div(class="title", "Smart Keyboard Prediction App"),
  
  div(class="box",
      
      h4("Name: Mazen Mohamed Elsayed | ID: 2405734"),
      
      textInput("input_text", "Type here:"),
      
      h3("Suggestions:"),
      
      uiOutput("suggestions")
  )
)

server <- function(input, output){
  
  output$suggestions <- renderUI({
    
    req(input$input_text)
    
    text <- tolower(input$input_text)
    
    suggestions <- c()
    
    # CASE 1: User pressed space → predict next word
    if(str_ends(text, " ")){
      
      words_input <- unlist(strsplit(trimws(text), " "))
      prev_word <- tail(words_input, 1)
      
      next_words <- bigrams %>%
        filter(word1 == prev_word) %>%
        count(word2, sort = TRUE)
      
      if(nrow(next_words) > 0){
        suggestions <- head(next_words$word2, 5)
      }
      
    } else {
      
      # CASE 2: User typing letters → autocomplete
      words_input <- unlist(strsplit(trimws(text), " "))
      current_word <- tail(words_input, 1)
      
      #  predict from 1 letter
      if(nchar(current_word) >= 1){
        prefix_matches <- dictionary[str_starts(dictionary, current_word)]
        suggestions <- head(prefix_matches, 5)
      }
    }
    
    if(length(suggestions) == 0){
      return(div("No suggestions"))
    }
    
    tagList(
      lapply(suggestions, function(word){
        div(class = "suggestion", word)
      })
    )
  })
}

shinyApp(ui = ui, server = server)
