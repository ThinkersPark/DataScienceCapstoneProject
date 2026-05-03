library(stringr)
library(shiny)
## setwd("C:/Users/Oliwia Kozlowska/Documents/R/RworkCoursera/Capstone/NextWordApp")

ui <- shinyUI(fluidPage(
  # Add custom CSS for iPhone SMS-like styling
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        margin: 0;
        padding: 20px;
        min-height: 100vh;
      }
      
      .phone-container {
        max-width: 500px;
        margin: 0 auto;
        background: #f5f5f5;
        border-radius: 40px;
        box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
        padding: 12px;
        border: 8px solid #000;
        overflow: hidden;
        display: flex;
        flex-direction: column;
        height: 800px;
      }
      
      .phone-header {
        background: #f5f5f5;
        padding: 20px;
        text-align: center;
        border-bottom: 1px solid #e0e0e0;
      }
      
      .phone-header h4 {
        margin: 0;
        color: #333;
        font-size: 24px;
        font-weight: 600;
      }
      
      .message-thread {
        height: 200px;
        overflow-y: auto;
        background: #fff;
        padding: 15px;
        display: flex;
        flex-direction: column;
      }
      
      .message-bubble-ai {
        background: #e5e5ea;
        color: #000;
        border-radius: 18px;
        padding: 12px 16px;
        margin: 8px 0;
        margin-left: auto;
        max-width: 80%;
        word-wrap: break-word;
        font-size: 14px;
        align-self: flex-start;
      }
      
      .message-bubble-user {
        background: #007aff;
        color: white;
        border-radius: 18px;
        padding: 12px 16px;
        margin: 8px 0;
        max-width: 80%;
        word-wrap: break-word;
        font-size: 14px;
        align-self: flex-end;
      }
      
      .content-wrapper {
        flex: 1;
        overflow-y: auto;
        background: #fff;
        display: flex;
        flex-direction: column;
        padding: 0;
      }
      
      .suggestion-container {
        display: flex;
        gap: 10px;
        margin: 10px 0;
        justify-content: center;
        padding: 0 12px;
      }
      
      .suggestion-btn {
        background: #e5e5ea !important;
        color: #007aff !important;
        border: 2px solid #007aff !important;
        border-radius: 20px !important;
        padding: 8px 14px !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        cursor: pointer !important;
        transition: all 0.3s ease !important;
      }
      
      .suggestion-btn:hover {
        background: #007aff !important;
        color: white !important;
      }
      
      .input-section {
        background: #fff;
        padding: 12px;
        border-top: 1px solid #e0e0e0;
        display: flex;
        gap: 8px;
        align-items: flex-end;
      }
      
      .input-section textarea {
        flex: 1;
        border: 2px solid #d0d0d0 !important;
        border-radius: 20px !important;
        padding: 10px 14px !important;
        font-size: 14px !important;
        font-family: inherit !important;
        resize: none;
        max-height: 150px;
        height: 120px !important;
      }
      
      .input-section textarea:focus {
        outline: none !important;
        border: 2px solid #007aff !important;
      }
      
      .learn-btn {
        background: #e5e5ea !important;
        color: #007aff !important;
        border: 2px solid #007aff !important;
        border-radius: 20px !important;
        padding: 10px 16px !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        cursor: pointer !important;
        transition: all 0.3s ease !important;
        width: auto !important;
        height: auto !important;
      }
      
      .learn-btn:hover {
        background: #007aff !important;
        color: white !important;
      }
      
      .title-row {
        background: #fff;
        padding: 15px 12px;
        text-align: center;
        font-weight: 600;
        color: #666;
        font-size: 13px;
        border-bottom: 1px solid #e0e0e0;
      }
      
      .button-section {
        background: #fff;
        padding: 12px;
        border-top: 1px solid #e0e0e0;
        text-align: center;
      }
      
      .feedback-section {
        height: 200px;
        overflow-y: auto;
        background: #fff;
        padding: 15px;
        border-top: 1px solid #e0e0e0;
        display: flex;
        flex-direction: column;
      }
      
      .feedback-bubble {
        background: #e5e5ea;
        color: #000;
        border-radius: 18px;
        padding: 12px 16px;
        margin: 8px 0;
        max-width: 80%;
        word-wrap: break-word;
        font-size: 14px;
        align-self: flex-start;
      }
      
      /* Scrollbar styling */
      .message-thread::-webkit-scrollbar,
      .content-wrapper::-webkit-scrollbar,
      .feedback-section::-webkit-scrollbar {
        width: 6px;
      }
      
      .message-thread::-webkit-scrollbar-track,
      .content-wrapper::-webkit-scrollbar-track,
      .feedback-section::-webkit-scrollbar-track {
        background: #f1f1f1;
      }
      
      .message-thread::-webkit-scrollbar-thumb,
      .content-wrapper::-webkit-scrollbar-thumb,
      .feedback-section::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 3px;
      }
      
      .message-thread::-webkit-scrollbar-thumb:hover,
      .content-wrapper::-webkit-scrollbar-thumb:hover,
      .feedback-section::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
    "))
  ),
  
  # Main container
  div(class = "phone-container",
      # Header
      div(class = "phone-header",
          h4("EASY WRITING APP")
      ),
      
      # Message thread area
      div(class = "message-thread",
          div(class = "message-bubble-ai",
              "Start typing and I'll suggest your next word!"
          ),
          div(style = "flex-grow: 1;") # Spacer to push next content down
      ),
      
      # Content wrapper - centered content
      div(class = "content-wrapper",
          # Suggestions title
          div(class = "title-row",
              "Helpful next word suggestions - tap to pick"
          ),
          
          # Suggestion buttons
          div(class = "suggestion-container",
              actionButton("button1", label = textOutput("W1Out"), class = "suggestion-btn"),
              actionButton("button2", label = textOutput("W2Out"), class = "suggestion-btn"),
              actionButton("button3", label = textOutput("W3Out"), class = "suggestion-btn")
          ),
          
          # Input section
          div(class = "input-section",
              textAreaInput("inText", 
                            label = NULL, 
                            placeholder = "Start writing...",
                            height = "120px", 
                            width = "100%"
              )
          ),
          
          # Learn button section
          div(class = "button-section",
              actionButton("buttonLearn", label = "Make me learn!", class = "learn-btn")
          )
      ),
      
      # Learning feedback - formatted like message thread
      div(class = "feedback-section",
          div(class = "feedback-bubble",
              textOutput("learnText")
          )
      )
  )
))


server <- function(input, output, session) {
  # Your server code here
  
  load("./data/training_set_app_load.RData"); training_set_app <- training_set_app_load
  load("./data/kn_model_app_load.RData"); kn_model_app <- kn_model_app_load
  source("./R/knFunctions.R")
  
  # Create reactive values to store the updated training set and model
  reactiveData <- reactiveValues(
    training_set_app = training_set_app,
    kn_model_app = kn_model_app
  )
  
  learntData <- reactive({
    msg <- ""
    if (input$inText != "") {
      
      training_set_app_updated <- c(reactiveData$training_set_app, input$inText)
      dtngram_app_updated <- build_ngram_counts(training_set_app_updated)
      kn_comps_app_updated <- compute_kn_components(dtngram_app_updated)  # FIX: was dtngram_app
      kn_model_app_updated <- make_kn_probability_model(kn_comps_app_updated)
      
      msg <- paste("I have learnt from your text: ", training_set_app_updated[length(training_set_app_updated)], ". Try me again!")
      
    } else {
      
      training_set_app_updated <- reactiveData$training_set_app
      kn_model_app_updated <- reactiveData$kn_model_app
      msg <- "Please write something I can learn from!" ## reactiveData$training_set_app[length(reactiveData$training_set_app)]
      
    }
    
    learnList <- list(
      training_set_app_updated = training_set_app_updated, 
      kn_model_app_updated = kn_model_app_updated, 
      msg = msg
    )
    
    return(learnList)
  }) %>% bindEvent(input$buttonLearn)
  
  # Update reactive values when learntData changes
  observe({
    reactiveData$training_set_app <- learntData()$training_set_app_updated
    reactiveData$kn_model_app <- learntData()$kn_model_app_updated
  })
  
  output$learnText <- renderText(learntData()$msg)
  
  predictWords <- reactive({
    if (input$inText != "") {
      
      intxt <- input$inText
      intxt <- str_replace_all(intxt, "[[:punct:]]", " ")
      intxt <- str_replace_all(intxt, "[[:digit:]]", " ")
      intxt <- str_replace_all(intxt, "  ", " ")
      intxt <- gsub("\\s+$", "", intxt)
      intxt <- tolower(intxt)
      intxt <- tail(unlist(strsplit(intxt, split=" ")), n=3)
      
      nextWords <- predict_next_word(reactiveData$kn_model_app, intxt, topk = 3, fallback_top_unigram = 100) 
      
      return(names(nextWords))
    }
    
  })
  
  output$W1Out <- renderText({predictWords()[1]})
  output$W2Out <- renderText({predictWords()[2]})
  output$W3Out <- renderText({predictWords()[3]})
  
  observe({
    updateTextAreaInput(session, "inText",
                        value = paste(input$inText, predictWords()[1]))
  }) %>% bindEvent(input$button1)
  
  observe({
    updateTextAreaInput(session, "inText",
                        value = paste(input$inText, predictWords()[2]))
  }) %>% bindEvent(input$button2)
  
  observe({
    updateTextAreaInput(session, "inText",
                        value = paste(input$inText, predictWords()[3]))
  }) %>% bindEvent(input$button3)
  
}

shinyApp(ui = ui, server = server)
