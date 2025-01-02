library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Confiscated Animal Identification"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("activity", 
                  "Choose a Confiscation Record:", 
                  choices = c("Activity 1", "Activity 2")),
      radioButtons("animal_choice", 
                   "Compare the confiscated DNA sequence with the known endangered species.  Which animal was being traded illegally?  When you've done the first, choose the second activity.", 
                   choices = c("Tiger", "Domestic Cat", "Lion", "Leopard", "Jaguar"),
                   selected = character(0)),  # Ensure no default selection
      actionButton("submit", "Submit Answer"),
      actionButton("reset", "Reset")  # Reset button
    ),
    
    mainPanel(
      # Image display
      uiOutput("image_ui"),
      
      # Display result of identification
      uiOutput("result")  # Use renderUI here instead of renderText
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Explicitly add resource path for images
  addResourcePath("images", "www/images")
  
  # Reactive to hold the selected image path based on dropdown selection
  selected_image <- reactive({
    if (input$activity == "Activity 1") {
      return("images/activity1.jpg")  # Path to Activity 1 image
    } else {
      return("images/activity2.jpg")  # Path to Activity 2 image
    }
  })
  
  # Display the selected image
  output$image_ui <- renderUI({
    img(src = selected_image(), height = "300px")
  })
  
  # Reactive value to hold the correct answers
  correct_answers <- reactive({
    if (input$activity == "Activity 1") {
      return("Jaguar")  # Correct answer for Activity 1
    } else {
      return("Leopard")  # Correct answer for Activity 2
    }
  })
  
  # Result output: Check if the user's choice is correct
  observeEvent(input$submit, {
    result_text <- if (input$animal_choice == correct_answers()) {
      "CORRECT"
    } else {
      "INCORRECT"
    }
    
    result_color <- if (input$animal_choice == correct_answers()) {
      "green"
    } else {
      "orange"
    }
    
    output$result <- renderUI({
      # Display the result in the selected color and styled with larger font
      tags$div(
        style = "text-align: center; font-size: 24px; margin-top: 20px;",
        tags$span(style = paste("color:", result_color, ";"), result_text)
      )
    })
  })
  
  # Automatically reset when activity changes
  observeEvent(input$activity, {
    # Reset radio button and result when activity changes
    updateRadioButtons(session, "animal_choice", selected = character(0))  # Explicitly reset radio buttons
    output$result <- renderUI({})  # Clear result
  })
  
  # Reset button: Clear inputs and results
  observeEvent(input$reset, {
    # Reset radio button and result
    updateRadioButtons(session, "animal_choice", selected = character(0))  # Clear radio buttons
    updateSelectInput(session, "activity", selected = "Activity 1")  # Reset activity dropdown to default
    output$result <- renderUI({})  # Clear result
  })
}

# Run the app
shinyApp(ui = ui, server = server)
