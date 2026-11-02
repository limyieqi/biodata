library(shiny)

drinks <- c("coffee", "tea", "watermelon", "orange", "plain water")

ui <- fluidPage(
  titlePanel("Friend Biodata Collection Portal"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "What is your name?"),
      textAreaInput("story", "Tell me about yourself", rows = 3),
      sliderInput('age', 'Age', min = 15, max = 100, value = 25),
      dateInput("DOB", "Birth date"),
      radioButtons("drink", "What's your favourite drink?", drinks),
      br(),
      actionButton("submit", "Submit Biodata", class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      textOutput("text"),
      br(),
      h4("Collected Biodata (Static Table):"),
      tableOutput("static"),
      br(),
      h4("Collected Biodata (Interactive Table):"),
      dataTableOutput("dynamic")
    )
  )
)

server <- function(input, output, session) {
  
  # Greeting text
  output$text <- renderText({"Hi, nice to see you! Please submit your details using the sidebar."})
  
  # Initialize a reactive data frame to store submissions
  biodata_db <- reactiveVal(
    data.frame(
      Name = character(),
      Story = character(),
      Age = integer(),
      DOB = character(),
      Favorite_Drink = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Observe when the submit button is clicked
  observeEvent(input$submit, {
    req(input$name)
    
    if (trimws(input$name) == "") {
      showNotification("Please enter your name!", type = "error")
      return()
    }
    
    # Create new row from inputs
    new_row <- data.frame(
      Name = input$name,
      Story = input$story,
      Age = input$age,
      DOB = as.character(input$DOB),
      Favorite_Drink = input$drink,
      stringsAsFactors = FALSE
    )
    
    # Append to the reactive data frame
    updated_db <- rbind(biodata_db(), new_row)
    biodata_db(updated_db)
    
    showNotification("Biodata successfully recorded!", type = "message")
  })
  
  # Render static table of collected submissions
  output$static <- renderTable({
    df <- biodata_db()
    if(nrow(df) == 0) {
      return(data.frame(Message = "No submissions yet."))
    }
    df
  })
  
  # Render interactive data table of collected submissions
  output$dynamic <- renderDataTable({
    df <- biodata_db()
    if(nrow(df) == 0) {
      return(data.frame(Message = "No submissions yet."))
    }
    df
  }, options = list(pageLength = 5))
}

shinyApp(ui, server)