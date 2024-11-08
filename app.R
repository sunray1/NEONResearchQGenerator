library(shiny)
library(shinythemes)
library(tools)

# Load data
variables <- read.csv("variables.csv", stringsAsFactors = FALSE)

# Function to replace placeholders with nested values
replace_placeholders <- function(variable) {
  pattern <- "\\[(.*?)\\]"
  replacement <- NULL
  if (grepl(pattern, variable)) {
    placeholder <- regmatches(variable, regexpr(pattern, variable))[[1]]
    key <- sub("\\[(.*?)\\]", "\\1", placeholder)
    nested_options <- variables$Variable[variables$Type == key]
    if (length(nested_options) > 0) {
      replacement <- sample(nested_options, 1)
      variable <- sub(pattern, replacement, variable)
    }
  }
  return(c(variable, replacement))
}

# Custom CSS using NEON's theme colors
custom_css <- "
body {
  background-color: #f5f6f7; /* grey[50] */
  font-family: 'Roboto', 'Helvetica', 'Arial', sans-serif;
}
h3, h4 {
  color: #0073cf; /* primary.main */
  font-weight: 700;
}
.card {
  background-color: #fff; /* background.paper */
  border-radius: 8px;
  padding: 20px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  margin-bottom: 20px;
}
.action-button {
  background-color: #fff; /* primary.main */
  color: #0073cf; /* primary.contrastText */
  border: 1px solid #0073cf;
  font-size: 18px;
  padding: 12px;
  width: 100%;
  border-radius: 4px;
  cursor: pointer;
  text-transform: uppercase;
  font-weight: bold;
}
.action-button:hover {
  background-color: #004986; /* primary.dark */
}
.static-text {
  background-color: #fff;
  border-color: #F0AB00;
  border-style: solid;
  border-top-width: 2px;
  border-left-width: 20px;
  border-right-width: 2px;
  border-bottom-width: 2px;
  padding: 15px;
  margin-bottom: 20px;
  font-size: 16px;
  color: #565a5c;
  line-height: 1.6
}
"

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(custom_css))),
  
  titlePanel("Research Question Generator"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Generate Question", class = "action-button"),
      br(), br(),
      p("Press the button to generate a random research question!", style = "text-align: center;")
    ),
    
    mainPanel(
      div(
        h3("Your Research Question:"),
        div(textOutput("question"), style = "font-size: 24px; margin-top: 10px; color: rgba(0, 0, 0, 0.9);"),
        class = "card"
      ),
      
      div(
        h3("Comments and Useful Links:"),
        div(
          uiOutput("comments"),  # Use uiOutput to render HTML
          style = "font-size: 16px; margin-top: 10px; color: rgba(0, 0, 0, 0.9);"
        ),
        class = "card"
      ),
      
      div(
        div(
          HTML("This app generates random research questions that could be answered using data available on NEON.
          This is designed to be used as a jumping off point to generate ideas. Try restructuring and expanding your questions
          in chatGPT to further dig into them.<br><br>
          Note that not all research questions will make ecological/biological 'sense' necessarily.<br><br>
          Remember that correlation does not equal causation when thinking about your research question. Being able to predict
          something based on something else is asking if they are correlated, where causation seeks to define the cause of something
          happening - the how or why. (ex. shoe size and intellegence are correlated, but ultimately, age is the causual variable)<br><br>
          At its simplest, establishing causation requires:<br><br>
          1. Correlation<br>
          2. Ruling out of confounding variables - can typically be done by randomizing all other variables (try analyzing the relationship at other sites)<br>
          3. A plausible theory of why X causes Y<br>
               "),
          class = "static-text"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_vars <- reactiveVal(list(x = NULL, y = NULL, comments = NULL))
  
  observeEvent(input$generate, {
    x_var_raw <- sample(variables$Variable[variables$Type == "X"], 1)
    y_var_raw <- sample(variables$Variable[variables$Type == "Y"], 1)
    x_var <- x_var_raw
    x_var_nested <- NA
    while (grepl("\\[.*?\\]", x_var)) {
      results <- replace_placeholders(x_var)
      x_var <- results[1]
      x_var_nested <- results[2]
    }
    
    if (is.na(x_var_nested)) {
      x_var_nested <- NULL
    }
    y_var <- y_var_raw
    y_var_nested <- NA
    while (grepl("\\[.*?\\]", y_var)) {
      results <- replace_placeholders(y_var)
      y_var <- results[1]
      y_var_nested <- results[2]
    }
    
    if (is.na(y_var_nested)) {
      y_var_nested <- NULL
    }
    
    # Retrieve comments for both main and nested variables
    x_comment <- variables$Comment[variables$Variable == x_var_raw]
    y_comment <- variables$Comment[variables$Variable == y_var_raw]
    
    # Retrieve comments for nested variables if present
    nested_x <- variables$Comment[variables$Variable == x_var_nested]
    nested_y <- variables$Comment[variables$Variable == y_var_nested]
    
    comments <- c(if (all(x_comment != "")) paste("<b>", toTitleCase(x_var), ":</b><br>",sep=""),
                  if (all(x_comment != "")) paste(x_comment, "<br><br>",sep=""),
                  if (all(y_comment != "")) paste("<b>", toTitleCase(y_var), ":</b><br>",sep=""),
                  if (all(y_comment != "")) paste(y_comment, "<br><br>",sep=""),
                  if (length(nested_x)!=0) paste("<b>", toTitleCase(x_var_nested), ":</b><br>", sep=""), 
                  if (length(nested_x)!=0) paste(nested_x, "<br><br>", sep=""), 
                  if (length(nested_y)!=0) paste("<b>", toTitleCase(y_var_nested), ":</b><br>", sep=""), 
                  if (length(nested_y)!=0) paste(nested_y, "<br><br>", sep="")
                  )
    selected_vars(list(x = x_var, y = y_var, comments = comments))
  })
  
  output$question <- renderText({
    vars <- selected_vars()
    if (!is.null(vars$x) & !is.null(vars$y)) {
      paste("How do/does ", vars$x, " affect/predict ", vars$y, "?", sep = "")
    } else {
      "Click the button to generate a research question."
    }
  })
  
  output$comments <- renderUI({
    vars <- selected_vars()
    if (!is.null(vars$comments)) {
      # Wrap each comment in a <p> tag with extra space after
      HTML(vars$comments)
    } else {
      HTML("<p>No comments available.</p>")
    }
  })
}

shinyApp(ui, server)