# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# APP CATALOG (META DATA) ----
app1 <- list(
    title = "Stock Analyzer",
    subtitle = "MongoDB Atlas",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and MongoDB Atlas Cloud.",
    sub_directory = "stock_analyzer_mongo_atlas",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "MongoDB", "Auth"),
        color = c("info", "success", "success", "success", "default")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app2 <- list(
    title = "Stock Analyzer",
    subtitle = "Local Data",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and uses internal data storage.",
    sub_directory = "stock_analyzer_local_data",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "Auth"),
        color = c("info", "success", "success", "default")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app3 <- list(
    title = "Old Faithful",
    subtitle = NA,
    description = "A test application used to validate Shiny Server.",
    sub_directory = "test_app",
    tags = tibble(
        tag = c("Sample Apps", "Shiny", "AWS"),
        color = c("info", "success", "success")
    ) %>% list(),
    img = "test_app.jpg"
)

app_catalog_tbl <- bind_rows(app1, app2, app3, .id = "id")


# FUNCTIONS ----

navbar_page_with_inputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form navbar-right", inputs)
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
        navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form)
    navbar
}

# UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
    tagList(
      tags$head(HTML("<title>Apps by Business Science</title>"))  
    ),
    style = "padding:0px;",
    navbar_page_with_inputs(
        title = div(
            tags$img(
                src = "https://www.business-science.io/img/business-science-logo.png",
                width = "30", 
                height = "30",
                style = "-webkit-filter: drop-shadow(3px 3px 3px #222);"
            ),
            " Apps by Business Science"
        ),
        collapsible = TRUE,
        theme = shinytheme("paper"),
        inputs = div(
            textInput(inputId = "search_box", label = NULL, placeholder = "Search", width = 200),
            actionButton(inputId = "seach_button", label = "Submit"),
            actionButton(inputId = "clear_button", label = "Clear")
        ),
        
        tabPanel(
            title = "Library",
            div(
                class = "container",
                id = "tag-filters",
                radioGroupButtons(
                    inputId = "input_tags",
                    choices = c("All", "AWS", "SHINY", "MONGODB"), 
                    justified = TRUE, 
                    status = "default" 
                )
            ),
            div(
                class = "container",
                id = "app-library",
                uiOutput(outputId = "output_cards")
            )
        )
    )
)

# SERVER ----
server <- function(session, input, output) {
  
  output$output_cards <- renderUI({
    div(
      class = "container",
      div(
        class = "row",
        div(
          class = "col-sm-4",
          style = "display:flex;",
          div(
            class = "panel panel-default",
            div(
              class = "panel-heading",
              span(class = "label label-info", "AWS")
            ),
            div(
              class = "panel-body",
              style = "padding:20px;",
              tags$img(
                class = "img  img-thumbnail",
                src = "images/stock_analyzer.jpg"
              ),
              br(), br(),
              h4("Stock Analyzer", br(), tags$small("MongoDB Atlas")),
              p(app_catalog_tbl$description[[1]]),
              a(
                type = "button",
                class = "btn btn-primary",
                target = "_blank",
                href = str_c("/", app_catalog_tbl$sub_directory[[1]]),
                "open"
              )
            )
          )
        )
      )
    )
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
