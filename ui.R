# UI.R -- shiny frontend

library("keboola.shiny.lib")

shinyUI(
    keboolaPage(
        fluidPage(
            selectInput("listGroup", "List group", c(), multiple = TRUE),
            uiOutput("description")
        )
        , appTitle = "Distribution Groups"    
    )
)
