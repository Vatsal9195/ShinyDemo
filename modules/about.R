# Module UI
about_ui <- function(id){
  ns = NS(id)
  fluidPage(
    includeMarkdown('ReadMe.md')
  )
}


about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      insertUI(
        selector = "#newTest",
        where = "afterBegin",
        ui = tags$div(id = 'UnivariateBox1teteet',
                      box(shiny::selectInput(inputId = ns('newTestssss'),
                                             choices = sort(names(iris)),
                                             label = 'Categorical/Continuous Column'),
                          height = 400,
                          width = 12))
      )
    })

  })
}