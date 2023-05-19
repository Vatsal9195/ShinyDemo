# Module UI
dashboard_ui <- function(id) {
  ns = NS(id)
  
  fluidPage(shinyjs::useShinyjs(),
            # ---- Uploading File ----
            column(width = 4,
                   fluidRow(
                     box(
                       fileInput(
                         ns("input_file"),
                         label = "Upload File:",
                         buttonLabel = 'Choose file',
                         accept = c('csv', 'comma-separated-values', '.csv'),
                       ),
                       # ---- EDA Type Option ----
                       shinyjs::hidden(
                         checkboxGroupInput(
                           inputId = ns("EDA_type"),
                           "EDA Type Selection: ",
                           c("Uni-variate" = "uni",
                             "Bi-variate" = "bi"),
                           inline = TRUE
                       )),width = 12
                     ),
                   div(id = 'Visualizations'))
  ),
  column(width = 8,
         # ---- Uni Chart ----
         conditionalPanel(condition = "input.EDA_type.includes('uni')", {
           box(width = 12,
               # h2("Uni-variate distribution", style = "text-align: center"),
               highchartOutput(outputId = ns("Uni_chart"))
               )
         }, ns = ns),
         # ---- Bi - Chart ----
         conditionalPanel(condition = "input.EDA_type.includes('bi')", {
           box(width = 12,
               # h2("Bi-variate distribution", style = "text-align: center"),
               highchartOutput(outputId = ns("Bi_chart")))
         }, ns = ns)
         )
  )
  
}

# Module Server
dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ----- Reactive to Fetch File details ----
    file_fetch <- reactive({
      file <- input$input_file
      
      req(file)
      
      ext <- tools::file_ext(file$datapath)
      temp_file <- read.csv(file$datapath, sep = ",", stringsAsFactors = FALSE)

      return(list(Data = temp_file, type = ext))
    })

    # ---- Reactive Values for Upload or Reset ----
    values <- reactiveValues(
      file_state = NULL
    )
    
    # ---- Event to Show Modal ----
    observeEvent(input$input_file, {
      
      shinyjs::reset(id = "EDA_type")
      
      file <- file_fetch()

      # modal to show and check data
      if(file$type == "csv"){
        
        showModal(modalDialog('',
                              tabsetPanel(
                                tabPanel(title = "Data", icon = icon('table'),
                                         DT::DTOutput(ns("rawData_DT"))),
                                tabPanel(title = "Structure",icon = icon('building'),
                                         DT::DTOutput(ns("Structure_DT"))),
                                tabPanel(title = "Summary", icon = icon('signal'),
                                         DT::DTOutput(ns("Summary_DT"))),
                                tabPanel(title = "Class",icon = icon('users'),
                                         DT::DTOutput(ns("Class_DT")))
                              ),
                              title = 'Confirm Dataset',
                              footer = tagList(
                                actionButton(ns('confirm_button'), 'Data Upload', icon = icon('upload'), status ='primary'),
                                actionButton(ns('cancel_button'), 'Discard', icon = icon('ban'))
                              ),
                              size = "l",
                              easyClose = TRUE
        ))}
      else{
        # modal to tell user to re-upload the file
        showModal(modalDialog(paste0('Please Upload a CSV file instead of .', file$type, ' file'), title = 'Alert', easyClose = TRUE))
      }
    })

    # ---- Button Event: Upload UI ----
    observeEvent(input$confirm_button, {
      values$file_state <- 'upload'
      
      shinyjs::show(id = 'EDA_type')
      removeUI(
        selector = "#UnivariateBox1"
      )
      removeUI(
        selector = "#BivariateBox2"
      )
      removeModal()
      
    })
    
    # ---- Button Event: Discard ----
    observeEvent(input$cancel_button, {
      values$file_state <- 'reset'
      shinyjs::hide("EDA_type")
      shinyjs::reset("EDA_type")
      shinyjs::reset(id = "input_file")
      removeUI("#Visualizations *", multiple = TRUE)
      # reload()
      removeModal()
      
    })
    
    # ---- Select Option display Event ----
    observeEvent(values$file_state,{
      
      if(values$file_state == 'upload'){
        file_data <- file_fetch()$Data
        
      }
      
      
      })
    
    # ObserveEvent to remove EDA boxes
    observeEvent(length(input$EDA_type) == 0, {

      removeUI(
        selector = "#UnivariateBox1"
      )
      removeUI(
        selector = "#BivariateBox2"
      )

    })
    
    # ---- Module Observe Event: For Select Inputs -----
    observeEvent(input$EDA_type, {
      # print(input$EDA_type)
      colinfo <- UD_structure_m(file_fetch()$Data)
      
      if('bi' %in% input$EDA_type){
        insertUI(
          selector = "#Visualizations",
          where = c("afterEnd"),
          ui = tags$div(id = 'BivariateBox2',
                        box(h3("Bi-variate Analysis Column selector", style = "text-align: center"),
                            selectInput(inputId = ns('BiColFilter1'),
                                        choices = sort(unique(colinfo$ColNames[colinfo$`Class` != 'character'])),
                                        label = 'Continuous Column - X Axis'),
                            selectInput(inputId = ns('BiColFilter2'),
                                        choices = sort(unique(colinfo$ColNames[colinfo$`Class` != 'character'])),
                                        label = 'Continuous Column - Y Axis'),
                            width = 12))
        )
      }
      
      if(!'bi' %in% input$EDA_type){
        removeUI(
          selector = "#BivariateBox2"
        )
      }
      
      if('uni' %in% input$EDA_type){
        insertUI(
          selector = "#Visualizations",
          where = c("afterBegin"),
          ui = tags$div(id = 'UnivariateBox1',
                        box(h3("Uni-variate Analysis Column selector", style = "text-align: center"),
                            shiny::selectInput(inputId = ns('UniColFilter1'),
                                        choices = sort(unique(colinfo$`ColNames`)),
                                        label = 'Categorical/Continuous Column'),
                            width = 12))
        )
      }
      
      if(!'uni' %in% input$EDA_type){
        # print('removing')
        removeUI(
          selector = "#UnivariateBox1"
        )
      }
      
    })
    
    # ---- Output: Data Table ----
    output$rawData_DT <- DT::renderDT({

      file <- file_fetch()

      temp_file <- file$Data

      temp_file %>%
        DT::datatable(
          filter = 'top',
          options = list(
            columnDefs = list(list(targets = '_all', className = 'dt-center'),
                              list(targets = '_all', className = 'dt-body-right')),
            ordering = TRUE,
            Scroller = FALSE,
            scrollCollapse = FALSE,
            scrollY = TRUE,
            scrollX = TRUE,
            paging = TRUE,
            pageLength = 10,
            searching = TRUE,
            fixedColumns = list(leftColumns = 1),
            dom = 'Brtip',
            style = 'default',
            info = TRUE
          ),
          selection  = "none",
          escape =  FALSE,
          rownames = TRUE
        )
    })


    # ---- Output: Structure Table ----
    output$Structure_DT <- DT::renderDT({

      file <- file_fetch()

      x <- file$Data

      data_str <- UD_structure_m(x) %>% rename('Column Names' = ColNames)

      heading_str  <- paste0(nrow(x), " Obs. of ", ncol(x), " variables")

      datatable(data_str,
                filter = 'top',
                class = 'cell-border stripe',
                options = list(
                  columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                    list(targets = '_all', className = 'dt-body-left')),
                  ordering = TRUE,
                  Scroller = FALSE,
                  scrollCollapse = FALSE,
                  scrollY = TRUE,
                  scrollX = TRUE,
                  paging = TRUE,
                  pageLength = 10,
                  searching = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  dom = 'Brtip',
                  style = 'default',
                  info = TRUE
                ),
                selection  = "none",
                escape =  FALSE,
                rownames = TRUE,
                caption = tags$caption(style = 'caption-side: top;font-weight: bold; text-align: center; color:black; font-size:100% ;', heading_str))

    })

    # ---- Output: Summary Table ----
    output$Summary_DT <- DT::renderDT({

      file <- file_fetch()

      x <- file$Data

      data_summ <- do.call(cbind, lapply(Filter(is.numeric, x), summary)) %>% t() %>% as.data.frame() %>% 
        mutate('Column Names' = row.names(.),
               Mean = round(Mean, 2)) %>% 
        select(`Column Names`, everything())

      row.names(data_summ) <- NULL
      
      datatable(data_summ,
                filter = 'top',
                class = 'cell-border stripe',
                options = list(
                  columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                    list(targets = c(0:1), className = 'dt-body-left'),
                                    list(targets = '_all', className = 'dt-body-right')),
                  ordering = TRUE,
                  Scroller = FALSE,
                  scrollCollapse = FALSE,
                  scrollY = TRUE,
                  scrollX = TRUE,
                  paging = TRUE,
                  pageLength = 15,
                  searching = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  dom = 'Brtip',
                  style = 'default',
                  info = TRUE
                ),
                selection  = "none",
                escape =  FALSE,
                rownames = TRUE)

    })

    # ---- Output: Class Table ----
    output$Class_DT <- DT::renderDT({

      file <- file_fetch()

      x <- file$Data

      data_A <- UD_structure_m(x)

      col <- as.character(data_A$ColNames[data_A$Class %ni% c("numeric", "Date", "logical")])

      j <- data.frame()

      for (i in col) {
        jtemp <- x %>% group_by(!!sym(i)) %>%
          summarise(Frequency = n()) %>%
          mutate(KPI = i) %>%
          rename('Category'  = !!sym(i))

        j <- rbind(j, jtemp)
      }
      
      j <- j %>% select(KPI, Category, Frequency)

      datatable(j,
                filter = 'top',
                class = 'cell-border stripe',
                options = list(
                  columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                    list(targets = c(0:1), className = 'dt-body-left'),
                                    list(targets = '_all', className = 'dt-body-right')),
                  ordering = TRUE,
                  Scroller = FALSE,
                  scrollCollapse = FALSE,
                  scrollY = TRUE,
                  scrollX = TRUE,
                  paging = TRUE,
                  pageLength = 15,
                  searching = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  dom = 'Brtip',
                  style = 'default',
                  info = TRUE
                ),
                selection  = "none",
                escape =  FALSE,
                rownames = TRUE)

    })
    
    # ---- Chart: Uni-Variate Distribution ----
    output$Uni_chart <- renderHighchart({
      
      req(values$file_state == 'upload')
      
      uni_col_filter <- input$UniColFilter1
      
      req(uni_col_filter)
      
      file_data <- file_fetch()$Data
      
      req(file_data)
      req(uni_col_filter %in% UD_structure_m(file_data)$ColNames)
      
      uni_col_df <- UD_structure_m(file_data) %>% 
        dplyr::filter(ColNames == uni_col_filter)
      
      if(uni_col_df$Class %in% c('character', 'logical')){
        groupingVar <- uni_col_filter
        file_data %>% 
          group_by(!!sym(groupingVar)) %>% 
          tally() %>% 
          ungroup() %>% 
          rename('Categ' = groupingVar) %>% 
          hchart(type = 'column',
                 hcaes(x = 'Categ',y = 'n')
          ) %>% 
          hc_title(text = 'Uni-variate Analysis - Categorical') %>% 
          hc_tooltip(pointFormat =  '<br/>{}Frequency: {point.y:,.4f}',
                     crosshairs = FALSE,
                     shared = TRUE)
      }else{
        
        xData <- file_data %>% 
          dplyr::select(uni_col_filter) %>% 
          pull() %>% na.omit()
        
        hchart(density(xData), 
               type = "area", name = uni_col_filter) %>% 
          hc_title(text = 'Uni-variate Analysis - Continuous')
      }
      
    })
    
    # ---- Chart: Bi-Variate Distribution ----
    output$Bi_chart <- renderHighchart({
      
      # fil_options <- Bi_colOptions()
      xsel <- input$BiColFilter1
      ysel <- input$BiColFilter2
      # xsel <- fil_options$xaxis
      # ysel <- fil_options$yaxis
      
      file_data <- file_fetch()$Data
      
      dataupload <- file_data %>% 
        dplyr::select(xsel, ysel) %>% 
        rename('x' = xsel, 'y' = ysel)
      
      dataupload <- as.data.frame(dataupload %>% distinct())
      
      dataupload$color <- colorize(1:nrow(dataupload))
      
      highchart() %>% 
        hc_chart(type = 'scatter') %>% 
        hc_title(text = paste0('Bi-variate Analysis - ',xsel,' vs ',ysel)) %>% 
        hc_add_series(data = list_parse(dataupload)) %>% 
        hc_xAxis(title = list(enabled = TRUE,
                              text = list(xsel))) %>% 
        hc_yAxis(title = list(enabled = TRUE,
                              text = list(ysel))) %>% 
        hc_tooltip(headerFormat = " ") %>% 
        hc_legend(enabled = FALSE)
      
    })
    
  })
}
