options(repos = c(CRAN = "https://cran.rstudio.com"))

library(shiny)
library(tidyverse)
library(arsenal)
library(survival)
library(ggfortify)
library(dq)
source("helper.R")

# increase max upload file size
options(shiny.maxRequestSize = 10*1024^2)

ui <- navbarPage(
  theme = mayoshiny::mayoshinytheme(),
  "Basic Data Exploration App",
  tabPanel(
    "Exploration",
    mainPanel(
      width = 12,
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabsetPanel(
        tabPanel(
          "Data Upload",
          actionButton("mockstudy", "Use sample dataset..."),
          fileInput("inputfile", NULL, buttonLabel = "...or upload a dataset", multiple = FALSE),
          textOutput("inputfiletext"),
          uiOutput("inputfileoptions"),
          hr(),
          actionButton("inputfilebutton", "Load/Reload data"),
          h4("Data set structure:"),
          verbatimTextOutput("inputfilestr"),
          h4("File input problems:"),
          verbatimTextOutput("inputfileproblems")
        ),
        tabPanel(
          "Data Viewer",
          dataTableOutput("datatable")
        ),
        tabPanel(
          "Summary Statistics",
          fluidRow(
            downloadButton("tableby.downloadHTML", "Download HTML") #, downloadButton("tableby.downloadPDF", "Download PDF")
          ),
          fluidRow(
            column(4, selectInput("tableby.y", "By-Variable", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("tableby.x", "X-Variables", choices = " ", multiple = TRUE, selectize = FALSE)),
            column(4, selectInput("tableby.strata", "Strata Variable", choices = " ", multiple = FALSE, selectize = FALSE))
          ),
          fluidRow(tableOutput("tableby"))
        ),
        tabPanel(
          "Data Quality",
          tabsetPanel(
            tabPanel(
              "Univariate",
              fluidRow(
                column(4, numericInput("nshow1", "N Records to Show:", value = 10)),
                column(4, numericInput("univ.cutoff", "Outlier cutoff", value = 0.05))
              ),
              fluidRow(p("This may take some time to compute.")),
              fluidRow(tableOutput("univ.table")),
              fluidRow(
                column(4, selectInput("univ.trendvar", "Plot Trends for", choices = " ", multiple = FALSE, selectize = FALSE))
              ),
              fluidRow(plotOutput("univ.trendplot", width = 600))
            ),
            tabPanel(
              "Pairwise",
              fluidRow(numericInput("nshow2", "N Records to Show:", value = 10)),
              fluidRow(tableOutput("pair.table")),
              fluidRow("Effective Number of Variables:"),
              fluidRow(tableOutput("pca.table")),
              fluidRow(plotOutput("pca.screeplot", width = 600))
            ),
            tabPanel(
              "By Observation",
              fluidRow(
                column(4, numericInput("nshow3", "N Records to Show:", value = 10)),
                column(4, numericInput("byobs.cutoff", "Outlier cutoff", value = 0.05))
              ),
              fluidRow(p("This may take some time to compute.")),
              fluidRow(
                column(6, plotOutput("byobs.plot")),
                column(6, tableOutput("byobs.table"))
              )
            )
          )
        ),
        tabPanel(
          "Plotting",
          fluidRow(
            column(
              3,
              selectInput("ggplot.y", "Y-Variable", choices = " ", multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.x", "X-Variable", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(
              3,
              selectInput("ggplot.plottype", "Plot Type", choices = PLOTTYPES, multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.facet", "By-Variable", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(
              3,
              selectInput("ggplot.color", "Color", choices = " ", multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.fill", "Fill", choices = " ", multiple = FALSE, selectize = FALSE)
            ),
            column(
              3,
              selectInput("ggplot.scale_y", "Y-Scale Transformation", choices = SCALETYPES("y"), multiple = FALSE, selectize = FALSE),
              selectInput("ggplot.scale_x", "X-Scale Transformation", choices = SCALETYPES("x"), multiple = FALSE, selectize = FALSE)
            )
          ),
          shinycssloaders::withSpinner(plotly::plotlyOutput("ggplotplot"), color = "#003da5"),
          p("To download this plot, hit the button on the toolbar above the plot.")
        ),
        tabPanel(
          "Survival Analysis",
          fluidRow(
            column(4, selectInput("surv.time", "Follow-Up Time", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("surv.event", "Follow-Up Status", choices = " ", multiple = FALSE, selectize = FALSE)),
            column(4, selectInput("surv.x", "X-Variables", choices = " ", multiple = FALSE, selectize = FALSE))
          ),
          plotOutput("survplot")
        )
      )
    )
  ),
  tabPanel(
    "Documentation",
    HTML(documentation),
    p(style="font-size: 10px; margin-top: 75px;", paste0("App version ", read.dcf("DESCRIPTION")[1, "Version"])),
    tags$a("NEWS file", href = "NEWS.md", target = "_blank")
  ),
  tabPanel("DISCLAIMER", mayoshiny::disclaimer())
)

server <- function(input, output, session) {

  # This allows you to toggle back and forth between uploaded data and mockstudy
  whichData <- reactiveValues(inputDat = NULL, mockStud = 0, fp = NULL)

  observeEvent(list(input$inputfile, input$mockstudy), {
    if(!identical(input$inputfile$datapath, whichData$inputDat))
    {
      whichData$fp <- input$inputfile$datapath
      whichData$inputDat <- input$inputfile$datapath
    } else if(input$mockstudy != whichData$mockStud)
    {
      whichData$fp <- "data/mockstudy.csv"
      whichData$mockStud <- input$mockstudy
    }
  })


  ################## Update data entry tab ##################

  output$inputfiletext <- renderText({
    if(!is.null(whichData$fp)) paste0("File of extension '", tools::file_ext(whichData$fp), "' detected.") else ""
  })

  cn <- reactive({
    validate(
      need(!is.null(whichData$fp), "Please select a dataset.")
    )
    if(tools::file_ext(whichData$fp) %in% with_col_types)
    {
      cn <- names(read_my_file(whichData$fp, n_max = 1))
    } else NULL
  })

  output$inputfileoptions <- renderUI({

    if(!is.null(cn()))
    {
      opts <- c(Guess = "guess", Numeric = "double", Character = "character", Date = "date", Logical = "logical", Skip = "skip")
      uis <- map2(cn(), seq_along(cn()), ~ column(2, selectInput(paste0("column", .y), .x, opts, selectize = FALSE)))
      fluidRow(h3("Provide column specifications:"), uis)
    } else NULL
  })

  # eventReactive avoids it being called when the app loads
  inputData <- eventReactive(input$inputfilebutton, {
    validate(
      need(!is.null(whichData$fp), "Please load the dataset.")
    )
    types <- map(seq_along(cn()), ~ match.fun(paste0("col_", input[[paste0("column", .x)]]))())
    read_my_file(whichData$fp, col_types = do.call(cols, types))
  }, ignoreNULL = FALSE)

  output$inputfileproblems <- renderPrint({
    if(nrow(p <- problems(inputData())))
    {
      print(as.data.frame(select(p, -file)), row.names = FALSE)
    } else cat("No problems detected.")
  })


  output$inputfilestr <- renderPrint({
    utils:::str.default(inputData(), give.attr = FALSE)
  })

  columnNames <- reactive(colnames(inputData()))

  ################## Update all dropdowns across the app ##################

  observeEvent(columnNames(), {
    cn <- function(a = NULL)
    {
      out <- c(" ", columnNames())
      if(!is.null(a)) names(out) <- c(a, out[-1])
      out
    }
    updateSelectInput(session, "tableby.y", choices = cn())
    updateSelectInput(session, "tableby.x", choices = cn())
    updateSelectInput(session, "tableby.strata", choices = cn())
    updateSelectInput(session, "univ.trendvar", choices = cn())
    updateSelectInput(session, "ggplot.y", choices = cn())
    updateSelectInput(session, "ggplot.x", choices = cn())
    updateSelectInput(session, "ggplot.facet", choices = cn("(No By-Variable)"))
    updateSelectInput(session, "ggplot.color", choices = cn("(No Color)"))
    updateSelectInput(session, "ggplot.fill", choices = cn("(No Fill)"))
    updateSelectInput(session, "surv.time", choices = cn())
    updateSelectInput(session, "surv.event", choices = cn("(All Events)"))
    updateSelectInput(session, "surv.x", choices = cn("(No X-Variables)"))
  })

  ################## Update data viewer tab ##################

  output$datatable <- renderDataTable({
    inputData()
  })

  ################## Update summary statistics tab ##################

  tableby_object <- reactive(do_the_tableby(input$tableby.y, input$tableby.x, input$tableby.strata, isolate(inputData())))

  output$tableby <- renderTable({
    as.data.frame(summary(tableby_object(), text = TRUE, term.name = TRUE))
  })

  output$tableby.downloadHTML <- downloadHandler(
    filename = function() "tableby.html",
    content = function(file) write2html(list(yaml(pagetitle = "Tableby Output"), tableby_object()), file = file, quiet = TRUE, term.name = TRUE)
  )
  # output$tableby.downloadPDF <- downloadHandler(
  #   filename = function() "tableby.pdf", content = function(file) write2pdf(tableby_object(), file = file, quiet = TRUE, term.name = TRUE)
  # )

  ################## Update data quality tab ##################

  univ.tab <- reactive({
    dq_univariate(inputData(), input$univ.cutoff)
  })

  output$univ.table <- renderTable({
    validate(
      need(is.numeric(input$nshow1) && input$nshow1 > 0, "Please enter a number greater than 0.")
    )
    summary(univ.tab(), digits.pval = 2, n = input$nshow1)
  })

  output$univ.trendplot <- renderPlot({
    validate(
      need(input$univ.trendvar != " ", "Please select a variable.")
    )

    plot(univ.tab(), variable = input$univ.trendvar, data = inputData())
  })

  pair.tab <- reactive({
    dq_pairwise(inputData())
  })

  output$pair.table <- renderTable({
    validate(
      need(is.numeric(input$nshow2) && input$nshow2 > 0, "Please enter a number greater than 0.")
    )
    summary(pair.tab(), input$nshow2)
  })

  pcas <- reactive({
    dq_pca(inputData())
  })

  output$pca.table <- renderTable({
    summary(pcas())
  })

  output$pca.screeplot <- renderPlot({
    plot(pcas())

  })

  by.obs.tab <- reactive({
    validate(
      need(nrow(inputData()) < 10000, "Sorry, this data quality metric is limited to datasets with less than 10,000 rows.")
    )
    dq_multivariate(inputData())
  })

  output$byobs.plot <- renderPlot({
    plot(by.obs.tab(), cutoff = input$byobs.cutoff)
  })

  output$byobs.table <- renderTable({
    validate(
      need(is.numeric(input$nshow3) && input$nshow3 > 0, "Please enter a number greater than 0.")
    )
    summary(by.obs.tab(), input$nshow3)
  })

  ################## Update plotting tab ##################

  output$ggplotplot <- plotly::renderPlotly({
    plotly::ggplotly(do_the_ggplot(
      y = input$ggplot.y,
      x = input$ggplot.x,
      color = input$ggplot.color,
      fill = input$ggplot.fill,
      facet = input$ggplot.facet,
      scale_y = input$ggplot.scale_y,
      scale_x = input$ggplot.scale_x,
      type = input$ggplot.plottype,
      dat = isolate(inputData())
    ))
  })

  ################## Update survival tab ##################

  output$survplot <- renderPlot({
    do_the_survplot(
      input$surv.time,
      input$surv.event,
      input$surv.x,
      isolate(inputData())
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

