ui <- fluidPage(

  shinyalert::useShinyalert(),
  shinyjs::useShinyjs(),

  tags$hr(),
  mainPanel(
    fluidRow(
      conditionalPanel(
        condition = "input.GO == 0",
        column(6, align = "center",
               offset = 6, actionButton("GO", "Begin!", icon = icon("play"))))),
    conditionalPanel(
      condition = "input.GO > 0",

      fluidRow(
        column(12, align = "center",
               plotOutput("new_plot"))
      ),

      fluidRow(
        column(12, align = "center",
               uiOutput("start_e"))
      ),
      fluidRow(
        column(12, align = "center",
               uiOutput("end_e"))
      ),

      helpText("**Note: Use your mouse to adjust the sliders. Then, if you need to be more precise,
                 you can use your left and right arrows on your keyboard to make small adjustments.**"),

      shinyBS::bsButton(inputId = "prevdive", "Previous Dive", style = "info"),
      shinyBS::bsButton(inputId = "submit", "Submit", style = "success"),
      shinyBS::bsButton(inputId = "nextdive", "Next Dive", style = "info"),

      tags$hr(),

      shinyjs::hidden(
        div(id='text_div',
            verbatimTextOutput("text")
        )
      ),


      tags$hr(),
      actionButton("browser", "browser"),
      tags$script("$('#browser').show();"))

  )
)
