# Define UI

shinyUI(
  navbarPage(
    position = "fixed-top",
    theme = bslib::bs_theme(version = 4, bootswatch = "pulse"),
    "",
    collapsible = TRUE,
    id = "navbar",
    tabPanel(
      "Home",
      waiter::use_waiter(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      fluidPage(
        # id = "homePage",
        fluidRow(
          style = "min-height: 25%; min-height: 25vh;"
        ),
        fluidRow(
          tags$h1(title, class = "mx-auto text-center"),
          class = "justify-content-center"
        ),
        fluidRow(
          div(
            class = "mx-auto",
            # QFieldCloud login
            textInput("qfieldcloud_username",
              "QFieldCloud email:",
              value = "",
              placeholder = ""
            ),
            # QFieldCloud password
            passwordInput("qfieldcloud_password",
              "QFieldCloud password:",
              value = "",
              placeholder = ""
            ),
            actionButton(
              "login",
              htmltools::HTML("login")
            ),
            uiOutput("login_status"),
          ),
          class = "justify-content-center"
        )
      )
    ),
    tabPanel(
      "Map",
      shinyFeedback::useShinyFeedback(),
      waiter::use_waiter(),
      leafgl::leafglOutput("map"),
      absolutePanel(
        class = "sidePanel",
        top = "75px",
        left = "0px",
        width = "250px",
        height = "100vh",
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "draw_map",
            "Update map",
            class = "btn m-2"
          ),
        ),
        selectInput(
          "s3_bucket_objects",
          "Select dataset:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        selectInput(
          "map_active_layer",
          "Select layer:",
          choices = NULL
        ),
        selectInput(
          "map_var",
          "Select column:",
          choices = NULL
        ),
        selectInput(
          "label_vars",
          "Popup labels:",
          choices = NULL,
          multiple = TRUE
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "add_popups",
            "Add popups to map",
            class = "btn m-2"
          ),
        ),
        checkboxInput(
          "mask_zero",
          label = "Mask zeros.",
          value = FALSE
        ),
        checkboxInput(
          "legend",
          label = "Add legend.",
          value = FALSE
        ),
        selectInput(
          "map_colour",
          "Fill colour:",
          choices = colour_mappings
        )
      ),
      div(
        class = "d-flex justify-content-center",
        actionButton(
          "recenter_map",
          "Recenter map",
          class = "btn m-2"
        ),
      )
      # downloadButton(
      #   "download_map_data",
      #   "Download map",
      #   class = "btn-primary m-2"
      # )
    ),
    tabPanel(
      "Table",
      shinyFeedback::useShinyFeedback(),
      absolutePanel(
        class = "sidePanel",
        top = "75px",
        left = "0px",
        width = "250px",
        height = "100vh",
        selectInput(
          "table_s3_bucket_objects",
          "Select dataset:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        selectInput(
          "table_active_layer",
          "Select layer:",
          choices = NULL
        ),
        selectInput(
          "table_vars",
          "Select columns:",
          choices = NULL,
          multiple = TRUE
        ),
        downloadButton(
          "download_csv_data",
          "download table",
          class = "btn m-2"
        )
      ),
      absolutePanel(
        id = "table",
        DT::dataTableOutput("data_table")
      )
    ),
    tabPanel(
      "Report",
      shinyFeedback::useShinyFeedback(),
      # tags$div(
      fixedPage(
        id = "report",
        tags$h1("Report Builder"),
        tags$h6("Use the preview tools to check outputs before generating and downloading the report."),
        actionButton(
          "generate_report",
          "generate report",
          class = "btn m-2"
        ),
        downloadButton(
          "download_report",
          "download",
          class = "btn m-2"
        ),
        tags$h5("Select data"),
        fluidRow(
          column(
            4,
            selectInput(
              "report_s3_bucket_objects",
              "Select dataset:",
              choices = NULL,
              selected = NULL,
              multiple = FALSE
            )
          ),
          column(
            4,
            selectInput(
              "report_active_layer",
              "Select layer:",
              choices = NULL
            )
          ),
          column(
            4,
            selectInput(
              "report_vars",
              "Select column:",
              choices = NULL,
              multiple = TRUE
            )
          )
        ),
        tags$h5("Summary chart styling options"),
        tags$h6("Option to create a bar plot showing the mean, sum, or count of column values within a group."),
        checkboxInput(
          "make_chart",
          label = "Make summary chart?",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.make_chart == true",
          fluidRow(
            column(
              4,
              selectInput(
                "report_group_vars",
                "Grouping column:",
                choices = NULL
              )
            ),
            column(
              4,
              radioButtons(
                "bar_plot_type",
                "Chart type:",
                c(
                  "count" = "count_records",
                  "sum" = "sum_values",
                  "mean" = "mean"
                )
              )
            ),
            column(
              4,
              actionButton(
                "preview_chart",
                "preview chart",
                class = "btn m-2"
              )
            )
          ),
          fluidRow(
            column(
              4,
              textInput(
                "x_lab",
                "X-axis title:",
                value = ""
              )
            ),
            column(
              4,
              textInput(
                "y_lab",
                "Y-axis title:",
                value = ""
              )
            ),
            column(
              4,
              numericInput(
                "font",
                "Text size:",
                min = 10,
                max = 36,
                value = 14,
                step = 1
              )
            )
          )
        ) # conditional panel end
      ) # end div
    )
  )
)
