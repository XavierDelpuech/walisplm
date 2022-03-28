#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(

      # App title ----
      titlePanel("Controles"),

      # Sidebar layout with input and output definitions ----
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

          # Input: Select a file 1----
          fileInput("file1", "Donnees meteo en format CSV",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),

          # Input: Select a file 2 ----
          fileInput("file2", "Parametres en format CSV",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),

          # Horizontal line ----
          tags$hr(),

          # Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),

          # Input: Select separator ----
          radioButtons("sep", "Separateur",
                       choices = c(Virgule = ",",
                                   PointVirgule = ";",
                                   Tabulation = "\t"),
                       selected = ";"),

          # Input: Select decimal ----
          radioButtons("dec", "Decimale",
                       choices = c(Point = ".",
                                   "Virgule" = ","),
                       selected = ","),

          # Horizontal line ----
          tags$hr(),

          # Input: Select number of rows to display ----
          radioButtons("disp", "Vue",
                       choices = c(Debut = "head",
                                   Tout = "all"),
                       selected = "head"),

          # Button
          downloadButton("downloadData", "Download"),

          # Horizontal line ----
          tags$hr(),

          # Example Files to download
          h4("Exemples a telecharger :"),
          a(href="ex_meteo.csv", "Fichier modele donnees meteo", download=NA, target="_blank"),
          a(href="ex_parametres.csv", "Fichier modele parametres", download=NA, target="_blank"),
        ),

        # Main panel for displaying outputs ----
        mainPanel(

          # Output: Data file ----
          tableOutput("tableMto"),
          tableOutput("tableParam"),
          plotOutput("distPlot")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "WaLISg"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
