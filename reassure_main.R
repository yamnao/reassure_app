# Function to install and load required packages
install_and_load <- function(package_name, install_from_github = NULL) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    if (!is.null(install_from_github)) {
      remotes::install_github(install_from_github, upgrade = FALSE)
    } else {
      install.packages(package_name)
    }
  }
  library(package_name, character.only = TRUE)
}

# Check and install CRAN packages
cran_packages <- c(
  "shiny", "shinydashboard", "shinyFiles", 
  "shinyBS", "DT", "RColorBrewer", "ggplot2", 
  "cowplot", "nipnTK", "ggh4x", 'remotes', 'dplyr', 'rio'
)

lapply(cran_packages, install_and_load)

# Check and install GitHub packages
install_and_load("smartextract", "yamnao/smartextract")
install_and_load("smartcleaning", "yamnao/smart_cleaning")
install_and_load("smartmetadata", "yamnao/smart_visualization")
install_and_load("mast", "afyac/mast")

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "REASSURE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "infos", icon = icon("info")),
      menuItem("Data Extraction", tabName = "extraction", icon = icon("download")),
      menuItem("Data Preprocessing", tabName = "cleaning", icon = icon("broom")),
      menuItem("Coverage Visualization", tabName = "coverage_smart", icon = icon("chart-line")),
      menuItem("Anthropology Visualization", tabName = "anthro_smart", icon = icon("object-group")),
      menuItem("Mortality Visualization", tabName = "mortality_smart", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        ".cleaning-survey-button {
          background-color: #cef1fd ; /* Blue color */
          border-color: #cef1fd ; /* Blue color */
          color: black; /* Text color */
        }",
        ".coverage-survey-button {
          background-color: #cef1fd ; /* Blue color */
          border-color: #cef1fd ; /* Blue color */
          color: black; /* Text color */
        }",
        ".anthro-survey-button {
          background-color: #cef1fd ; /* Blue color */
          border-color: #cef1fd ; /* Blue color */
          color: black; /* Text color */
        }",
        ".mortality-survey-button {
          background-color: #cef1fd ; /* Blue color */
          border-color: #cef1fd ; /* Blue color */
          color: black; /* Text color */
        }"
      )
    ),
    tabItems(
      #### INFO TAB ---------------------------------------------
      tabItem(tabName = "infos",
              h2(style = "color: #3652a8; margin-left: 200px;;","Welcome to the REASSURe App"),
              br(),
              div(style="max-width: 900px; float: left;; text-align: justify;", "This tool is designed to help you", tags$b("extract, clean, and visualize multiple Standardized Monitoring and Assessment of Relief and Transitions (SMART) surveys simultaneously."), 
                br(), 
                p("Developed by LSHTM with support from UNICEF, it streamlines the management and analysis of SMART Surveys.", 
                "SMART surveys are essential for evaluating nutritional status and mortality.",
                "Typically, these surveys are analyzed using the ENA software, which handles only one survey at a time.", br(), 
                "For projects requiring the analysis of hundreds of SMART surveys at different administrative levels, 
                this tool has been developed to optimize the process. It applies the results of the ENA to several surveys 
                simultaneously and includes additional functionality to improve analysis and management."),
              br(),
              h4("REASSURe is composed of several tabs:"),
              tags$ul(style="padding-left: 20px;",
                tags$li(tags$b('Data Extraction'),": Extracts SMART Survey content, including nutrition, mortality, and clusters data, along with metadata such as administrative levels."),
                br(),
                tags$li(tags$b('Data Preprocessing'),": Cleans nutrition and mortality data by handling missing information, filtering outliers, and removing impossible values based on WHO standards."),
                br(),
                tags$li(tags$b('Coverage Visualization'),": Visualizes SMART Survey coverage over time or by quality score."),
                br(),
                tags$li(tags$b('Anthropology Visualization'),": Visualizes nutritional data from SMART surveys, including flagged data or gender distribution."),
                br(),
                tags$li(tags$b('Mortality Visualization'),": Visualizes mortality data from the SMART surveys, such as crude death rates over time.")
              ),
              br(),
              p("For further details, please consult the documentation."))
      ),
      #### EXTRACTION TAB ---------------------------------------------
      tabItem(tabName = "extraction",
              h2(style = "color: #3652a8; margin-left: 200px;;", "Extract SMART Survey Content"), br(),
              div(style="max-width: 900px; float: left;; text-align: justify;",
                p("REASSURe organizes the SMART surveys and extracts their contents (clusters, nutrition, and mortality data) into a specified output folder. 
                Each SMART survey will have its own folder containing four files: nutrition, mortality, cluster, and the raw SMART file.", br(),
                "In addition, a metadata Excel file will be generated, listing the name, location, and date of each SMART survey."),
              h4("Parameters:"),
              tags$ol(
                tags$li(tags$b("Country:"), " Select the country where the SMART surveys were conducted."),
                tags$ul(
                  tags$li(tags$b("KEN:"), " For Kenya"),
                  tags$li(tags$b("SOM:"), " For Somalia"),
                  tags$li(tags$b("No Specified Country:"), " If the location is irrelevant or surveys were conducted in multiple countries."),
                  tags$li(tags$b("Add a Country:"), "Users can upload the other country's administrative level file. Refer to the documentation for file content details.")
                ),
                br(),
                tags$li(tags$b("Folder containing SMART Surveys:"), " Select the folder containing the .as files."),
                br(),
                tags$li(tags$b("Folder to save the results:"), " Select the folder to save the extracted content.")
              ),
              hr(style = "border-top: 1px solid #000000;"),
              # Use a div to contain the selectInput and the button
              div(
                style = "display: flex; align-items: center;",
                selectInput("country", "Select SMART localisation:",
                            choices = c("", "No Specified Country", "KEN", "SOM", "Add a country")),
                
                # Add a space between the selectInput and the button if needed
                tags$div(style = "margin-left: 10px;",
                         bsButton("country_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
                         bsTooltip("country_info", "If the location is irrelevant, select No Specified Country.", "right")
                )),
              uiOutput("country_file_ui"),
              # Title before the button
              tags$div(style = "font-weight: bold; margin-right: 10px;", "Select the folder containing SMART Surveys:"),
              shinyDirButton("survey_path", "Select folder contaning .as files (SMART Surveys)",
                             "Select Survey Folder", FALSE, class = "cleaning-survey-button"),
              bsButton("survey_path_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
              bsTooltip("survey_path_info", "All the .as file need to be in the same folder", "right"),
              verbatimTextOutput("survey_path_text"),
              br(),
              # Title before the button
              tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder to save the results:"),
              shinyDirButton("output_path", "Select folder to save SMART Surveys content",
                             "Select Output Folder",  FALSE, class = "cleaning-survey-button"),
              bsButton("output_path_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
              bsTooltip("output_path_info", "NB: the results folder must be different from the SMART survey folder.", "right"),
              verbatimTextOutput("output_path_text"),
              br(),
             actionButton("extract_button", "Start Extraction", 
                          style="color: #fff; background-color: #f15d20; border-color: #f12020"),
              br(), br(), br(),
              plotOutput("extraction_plot"))
              
      ),
      #### CLEANING TAB ---------------------------------------------
      tabItem(tabName = "cleaning",
              h2(style = "color: #3652a8; margin-left: 200px;;", "Cleaning SMART Survey Content"), br(),
              div(style="max-width: 900px; float: left;; text-align: justify;",
              p("REASSURe will clean the SMART Surveys by addressing missing values, filtering outliers, and ensuring dataset consistency (based on this paper: ", 
                a(href = "https://pubmed.ncbi.nlm.nih.gov/24883244/", "PubMed Article"), ").",
                "The output folder will contain cleaned nutrition, mortality, and cluster Excel files, along with a 
                cleaned metadata file containing quality scores and other mortality metrics."),
              h4("Parameters:"),
              p(tags$ol(
                tags$li(tags$b("Folder containing SMART Surveys extracted:"), " Select the folder with the extracted SMART Survey data (from the first tab)."),
                br(),
                tags$li(tags$b("Cleaning Type:"), " Select the type of cleaning process you want to execute.")
              )),
              p("Once you have provided the required data, click on the ", tags$b('"Start cleaning"'), " button to initiate the data cleaning process."),
              hr(style = "border-top: 1px solid #000000;"),
              # Title before the button
              tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder contaning SMART information:"),
              div(
                style = "display: flex; align-items: center;",
                uiOutput("cleaning_survey_ui"),
                
                tags$div(style = "margin-left: 10px;",
                         bsButton("cleaning_survey_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
                         bsTooltip("cleaning_survey_info", "Each folder needs to contain: nutrition, mortality and cluster information.", "right")
                )),
              verbatimTextOutput("cleaning_survey_path_text"),
              br(),
              selectInput("type_cleaning", "Select the type of cleaning:",
                          choices = c('Norm WHO 2006' = "who_2006", 
                                      'Norm used in the ENA Software' = "smart_flag", 
                                      'Norm WHO 1995' ="who_1995_survey", 
                                      'Norm WHO 1995 (growth)' = "who_1995_growth", 
                                      'Norm Epi' = "epi_info")),
              br(),
              actionButton("cleaning_button", "Start Cleaning", 
                           style="color: #fff; background-color: #f15d20; border-color: #f12020"),
              br(), br(), br(),
              plotOutput("cleaning_plot"))
      ),
      #### VISUALIZATION COVERAGE ----------------------------------------
      tabItem(tabName = "coverage_smart",
              h2(style = "color: #3652a8; margin-left: 200px;;",  "Visualization of SMART Surveys coverage"), br(),
              div(style="max-width: 900px; float: left;; text-align: justify;", 
              p("This tab provides different visualization and will save them in a folder called", 
                tags$b("visualization_output"), "."),
              h4("Parameters:"),
              p(tags$ol(
                tags$li(tags$b("Folder containing SMART Surveys cleaned:"), " Select the folder with the cleaned SMART Surveys."),
                br(),
                tags$li(tags$b("Type of plot:"), " Select the plot you want to visualize and save. For each plot, click the ", tags$b('"Start"'), " button to launch the process.")
                )),
              hr(style = "border-top: 1px solid #000000;"),
              # Title before the button
              tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder contaning the cleaned SMART information:"),
              div(
                style = "display: flex; align-items: center;",
                uiOutput("coverage_survey_ui"),
                
                tags$div(style = "margin-left: 10px;",
                         bsButton("coverage_survey_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
                         bsTooltip("coverage_survey_info", "Folder containing the metadata cleaning file.", "right")
                )),
              verbatimTextOutput("coverage_survey_path_text"),
              br(),
              selectInput("plot_coverage", "Select the type of plot you want to visualize:",
                          choices = c('Quality Score Histogram' = "qs_hist", 
                                      'Sample Size Histogram' = "ss_hist", 
                                      'Recall Days Histogram' ="rd_hist", 
                                      'SMART Coverage' = "smart_coverage")),
             actionButton("coverage_button", "Start Visualization", 
                          style="color: #fff; background-color: #f15d20; border-color: #f12020"),
             fluidRow(column(12,align="center",
                             plotOutput("coverage_plot"))
             ))
            
      ),
      #### VISUALIZATION ANTHRO ----------------------------------------
      tabItem(tabName = "anthro_smart",
              h2(style = "color: #3652a8; margin-left: 200px;;",  "Visualization of SMART Surveys anthropology"), br(),
              div(style="max-width: 900px; float: left;; text-align: justify;", 
                  p("This tab provides different visualization and will save them in a folder called", 
                    tags$b("visualization_output"), "."),
                  h4("Parameters:"),
                  p(tags$ol(
                    tags$li(tags$b("Folder containing SMART Surveys cleaned:"), " Select the folder with the cleaned SMART Surveys."),
                    br(),
                    tags$li(tags$b("Type of plot:"), " Select the plot you want to visualize and save. For each plot, click the ", tags$b('"Start"'), " button to launch the process.")
                  )),
                  hr(style = "border-top: 1px solid #000000;"),
                  # Title before the button
                  tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder contaning the cleaned SMART information:"),
                  div(
                    style = "display: flex; align-items: center;",
                    uiOutput("anthro_survey_ui"),
                    tags$div(style = "margin-left: 10px;",
                             bsButton("anthro_survey_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
                             bsTooltip("anthro_survey_info", "Folder containing the metadata cleaning file.", "right")
                    )),
              verbatimTextOutput("anthro_survey_path_text"),
              br(),
              selectInput("plot_anthro", "Select the type of plot you want to visualize:",
                          choices = c('Hist of Flagged WHZ' = "hist_whz", 
                                      'Hist of Flagged HAZ' = "hist_haz",
                                      'Hist of Flagged WAZ' = "hist_waz",
                                      'Hist of Flagged bio' = "hist_bio",
                                      'Flagged WHZ per SMART Survey' = "whz_year", 
                                      'Flagged HAZ per SMART Survey' = "haz_year",
                                      'Flagged WAZ per SMART Survey' = "waz_year",
                                      'Percentage boys/girls per SMART Survey' ="perc_boy_girls")),
               actionButton("anthro_button", "Start Visualization", 
                                    style="color: #fff; background-color: #f15d20; border-color: #f12020"),
              br(),
              fluidRow(column(12,align="center",
                              plotOutput("anthro_plot")))
              )
      ),
      #### VISUALIZATION MORTALITY ----------------------------------------
      tabItem(tabName = "mortality_smart",
              h2(style = "color: #3652a8; margin-left: 200px;;",  "Visualization of SMART Surveys mortality"), br(),
              div(style="max-width: 900px; float: left;; text-align: justify;", 
                  p("This tab provides different visualization and will save them in a folder called", 
                    tags$b("visualization_output"), "."),
                  h4("Parameters:"),
                  p(tags$ol(
                    tags$li(tags$b("Folder containing SMART Surveys cleaned:"), " Select the folder with the cleaned SMART Surveys."),
                    br(),
                    tags$li(tags$b("Type of plot:"), " Select the plot you want to visualize and save. For each plot, click the ", tags$b('"Start"'), " button to launch the process.")
                  )),
                  hr(style = "border-top: 1px solid #000000;"),
              # Title before the button
              tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder contaning the cleaned SMART information:"),
              div(
                style = "display: flex; align-items: center;",
                uiOutput("mortality_survey_ui"),
                tags$div(style = "margin-left: 10px;",
                         bsButton("mortality_survey_info", label = "", icon = icon("question-circle"), style = "info", size = "extra-small"),
                         bsTooltip("mortality_survey_info", "Folder containing the metadata cleaning file.", "right")
                )),
              verbatimTextOutput("mortality_survey_path_text"),
              br(),
              selectInput("plot_mortality", "Select the parameters you want to visualize:",
                          choices = c('CDR Over Time' = "cdr_plot", 
                                      'Under 5-yearsDR Over Time' = "cdr_u5_plot")),
             actionButton("mortality_button", "Start Visualization", 
                                    style="color: #fff; background-color: #f15d20; border-color: #f12020"),
              br(),
             fluidRow(column(12,align="center",
                             plotOutput("mortality_plot")))
              )
      )
    )
  )
)

# Define server logic required to draw the plots
server <- function(input, output, session) {
  # Enable shinyFiles
  volumes = getVolumes()()
  #roots <- c(home = fs::path_home(), root = "/")
  
  ## EXTRACTION FOLDER -----------------------------------------------
  shinyDirChoose(input, "survey_path", roots = volumes)
  shinyDirChoose(input, "output_path", roots = volumes)
  survey_path <- reactive({
    return(parseDirPath(roots = volumes, input$survey_path))
  })
  
  
  output_path <- reactive({
    return(parseDirPath(roots = volumes, input$output_path))
  })
  
  output$survey_path_text <- renderText({
    survey_path()
  })
  
  output$output_path_text <- renderText({
    output_path()
  })
  
  # Reactive value to store whether the extract button has been pushed
  extract_pushed <- reactiveVal(FALSE)
  
  # Dynamically show fileInput when "Add a country" is selected
  output$country_file_ui <- renderUI({
    if (input$country == "Add a country") {
      tagList(
        textInput("new_country_name", "Enter the three first letter of the new country:", ""),
        fileInput("country_file", "Upload a file with country data:", 
                  accept=c('.rds', '.rda', '.csv', '.xlsx')),
        p("Below is an overview of the expected file format:"),
        dataTableOutput("sample_file_preview")
      )
    } else if(input$country == 'No Specified Country'){
      tagList(
        textInput("new_country_name", "Enter the three first letter of the country under study (or NSC for no specific country):", ""),
        
      )
    }
  })
  
  # Render the sample file preview
  output$sample_file_preview <- renderDataTable({
    if (input$country == "Add a country") {
      sample_file <- smartextract::som_admins_options[1:5,]  # Path to your sample file
      DT::datatable(sample_file, options = list(dom = 't'))
    }
  })
  
  
  observeEvent(input$extract_button, {
    country <- input$country
    country_data <- NA
    if (country == "Add a country") {
        # Handle file upload and new country name
        new_country_name <- input$new_country_name
        country_data <- rio::import(input$country_file$datapath) |>
          dplyr::mutate(eventual_name = tolower(gsub("[[:punct:]]+", "", eventual_name)))
        country <- new_country_name
    }else if(country == 'No Specified Country'){
      new_country_name <- input$new_country_name
      country <- new_country_name
      country_data <- 'unspecified'
    }
    # Call the function f_sort_smart with the selected parameters
    smartextract::f_sort_smart(smart_folder = paste(survey_path(), '/', sep=""), 
                               output_folder =paste(output_path(), '/', sep=""), 
                               country = country, 
                               country_data = country_data)
    
    # Check for issues and handle "Unknown Date"
    handle_issues(smart_folder = paste(survey_path(), '/', sep=""), 
                  output_dir= paste(output_path(), '/', sep=""), 
                  country = country, session=session, 
                  country_data = country_data)
    
    # After handling issues, generate a ggplot2 plot
    output$extraction_plot <- renderPlot({
      log_file <- rio::import(paste(paste(output_path(), '/', 'metadata.csv', sep="")))
      smartextract::f_plot_log_file(log_file)
    })
    # Set the reactive value to TRUE indicating that extract button has been pushed
    extract_pushed(TRUE)
  })
  
  ## CLEANING FOLDER ------------------------------------------------------
  shinyDirChoose(input, "cleaning_survey_path", roots = volumes)
  
  output$cleaning_survey_ui <- renderUI({
    if (extract_pushed()) {
      tagList(
        textInput("cleaning_survey_path_text", "Select folder contaning SMART extracted", value = output_path()),
        shinyDirButton("cleaning_survey_path", "Select Survey Folder", "Please select the folder containing SMART Surveys", 
                       FALSE, class = "cleaning-survey-button")
      )
    } else {
      tagList(
        shinyDirButton("cleaning_survey_path", "Select survey contaning SMART extracted", 
                       "Please select the folder containing SMART Surveys", FALSE, 
                       class = "cleaning-survey-button")
      )
    }
  })
  
  
  output$cleaning_survey_path_text <- renderText({
    parseDirPath(roots = volumes, input$cleaning_survey_path)
  })
  # Reactive value to store whether the extract button has been pushed
  cleaning_pushed <- reactiveVal(FALSE)
  
  observeEvent(input$cleaning_button, {
    cleaning_survey_path <- if(extract_pushed()) {
      input$cleaning_survey_path_text
    } else {
      parseDirPath(roots = volumes, input$cleaning_survey_path)
    }
    smartcleaning::f_cleaning(all_survey_folder = paste(cleaning_survey_path, '/', sep=""),
                              cleaning_criteria=input$type_cleaning)
    
    log_file <- rio::import(paste(cleaning_survey_path, '/metadata.csv', sep=""))
    # After handling issues, generate a ggplot2 plot
    output$cleaning_plot <- renderPlot({
      smartextract::f_plot_log_file(log_file)
    })
    # Set the reactive value to TRUE indicating that extract button has been pushed
    cleaning_pushed(TRUE)
  })
  
  ### ------------------------------- COVERAGE TAB ----------------
  
  shinyDirChoose(input, "coverage_survey_path", roots = volumes)
  
  output$coverage_survey_ui <- renderUI({
    if (cleaning_pushed()) {
      tagList(
        textInput("coverage_survey_path_text", "Select folder contaning metadata from SMART", value = output_path()),
        shinyDirButton("coverage_survey_path", "Select Survey Folder", "Please select the folder containing SMART Surveys", 
                       FALSE, class = "coverage-survey-button")
      )
    } else {
      tagList(
        shinyDirButton("coverage_survey_path", "Select folder contaning metadata from SMART", 
                       "Please select the folder containing SMART Surveys", FALSE, 
                       class = "coverage-survey-button")
      )
    }
  })
  
  output$coverage_survey_path_text <- renderText({
    parseDirPath(roots = volumes, input$coverage_survey_path)
  })

  v <- reactiveValues(height = NULL, 
                      width = NULL, 
                      coverage_survey_path=NULL, 
                      type_plot = NULL)
  
  observeEvent(input$coverage_button, {
    v$coverage_survey_path <- if(cleaning_pushed()) {
      input$coverage_survey_path_text
    } else {
      parseDirPath(roots = volumes, input$coverage_survey_path)
    }
    ## Define the size of the plot
    if(input$plot_coverage == 'smart_coverage'){
      v$height <- 200
      v$width <- 500
    }else{
      v$height <- 300
      v$width <- 300
    }
    v$type_plot <- input$plot_coverage
    output$coverage_plot <- renderPlot({
      smartmetadata::f_plot_coverage_tab(folder_name = v$coverage_survey_path, 
                                         type_plot = v$type_plot)
    }, height = function() { v$height }, width = function() { v$width })
  })
  
  
  
  ### ------ PART ANTHRO PLOT -------------------------------------------------
  shinyDirChoose(input, "anthro_survey_path", roots = volumes)
  
  output$anthro_survey_ui <- renderUI({
    if (cleaning_pushed()) {
      tagList(
        textInput("anthro_survey_path_text", "Select folder contaning metadata from SMART", value = output_path()),
        shinyDirButton("anthro_survey_path", "Select Survey Folder", "Please select the folder containing SMART Surveys", 
                       FALSE, class = "anthro-survey-button")
      )
    } else {
      tagList(
        shinyDirButton("anthro_survey_path", "Select folder contaning metadata from SMART", 
                       "Please select the folder containing SMART Surveys", FALSE, 
                       class = "anthro-survey-button")
      )
    }
  })
  
  output$anthro_survey_path_text <- renderText({
    parseDirPath(roots = volumes, input$anthro_survey_path)
  })
  
  v <- reactiveValues(height = NULL, 
                      width = NULL, 
                      anthro_survey_path=NULL, 
                      type_plot = NULL)
  
  observeEvent(input$anthro_button, {
    v$anthro_survey_path <- if(cleaning_pushed()) {
      input$anthro_survey_path_text
    } else {
      parseDirPath(roots = volumes, input$anthro_survey_path)
    }
    ## Define the size of the plot
    if(grepl(pattern = 'hist', x=input$plot_anthro)){
      v$height <- 300
      v$width <- 300
    }else{
      v$height <- 500
      v$width <- 1000
    }
    v$type_plot <- input$plot_anthro
    output$anthro_plot <- renderPlot({
      smartmetadata::f_plot_nutrition_tab(folder_name = v$anthro_survey_path, 
                                          type_plot = v$type_plot)
    }, height = function() { v$height }, width = function() { v$width })
  })
  
 
  

  ### ------ PART MORTALITY PLOT -------------------------------------------------
  shinyDirChoose(input, "mortality_survey_path", roots = volumes)
  
  output$mortality_survey_ui <- renderUI({
    if (cleaning_pushed()) {
      tagList(
        textInput("mortality_survey_path_text", "Select folder contaning metadata from SMART", value = output_path()),
        shinyDirButton("mortality_survey_path", "Select Survey Folder", "Please select the folder containing SMART Surveys", 
                       FALSE, class = "mortality-survey-button")
      )
    } else {
      tagList(
        shinyDirButton("mortality_survey_path", "Select folder contaning metadata from SMART", 
                       "Please select the folder containing SMART Surveys", FALSE, 
                       class = "mortality-survey-button")
      )
    }
  })
  
  output$mortality_survey_path_text <- renderText({
    parseDirPath(roots = volumes, input$mortality_survey_path)
  })
  
  v <- reactiveValues(height = NULL, 
                      width = NULL, 
                      mortality_survey_path=NULL, 
                      type_plot = NULL)
  
  observeEvent(input$mortality_button, {
    v$mortality_survey_path <- if(cleaning_pushed()) {
      input$mortality_survey_path_text
    } else {
      parseDirPath(roots = volumes, input$mortality_survey_path)
    }
    ## Define the size of the plot
    v$height <- 600
    v$width <- 500
    v$type_plot <- input$plot_mortality
    output$mortality_plot <- renderPlot({
      smartmetadata::f_plot_morality_tab(folder_name = v$mortality_survey_path, 
                                         type_plot = v$type_plot)
    }, height = function() { v$height }, width = function() { v$width })
  })
  
  
  
  
  ### FUNCTION FOR THE EXTRACT PART GENERATE UIS ------------------------------------------
  
  # Function to handle issues such as "Unknown Date"
  handle_issues <- function(smart_folder, output_dir, country, session, country_data){
    # Check for issues in the output directory
    # Assuming issues are stored in a specific folder like "Issue"
    issue_folder <- file.path(output_dir, "smart_with_issue")
    log_file <- read.csv(paste(output_dir, 'metadata.csv', sep=""))
    # List files in the issue folder
    files <- list.files(paste(output_dir, 'smart_with_issue', sep=""))
    files <- log_file[log_file$old_name %in% files & log_file$type_of_issue == 'Unknown Admin', 'old_name']
    nb_files <- length(files)
    ## possible localization
    if(country == 'SOM'){
      admin_list <- smartextract::f_location_possibility('SOM')
      possible_localizations <- unique(tolower(sort(unique(admin_list$right_name))))
    }else if(country == 'KEN'){
      admin_list <- smartextract::f_location_possibility('KEN')
      possible_localizations <- unique(tolower(sort(unique(admin_list$right_name))))
    }else{
      possible_localizations <- unique(tolower(sort(country_data$right_name)))
    }
    # Create modal dialog for unknown admin issue
    showModal(modalDialog(
      title = "Issue: Unknown Admin",
      lapply(1:nb_files, function(ind) {
        file <- files[ind]
        tagList(
          p(paste('The SMART survey ', file, 'has an issue.')),
          selectInput(paste("new_localization_", ind, sep=""),
                      "Please select the right localization:",
                      choices = c("Not found", possible_localizations))
        )
      }),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_button", "OK")
      )
    ))
    
    observeEvent(input$ok_button, {
      for(ind in 1:nb_files) {
        new_localization <- input[[paste("new_localization_", ind, sep="")]]
        if(new_localization != "Not found") {
          log_file <- read.csv(paste(output_dir, 'metadata.csv', sep=""))
          admin_name <- tolower(gsub("[[:punct:][:blank:]]+", "", new_localization))
          smartextract::f_deal_issues(
            country = country,
            file = files[ind],
            admin_name = admin_name,
            log_file = log_file,
            smart_folder = smart_folder,
            output_dir = output_dir,
            country_data = country_data
          )
        }
      }
      removeModal()
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
