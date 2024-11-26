## Function to install and load required packages------------------------
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
  "shiny", "shinydashboard", "shinyFiles", 'bs4Dash',
  "shinyBS", "DT", "RColorBrewer", "ggplot2", 'shinyjs',
  "cowplot", "nipnTK", "ggh4x", 'remotes', 
  'dplyr', 'rio', 'lubridate', 'officer',
  'flextable', 'survey', 'shinycssloaders'
)

lapply(cran_packages, install_and_load)

# Check and install GitHub packages
install_and_load("smartextract", "yamnao/smartextract")
install_and_load("smartcleaning", "yamnao/smart_cleaning")
install_and_load("smartmetadata", "yamnao/smart_visualization")
install_and_load("mast", "afyac/mast")

## Define UI for the application ---------------------------------------
ui <- dashboardPage(
  # Header
  dashboardHeader(title = "REASSURe"),
  # Tab menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "infos", icon = icon("info")),
      menuItem("Data Extraction", tabName = "extraction", icon = icon("cogs")),
      menuItem("Data Preprocessing", tabName = "cleaning", icon = icon("broom")),
      menuItem("Coverage Visualization", tabName = "coverage_smart", icon = icon("chart-pie")),
      menuItem("Anthropology Visualization", tabName = "anthro_smart", icon = icon("chart-line")),
      menuItem("Mortality Visualization", tabName = "mortality_smart", icon = icon("chart-bar"))
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
      tabItem(
        tabName = "infos",
        div(
          style = " margin-bottom: 20px;",
          tags$img(src = "LSHTM_Logo.jpg", width = 150), # Centered and slightly larger logo
          h2(
            style = "text-align: center;color: #3652a8; font-weight: bold;",
            "Welcome to the REASSURe App"
          )
        ),
        div(
          style = "max-width: 900px; margin: 0 auto; text-align: justify; padding: 10px;",
          tags$p(
            style = "font-size: 16px; line-height: 1.5;",
            "This tool is designed to help ",
            tags$b("extracting, cleaning, and visualizing multiple Standardized Monitoring and Assessment of Relief and Transitions (SMART) surveys simultaneously.")
          ),
          tags$p(
            style = "font-size: 16px; line-height: 1.5;",
            "Developed by LSHTM with support from UNICEF, it simplifies the management and analysis of SMART Surveys. ",
            "SMART surveys are essential for evaluating nutritional status and mortality. Typically, these surveys are analyzed using the ENA software (https://smartmethodology.org), which handles only one survey at a time. ",
            "In order to help projects requiring the analysis of hundreds of SMART surveys at different administrative levels, this tool has been developed. ",
            "It applies the results of the ENA to several surveys simultaneously and includes additional functionality to improve analysis and management."
          ),
          br(),
          h4(style = "color: #3652a8; font-weight: bold;", "REASSURe is composed of several tabs:"),
          tags$ul(
            style = "padding-left: 20px; font-size: 16px;",
            tags$li(
              tags$b("Data Extraction"),
              ": Extracting SMART Survey content, including nutrition, mortality, and clusters data, along with metadata such as administrative levels."
            ),
            tags$li(
              tags$b("Data Preprocessing"),
              ": Cleaning nutrition and mortality data by handling missing information, filtering outliers, and removing impossible values based on WHO standards."
            ),
            tags$li(
              tags$b("Coverage Visualization"),
              ": Visualizing SMART Survey coverage over time or by quality score."
            ),
            tags$li(
              tags$b("Anthropology Visualization"),
              ": Visualizing nutritional data from SMART surveys, including flagged data or gender distribution."
            ),
            tags$li(
              tags$b("Mortality Visualization"),
              ": Visualizing mortality data from the SMART surveys, such as crude death rates over time."
            )
          ),
          br(),
          tags$p(
            style = "font-size: 16px; line-height: 1.5;",
            "For further details, please consult the documentation."
          )
        )
      ),
      #### EXTRACTION TAB ---------------------------------------------
      tabItem(
        tabName = "extraction",
        h2(style = "color: #3652a8;", "Extraction"), 
        br(),
        # Texte rétractable dans une carte
        bs4Card(
          title = "Explanation",
          collapsible = TRUE,
          width = 12,
          status = "info",
          div(
            style = "font-size: 16px; line-height: 1.5; text-align: justify;",
            p("REASSURe extracts SMART survey their contents (clusters, nutrition, and mortality data) into a specified output folder. 
                In fact, each SMART survey will have its own folder containing four files: nutrition, mortality, cluster, and the raw SMART file."),
            p("In addition, a metadata excel file will be generated, listing the name, location, and date of each SMART survey."),
            tags$b(p("In order to extract SMART contents, the following parameters need to be precised:")),
            tags$ol(
              tags$li(tags$b("Select SMART localisation: "), "Select the country where the SMART surveys were conducted."),
              tags$ul(
                tags$li("The 'KEN' option refers to Kenya."),
                tags$li("The 'SOM' option refers to Somalia"),
                tags$li("The 'No Specified Country' option can be used if the location is irrelevant or surveys were conducted in multiple countries."),
                tags$li("The 'Add a Country' option can be used if SMART have been done in an other country.", tags$i("Refer to the documentation for the additional content required in this case."))
              ),
              br(),
              tags$li(tags$b("Folder containing SMART Surveys:"), " Select the folder containing the .as files."),
              br(),
              tags$li(tags$b("Folder to save the results:"), " Select the folder to save the extracted content.")
            )
          )
        ),
        
        # Section pour les boutons et les sélecteurs
        bs4Card(
          title = "Parameters",
          status = "primary",
          width = 12,
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 20px;",
            
            # Sélection du pays
            div(
              style = "flex: 1; margin-right: 10px;",
              selectInput("country", "Select SMART localisation:",
                          choices = c("", "No Specified Country", "KEN", "SOM", "Add a country"))
            ),
            # Bouton info
            div(
              style = "margin-left: 10px;",
              actionButton("country_info", label = "", icon = icon("question-circle"),
                           class = "btn btn-info btn-sm", 
                           `data-toggle` = "tooltip", 
                           title = "If the location is irrelevant, select 'No Specified Country'.")
            )
          ),
          uiOutput("country_file_ui"),
          # Dossier contenant les fichiers SMART
          div(
            style = "margin-top: 20px;",
            tags$div(style = "font-weight: bold;", "Select the folder containing SMART Surveys:"),
            div(
              style = "display: flex; align-items: center;",
              shinyDirButton("survey_path", "Select folder containing .as files (SMART Surveys)", 
                             "Select Survey Folder", FALSE, class = "cleaning-survey-button"),
              div(
                style = "margin-left: 10px;",
                actionButton("survey_path_info", label = "", icon = icon("question-circle"),
                             class = "btn btn-info btn-sm", 
                             `data-toggle` = "tooltip", 
                             title = "All the .as files need to be in the same folder if you want to extract them.")
              )
            ),
            verbatimTextOutput("survey_path_text")
          ),
          
          # Dossier de sortie
          div(
            style = "margin-top: 20px;",
            tags$div(style = "font-weight: bold;", "Select folder to save the results:"),
            div(
              style = "display: flex; align-items: center;",
              shinyDirButton("output_path", "Select folder to save SMART Surveys content", 
                             "Select Output Folder", FALSE, class = "cleaning-survey-button"),
              div(
                style = "margin-left: 10px;",
                actionButton("output_path_info", label = "", icon = icon("question-circle"),
                             class = "btn btn-info btn-sm", 
                             `data-toggle` = "tooltip", 
                             title = "Results folder must be different from the SMART survey folder (one folder per country).")
              )
            ),
            verbatimTextOutput("output_path_text")
          ),
          
          # Bouton d'extraction centré
          div(
            style = "text-align: center; margin-top: 30px;",
            actionButton("extract_button", "Start Extraction", 
                         style = "color: #fff; background-color: #f15d20; border-color: #f12020; font-size: 16px; padding: 10px 20px;")
          )
        ),
        
        # Placeholder pour les graphiques
        bs4Card(
          title = "Visualization",
          status = "secondary",
          collapsible = TRUE,
          width = 12,
          withSpinner(plotOutput("extraction_plot"), type = 6, color = "#f15d20")
        )
      ),
      
      #### CLEANING TAB ---------------------------------------------
      tabItem(
        tabName = "cleaning",
        h2(style = "color: #3652a8;", "Preprocessing & Cleaning Nutrition Information"), 
        br(),
        bs4Card(
          title = "Explanation",
          collapsible = TRUE,
          width = 12,
          status = "info",
          div(
            style = "font-size: 16px; line-height: 1.5; text-align: justify;",
            p("REASSURe will clean the SMART Surveys by addressing missing values, filtering outliers, and ensuring dataset consistency (based on this paper: ", 
              a(href = "https://pubmed.ncbi.nlm.nih.gov/24883244/", "PubMed Article"), ").",
              "The output folder will contain cleaned nutrition, mortality, and cluster Excel files, along with a 
                cleaned metadata file containing quality scores and other mortality metrics."),
            tags$b(p("In order to clean the SMART contents, the following parameters need to be precised:")),
            tags$ol(
              tags$li(tags$b("Folder containing SMART Surveys extracted:"), " Select the folder with the extracted SMART Survey data (from the first tab)."),
              br(),
              tags$li(tags$b("Type of cleaning:"), " Select the type of cleaning process you want to execute.")
            ),
            p("Once you have provided the required data, click on the ", 
              tags$b('"Start cleaning"'), " button to initiate the data cleaning process.")
        )),
        bs4Card(
          title = "Parameters",
          collapsible = TRUE,
          width = 12,
          status = "primary",
          tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder contaning SMART information:"),
          div(
            style = "display: flex; align-items: center; margin-top: 20px;",
            uiOutput("cleaning_survey_ui"),
            div(
              style = "margin-left: 20px;",
              actionButton("cleaning_survey_info", label = "", icon = icon("question-circle"),
                           class = "btn btn-info btn-sm", 
                           `data-toggle` = "tooltip", 
                           title = "All the .as files need to be in the same folder if you want to extract them.")
            )),
          verbatimTextOutput("cleaning_survey_path_text"),
          div(
            style = "flex: 1; align-items: center; margin-top: 20px;",
            selectInput("type_cleaning", "Select the type of cleaning:",
                        choices = c('Norm WHO 2006' = "who_2006", 
                                    'Norm used in the ENA Software' = "smart_flag", 
                                    'Norm WHO 1995' ="who_1995_survey", 
                                    'Norm WHO 1995 (growth)' = "who_1995_growth", 
                                    'Norm Epi' = "epi_info")),
          ),
          actionButton("cleaning_button", "Start Cleaning", 
                       style="color: #fff; background-color: #f15d20; border-color: #f12020")
        ),
      bs4Card(
        title = "Visualizations",
        collapsible = TRUE,
        width = 12,
        status = "secondary",
        # Add spinner around the explanatory text and image
        withSpinner(
          uiOutput("cleaning_content"), # Combined output for text and image
          type = 6, 
          color = "#f15d20"
        )
        )),
      
      #### VISUALIZATION COVERAGE ----------------------------------------
      tabItem(tabName = "coverage_smart",
              h2(style = "color: #3652a8;",  "Coverage"), 
              br(),
              bs4Card(
                title = "Explanation",
                collapsible = TRUE,
                width = 12,
                status = "info",
                div(style = "font-size: 16px; line-height: 1.5; text-align: justify;", 
                  p("This tab provides different visualizations to understand the coverage provided by the SMART extracted.
                    In this tab, you can plot quality scores, sample sizes, recall days and the coverage over years.
                    In addition, all of these plots would be saved in a folder called", 
                    tags$b("visualization_output"), "."),
                  tags$b(p("In order to understand the coverage of SMART extracted, the following parameters need to be precised:")),
                  p(tags$ol(
                    tags$li(tags$b("Folder containing SMART Surveys cleaned:"), " Select the folder with the cleaned SMART Surveys."),
                    br(),
                    tags$li(tags$b("Type of plot:"), 
                            " Select the plot you want to visualize. For each plot, click the ", 
                            tags$b('"Start Visualization"'), " button to launch the process.")
                  ))
                  )
              ),
              bs4Card(
                title = "Parameters",
                collapsible = TRUE,
                width = 12,
                status = "primary", 
                # Title before the button
                tags$div(
                  style = "font-weight: bold; align-items: center; margin-right: 10px;", "Select folder contaning the cleaned SMART information:"),
                div(
                  style = "display: flex; align-items: center; margin-top: 20px;",
                  uiOutput("coverage_survey_ui"),
                  div(
                    style = "margin-left: 20px;",
                    actionButton("coverage_survey_info", label = "", icon = icon("question-circle"),
                                 class = "btn btn-info btn-sm", 
                                 `data-toggle` = "tooltip", 
                                 title = "Select the folder where the SMART contents have been extracted or where the 'metadata_clean.csv' file is saved"),
                    
                  )
                  ),verbatimTextOutput("coverage_survey_path_text"),
                div(style = "flex: 1; align-items: center; margin-top: 20px;",
                    selectInput("plot_coverage", "Select the type of plot you want to visualize:",
                                choices = c('Quality Score Histogram' = "qs_hist", 
                                            'Sample Size Histogram' = "ss_hist", 
                                            'Recall Days Histogram' ="rd_hist", 
                                            'SMART Coverage' = "smart_coverage"))
                ),
              div(style = "display: flex; align-items: center; margin-top: 20px;",
                  actionButton("coverage_button", "Start Visualization", 
                               style="color: #fff; background-color: #f15d20; border-color: #f12020")
                  )
              ),
              bs4Card(
                title = "Visualizations",
                collapsible = TRUE,
                width = 12,
                status = "secondary",
                # Add spinner around the explanatory text and image
                withSpinner(
                  div(
                    style = "display: flex; justify-content: center;",  # Center the content
                    plotOutput(
                      "coverage_plot", 
                      height = "400px",  # Adjust height as needed
                      width = "50%"      # Set the width of the plot relative to the container
                    )
                  ),
                  type = 6, 
                  color = "#f15d20"
                )
                )
              
      ),
      #### VISUALIZATION ANTHRO ----------------------------------------
      tabItem(tabName = "anthro_smart",
              h2(style = "color: #3652a8;",  "Anthropology"),
              br(),
              bs4Card(
                title = "Explanation",
                collapsible = TRUE,
                width = 12,
                status = "info",
                div(style = "font-size: 16px; line-height: 1.5; text-align: justify;",  
                p("This tab provides different visualizations to understand the anthropology data provided by the SMART extracted.
                In this tab, you can plot flagged WHZ, WAz and HAZ and percentage of boys/girls.
                In addition, all of these plots would be saved in a folder called", 
                  tags$b("visualization_output"), "."),
                tags$b(p("In order to understand the anthropology data of SMART extracted, the following parameters need to be precised:")),
                p(tags$ol(
                  tags$li(tags$b("Folder containing SMART Surveys cleaned:"), " Select the folder with the cleaned SMART Surveys."),
                  br(),
                  tags$li(tags$b("Type of plot:"), " Select the plot you want to visualize and save. For each plot, click the ", 
                          tags$b('"Start Visualization"'), 
                          " button to launch the process.")
                  )
                  )
                )
              ),
              bs4Card(
                title = "Parameters",
                collapsible = TRUE,
                width = 12,
                status = "primary", 
                # Title before the button
                tags$div(style = "font-weight: bold; margin-right: 20px;", "Select folder contaning the cleaned SMART information:"),
                div(
                  style = "display: flex; align-items: center; margin-top: 20px;",
                  uiOutput("anthro_survey_ui"),
                  div(
                    style = "margin-left: 20px;",
                    actionButton("anthro_survey_info", label = "", icon = icon("question-circle"),
                                 class = "btn btn-info btn-sm", 
                                 `data-toggle` = "tooltip", 
                                 title = "Select the folder where the SMART contents have been extracted or where the 'metadata_clean.csv' file is saved."),
                    
                  )),
                verbatimTextOutput("anthro_survey_path_text"),
                div(style = "flex: 1; align-items: center; margin-top: 20px;",
                  selectInput("plot_anthro", "Select the type of plot you want to visualize:",
                              choices = c('Hist of Flagged WHZ' = "hist_whz", 
                                          'Hist of Flagged HAZ' = "hist_haz",
                                          'Hist of Flagged WAZ' = "hist_waz",
                                          'Hist of Flagged bio' = "hist_bio",
                                          'Flagged WHZ per SMART Survey' = "whz_year", 
                                          'Flagged HAZ per SMART Survey' = "haz_year",
                                          'Flagged WAZ per SMART Survey' = "waz_year",
                                          'Percentage boys/girls per SMART Survey' ="perc_boy_girls"))
                ),
                div(
                  style = "display: flex; align-items: center; margin-top: 20px;",
                  actionButton("anthro_button", "Start Visualization", 
                               style="color: #fff; background-color: #f15d20; border-color: #f12020")
                )
                ),
              bs4Card(
                title = "Visualizations",
                collapsible = TRUE,
                width = 12,
                status = "secondary",
                # Add spinner around the explanatory text and image
                withSpinner(
                  div(
                    style = "display: flex; justify-content: center;",  # Center the content
                    plotOutput(
                      "anthro_plot"
                    )
                  ),
                  type = 6, 
                  color = "#f15d20"
                )
              )
              ),
      #### VISUALIZATION MORTALITY ----------------------------------------
      tabItem(tabName = "mortality_smart",
              h2(style = "color: #3652a8;",  "Mortality"),
              br(),
              bs4Card(
                title = "Explanation",
                collapsible = TRUE,
                width = 12,
                status = "info",
                div(style="max-width: 900px; float: left;; text-align: justify;", 
                p("This tab provides different visualizations to understand the mortality data provided by the SMART extracted.
                  In this tab, you can plot crude death rate (cdr) and crude death rate under 5 (under 5-year) over time.
                  In addition, all of these plots would be saved in a folder called", 
                  tags$b("visualization_output"), "."),
                tags$b(p("In order to understand the mortality data of SMART extracted, the following parameters need to be precised:")),
                p(tags$ol(
                    tags$li(tags$b("Folder containing SMART Surveys cleaned:"), " Select the folder with the cleaned SMART Surveys."),
                    br(),
                    tags$li(tags$b("Type of plot:"), " Select the plot you want to visualize and save. For each plot, click the ", 
                            tags$b('"Start Visualization"'), " button to launch the process.")
                  ))
                )
                ),
              bs4Card(
                title = "Parameters",
                collapsible = TRUE,
                width = 12,
                status = "primary", 
                # Title before the button
                tags$div(style = "font-weight: bold; margin-right: 10px;", "Select folder contaning the cleaned SMART information:"),
                div(
                  style = "display: flex; align-items: center; margin-top: 20px;",
                  uiOutput("mortality_survey_ui"),
                  div(
                    style = "margin-left: 20px;",
                    actionButton("mortality_survey_info", label = "", icon = icon("question-circle"),
                                 class = "btn btn-info btn-sm", 
                                 `data-toggle` = "tooltip", 
                                 title = "Select the folder where the SMART contents have been extracted or where the 'metadata_clean.csv' file is saved.")
                    
                  )
                  ),
                verbatimTextOutput("mortality_survey_path_text"),
                div(
                  style = "flex: 1; align-items: center; margin-top: 20px;",
                  selectInput("plot_mortality", "Select the parameters you want to visualize:",
                                choices = c('CDR Over Time' = "cdr_plot", 
                                            'Under 5-yearsDR Over Time' = "cdr_u5_plot"))
                  ),
                div(
                  style = "display: flex; align-items: center; margin-top: 20px;",
                  actionButton("mortality_button", "Start Visualization", 
                                 style="color: #fff; background-color: #f15d20; border-color: #f12020"))
              ),
              bs4Card(
                title = "Visualizations",
                collapsible = TRUE,
                width = 12,
                status = "secondary",
                div(
                  style = "justify-content: center; align-items: center;",
                  plotOutput("mortality_plot")
                )
              )
      )
    )
  )
)

## Define server logic required to draw the plots---------------------------------
server <- function(input, output, session) {
  # Enable Bootstrap tooltips
  runjs('$(function () { $(\'[data-toggle="tooltip"]\').tooltip(); })')
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
  
  #Dynamically show fileInput when "Add a country" is selected
  output$country_file_ui <- renderUI({
    if (input$country == "Add a country") {
      tagList(
        textInput("new_country_name", "Enter the three first letters of the new country:", ""),
        fileInput("country_file", "Upload a file with country data:", 
                  accept = c('.rds', '.rda', '.csv', '.xlsx')),
        p("Below is an overview of the expected file format:"),
        dataTableOutput("sample_file_preview")
      )
    } else if (input$country == "No Specified Country") {
      tagList(
        textInput("new_country_name", "Enter the three first letters of the country under study (or NSC for no specific country):", "")
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
    
  
  # Handle the "Start Extraction" button
  observeEvent(input$extract_button, {
    shinyjs::disable("extract_button")
    on.exit(shinyjs::enable("extract_button"))
    
    # Clear the existing plot before starting the process
    output$extraction_plot <- NULL
    
    country <- input$country
    country_data <- NA
    if (country == "Add a country") {
      # Handle file upload and new country name
      new_country_name <- input$new_country_name
      country_data <- rio::import(input$country_file$datapath) |>
        dplyr::mutate(eventual_name = tolower(gsub("[[:punct:]]+", "", eventual_name)))
      country <- new_country_name
    } else if (country == 'No Specified Country') {
      new_country_name <- input$new_country_name
      country <- new_country_name
      country_data <- 'unspecified'
    }
    
    withProgress(message = 'Extracting...', value = 0, {
      incProgress(0.3, detail = "Processing inputs...")
      Sys.sleep(1) # Simulate work
      
      # Run the extraction function
      incProgress(0.5, detail = "Extracting data...")
      # Call the function f_sort_smart with the selected parameters
      smartextract::f_sort_smart(
        smart_folder = paste(survey_path(), '/', sep=""), 
        output_folder = paste(output_path(), '/', sep=""), 
        country = country, 
        country_data = country_data
      )
      
      incProgress(1, detail = "Finalizing...")
      Sys.sleep(1)
    })
    
    
    # Handle issues
    handle_issues(
      smart_folder = paste(survey_path(), '/', sep=""),
      output_dir = paste(output_path(), '/', sep=""),
      country = country,
      session = session,
      country_data = country_data
    )
  
    # Render plot after extraction
    output$extraction_plot <- renderPlot({
      log_file <- rio::import(paste(paste(output_path(), '/', 'metadata.csv', sep="")))
      smartextract::f_plot_log_file(log_file)
    })
    
    # Indicate extraction button was pressed
    extract_pushed(TRUE)
  })
  
  ## CLEANING FOLDER ------------------------------------------------------
  shinyDirChoose(input, "cleaning_survey_path", roots = volumes)
  
  output$cleaning_survey_ui <- renderUI({
    if (extract_pushed()) {
      tagList(
        textInput("cleaning_survey_path_text", NULL, value = output_path()),
        shinyDirButton("cleaning_survey_path", "Please change the folder if needed", "Please select the folder containing SMART Surveys", 
                       FALSE, class = "cleaning-survey-button")
      )
    } else {
      tagList(
        shinyDirButton("cleaning_survey_path", "Select folder containing SMART extracted", 
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
    shinyjs::disable("cleaning_button")
    on.exit(shinyjs::enable("cleaning_button"))
    
    cleaning_survey_path <- if(extract_pushed()) {
      input$cleaning_survey_path_text
    } else {
      parseDirPath(roots = volumes, input$cleaning_survey_path)
    }
    
    withProgress(message = 'Cleaning...', value = 0, {
      incProgress(0.3, detail = "Checking inputs...")
      Sys.sleep(1) # Simulate work
      
      # Run the extraction function
      incProgress(0.5, detail = "Preprocessing data...")
      # Call the function f_cleaning with the selected parameters
      smartcleaning::f_cleaning(all_survey_folder = paste(cleaning_survey_path, '/', sep=""),
                                cleaning_criteria=input$type_cleaning)
      
      incProgress(1, detail = "Finalizing...")
      Sys.sleep(1)
    })
    
    
    log_file <- rio::import(paste(cleaning_survey_path, '/metadata.csv', sep=""))
    # After handling issues, generate a ggplot2 plot
    # Render combined explanatory text and image
    output$cleaning_content <- renderUI({
      tagList(
        tags$p("Now you can look into the different folders and check the different plausibility score outcomes."),
        tags$img(
          src = "plausibility_score_example.png", 
          alt = "Example of Plausibility Check", 
          style = "max-width: 100%; height: auto; margin-top: 10px;"
        )
      )
    })
    # Set the reactive value to TRUE indicating that extract button has been pushed
    cleaning_pushed(TRUE)
  })
  
  ### ------------------------------- COVERAGE TAB ----------------
  
  shinyDirChoose(input, "coverage_survey_path", roots = volumes)
  
  output$coverage_survey_ui <- renderUI({
    if (cleaning_pushed()) {
      tagList(
        textInput("coverage_survey_path_text", NULL, value = output_path()),
        shinyDirButton("coverage_survey_path", "Please change the folder if needed", "Please select the folder containing SMART Surveys", 
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
        textInput("anthro_survey_path_text", NULL, value = output_path()),
        shinyDirButton("anthro_survey_path", "Please change the folder if needed", "Please select the folder containing SMART Surveys", 
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
    # Define the size of the plot
    if(grepl(pattern = 'hist', x=input$plot_anthro)){
      v$height <- 300
      v$width <- 300
    }else{
      v$height <- 400
      v$width <- 1000
    }
    v$type_plot <- input$plot_anthro
    output$anthro_plot <- renderPlot({
      smartmetadata::f_plot_nutrition_tab(folder_name = v$anthro_survey_path, 
                                          type_plot = v$type_plot)
    # })
    }, height = function() { v$height }, width = function() { v$width })
  })
  
  
  
  
  ### ------ PART MORTALITY PLOT -------------------------------------------------
  shinyDirChoose(input, "mortality_survey_path", roots = volumes)
  
  output$mortality_survey_ui <- renderUI({
    if (cleaning_pushed()) {
      tagList(
        textInput("mortality_survey_path_text", NULL, value = output_path()),
        shinyDirButton("mortality_survey_path", "Please change the folder if needed", "Please select the folder containing SMART Surveys", 
                       FALSE, class = "mortality-survey-button")
      )
    } else {
      tagList(
        shinyDirButton("mortality_survey_path", "Select folder containing metadata from SMART", 
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
    v$height <- 400
    v$width <- 500
    v$type_plot <- input$plot_mortality
    output$mortality_plot <- renderPlot({
      smartmetadata::f_plot_morality_tab(folder_name = v$mortality_survey_path, 
                                         type_plot = v$type_plot)
    }, height = function() { v$height }, width = function() { v$width })
  })
  
  
  
  
  ### FUNCTION FOR THE EXTRACT PART GENERATE UIS ------------------------------------------
  
  # Function to handle issues such as "Unknown Date"
  handle_issues <- function(smart_folder, output_dir, country, session, country_data) {
    issue_folder <- file.path(output_dir, "smart_with_issue")
    log_file <- read.csv(paste(output_dir, 'metadata.csv', sep = ""))
    # List files in the issue folder
    files <- list.files(paste(output_dir, 'smart_with_issue', sep=""))
    files <- log_file[log_file$old_name %in% files & log_file$type_of_issue == 'Unknown Admin', 'old_name']
    nb_files <- length(files)
    
    if (country %in% c('SOM', 'KEN')) {
      admin_list <- smartextract::f_location_possibility(country)
      possible_localizations <- unique(tolower(sort(admin_list$right_name)))
    } else {
      if (!is.data.frame(country_data)) stop("Country data is invalid.")
      possible_localizations <- unique(tolower(sort(country_data$right_name)))
    }
    
    # Generate UI
    output$unknown_admin_ui <- renderUI({
      tagList(
        lapply(1:nb_files, function(ind) {
          file <- files[ind]
          tagList(
            p(paste('The SMART survey', file, 'has an issue.')),
            selectInput(paste("new_localization_", ind, sep = ""),
                        "Please select the right localization:",
                        choices = c("Not found", possible_localizations))
          )
        })
      )
    })
    
    # Show Modal
    showModal(modalDialog(
      title = "Issue: Unknown Admin",
      uiOutput("unknown_admin_ui"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_button", "OK")
      )
    ))
    
    # Handle OK button
    observeEvent(input$ok_button, {
      isolate({
        for (ind in 1:nb_files) {
          new_localization <- input[[paste("new_localization_", ind, sep = "")]]
          if (new_localization != "Not found") {
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
    })
  }
  
  }

## Run the application -------------------------------------------
shinyApp(ui = ui, server = server)
