#
# This is a Shiny web application designed to append barcode sequences to already mined oligopaints. 
# Two types of barcodes can be appended: OligoSTORM  and OligoFISSEQ.
# Author: Antonios Lioutas
# Date: 04 April 2020
#


##  necessary libraries
require(shiny)
require(utils)
#require(tidyverse)
require(dplyr)
require(tidyr)
require(purrr)
require(forcats)
require(stringr)
require(tibble)
require(data.table)
require(DT)
require(gsheet)
require(tippy)
require(shinyBS)
require(shinyjs)
require(shinythemes)
library(shinycssloaders) #####  %>% withSpinner(color="#0dc5c1")
require(shinybusy)
require(shinyhelper)
require(shinyWidgets)
require(feather)
# require(bedr)
# require(tidygenomics)
require(RBedtools) #install.packages("devtools") && devtools::install_github("vinay-swamy/RBedtools")
require(data.table)
require(bsplus)
require(bslib)
require(plotly)
#require(emo)


## App options
options(spinner.color="#000000", color.background = '#FFFFFF', spinner.type = 6)

## Source necessary scripts and functions
file.sources <- list.files("./utils/",
                           pattern="*.R$", full.names=TRUE,
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

# ## Source necessary scripts
#source("./utils/appending_utils.R")
# 
# ### Append MS and BS script
#source("./utils/append_MS_BS_function.R")

# define resetting the app
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


##////////////////////// *********************************** ********************** //////////////////////##
##////////////////////// ********************************       ********************** //////////////////////##
##////////////////////// ****************************              ********************** //////////////////////##
##////////////////////// ************************        APP UI     ************************* ////////////////////##
##////////////////////// ****************************              ********************** //////////////////////##
##////////////////////// ********************************       ********************** //////////////////////##
##////////////////////// *********************************** ********************** //////////////////////##


ui <- fluidPage(
  # google analytics
  tags$head(includeScript("google-analytics.html")),
  # tags$head(includeHTML(("google-analytics 2.html"))),
  # style CSS
  tags$style(HTML("hr {border-top: 1px solid #A4A4A4;}", 
                  #"footer{ position:absolute; bottom:0; width:100%; color: white; padding: 10px; background-color: black; z-index: 1000;}",
                  "footer{padding-left: 2%; width:150%; background: white;}", #f8f8f8
                  "body{font-size: 1.4em;}","h6{font-size: 0.6em;}"
  )), 
    tags$style(HTML("
      #h3_white {
        color: #ffffff;               /* White text */
        background-color: #000000;    /* Black background */
        padding: 10px;                   /* No padding */
      }
    ")),
  # favicon
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  # disable copy and cut and paste!
  #tags$body(oncopy="return false", oncut="return false"), #, onpaste="return false"),
  #, "h1{font-size: 3vw;}", "h2{font-size: 2vw;}", "h3{font-size: 2.5vw;}","h4{font-size: 2vw;}","h5{font-size: 1.5vw;}"" 
  # theme selection
  #theme = shinytheme("lumen"),
  #theme = shinytheme("cosmo"),
  useShinyjs(),
  # giving space to pop-up messages
  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
  # # Detect disconnection and reload the page
  # tags$script(HTML("
  #       $(document).on('shiny:disconnected', function(event) {
  #           alert('The application has disconnected. It will now reload.');
  #           setTimeout(function() {
  #               location.reload();
  #           }, 3000);  // Reload the page after a delay (3000 milliseconds here)
  #       });
  #   ")),
  # Application title
  # img(src='~/srv/shiny-server/www/oasis_logo.png', align = "left", height = "15%", width = "15%"),
  img(src="oasis_logo.png", align = "left", height = "180px", width = "180px", style = "margin-left: 15px; margin-right: 15px; margin-top: 15px; margin-bottom: 15px;"),
  titlePanel("Oligopaints Appending in 3 Simple Interactive Steps"),
  #h1("OASIS"),
  helpText("This is an application designed to interactively append barcode DNA sequences to Oligopaints.",
           "\n The barcodes that can be appended are: OligoSTORM and OligoFISSEQ.", #, and lambdaFISH
           h5("Contact: Antonios Lioutas","(antonios_lioutas <at> hms.harvard.edu)"), 
           div(
             style = "display: inline-block; margin-right: 20px;", # Styling for inline display and some space between links
             tags$a(href="https://www.transvection.org", target="_blank", "Ting Wu lab")
           ),
           div(
             style = "display: inline-block;", # Styling for inline display
             tags$a(href="https://youtu.be/-OBmgxL7BNo", target="_blank", "Video tutorial")
           )
           ),
  # h6(icon("fa fa-circle"), "To start from scratch, refresh the webpage."),
  # helpText("After any change press the corresponding button to update the results."),
  h6(icon("fa fa-circle"), "To start from scratch, refresh the webpage.", icon("fa fa-hand-point-right"), "After any modification press the corresponding button to apply the changes."),
  add_busy_bar(color = "#F1C232", height ="12px"),
  navbarPage(HTML(paste0('<span style="font-size: small;">v0.0.6</span>')),  #, tags$b("OASIS")
             
             
             ########################################################################################################
             # Panel SELECT FILES  
             ###############-----------------------------------/1/-------------------------- ########################
             
             
             tabPanel(" Upload", icon = icon("fa-solid fa-1"), fluid = TRUE,
                      # Sidebar with a menu to select what streets to append 
                      fluidRow(
                        column(3,
                               style="background-color: #f8f8f8;", 
                               
                               ################################### select Oligopaints source ###################################
                               radioButtons(
                                 inputId = "op_select",
                                 label = h4("Oligopaints source", style="font-weight: bold;"), 
                                 choices = c( "Use intersected Oligopaints" = "intersected",
                                              "Use genomic coordinates" = "new"),
                                 selected = "intersected")%>%
                                 helper(type = "markdown",
                                        title = "Select what to append on Oligopaints:",
                                        icon = "info-circle",
                                        colour = "#C0C0C0",
                                        # ATTN
                                        content = "upload_info",
                                        size = "m",
                                        buttonLabel = "Done"),
                               hr(),
                               ################################### Use genomic coordinates  ###################################
                               div(id = "op_select_div_new",
                                   # linker "ATATAC" or "TTTTT" 100nt backbone min length for 2 barcodes. 3 and 4 are optional.
                                   # checkboxInput('MIP', "Convert Oligopaints to MIPs", value = F),
                                   # checkboxInput('MIP_amp', "Add MIPs amplification sequences", value = F),
                                   # fileInput('MIP_ampF', "MIP forward amplification sequence", multiple = FALSE,
                                   #           accept = c(
                                   #             "text/csv",
                                   #             "text/comma-separated-values,text/plain",
                                   #             ".csv",
                                   #             ".bed")),
                                   # fileInput('MIP_ampR', "MIP reverse amplification sequence", multiple = FALSE,
                                   #           accept = c(
                                   #             "text/csv",
                                   #             "text/comma-separated-values,text/plain",
                                   #             ".csv",
                                   #             ".bed")),
                                   # textInput("MIP_seq", "MIP gap sequence", width = "100%"),
                                   # numericInput("MIP_left_padding", "MIP left padding", value = 0),
                                   # fileInput('MIP_bc1', "MIP barcode 1", multiple = FALSE,
                                   #           accept = c(
                                   #             "text/csv",
                                   #             "text/comma-separated-values,text/plain",
                                   #             ".csv",
                                   #             ".bed")),
                                   # fileInput('MIP_bc2', "MIP barcode 2", multiple = FALSE,
                                   #           accept = c(
                                   #             "text/csv",
                                   #             "text/comma-separated-values,text/plain",
                                   #             ".csv",
                                   #             ".bed")),
                                   # fileInput('MIP_bc3', "MIP barcode 3", multiple = FALSE,
                                   #           accept = c(
                                   #             "text/csv",
                                   #             "text/comma-separated-values,text/plain",
                                   #             ".csv",
                                   #             ".bed")),
                                   # fileInput('MIP_bc4', "MIP barcode 4", multiple = FALSE,
                                   #           accept = c(
                                   #             "text/csv",
                                   #             "text/comma-separated-values,text/plain",
                                   #             ".csv",
                                   #             ".bed")),
                                   
                                   # hr(),
                                   selectInput(inputId = "organism_oligopaints",
                                               list(icon("pastafarianism"), 
                                                    label = "Select genome"),
                                               # label = tippy("Select organism", "<strong>What organism you are planning to use the oligopaints for? Selecting the organism OASIS will make sure barcode sequences are orthogonal to the selected organism.</strong>",placement = "right"),
                                               # choices = setNames(list.files("./oligopaints_feather/") %>%
                                               #                      str_replace(., ".tsv", "") %>%
                                               #                      str_extract(.,"_.*_[:alnum:]*") %>%
                                               #                      str_replace(., "_(?=[[:alpha:]])","")
                                               #                    # %>%
                                               #                    #   str_replace(., "(?<=[[:digit:]])_", "")
                                               #                    , 
                                               #                    list.files("./oligopaints_feather/") %>%
                                               #                      str_replace(., "_(?=[[:alpha:]])"," (") %>%
                                               #                      str_replace(., "(?<=[[:digit:]])_", ") ") %>%
                                               #                      str_replace(., ".feather", "")),
                                               choices = default_oligopaints$Name,
                                               selected = "hg38 newBalance"
                                   )%>%
                                     helper(type = "markdown",
                                            title = "Select organism",
                                            icon = "info-circle",
                                            colour = "#C0C0C0",
                                            content = "select_organism_info",
                                            size = "m",
                                            buttonLabel = "Done"),
                                   checkboxInput("avoid_repeats", label = "Avoid Repeats", 
                                                 value = TRUE)%>%
                                     helper(type = "markdown",
                                            title = "Avoid Repeat overlapping Oligopaints",
                                            icon = "info-circle",
                                            colour = "#C0C0C0",
                                            content = "avoid_repeats_info",
                                            size = "m",
                                            buttonLabel = "Done"),
                                   sliderInput("off_target_score",label = "Off-Target Score (default:200)",
                                               min = 0, max = 10000, value = 200, step = 1)%>%
                                     helper(type = "markdown",
                                            title = "Off-target Score",
                                            icon = "info-circle",
                                            colour = "#C0C0C0",
                                            content = "off_target_info",
                                            size = "m",
                                            buttonLabel = "Done"),
                                   sliderInput("k_mer_count","Max K-mer count (default:5)", min = 0, max = 255, value = 5, step = 1)%>%
                                     helper(type = "markdown",
                                            title = "Max k-mer count",
                                            icon = "info-circle",
                                            colour = "#C0C0C0",
                                            content = "kmer_info",
                                            size = "m",
                                            buttonLabel = "Done"),
                                   hr(),
                                   h5("Choose .bed files containing your intersected annealing sequences"),
                                   checkboxGroupInput(inputId = "streets", label = "Select what to append on Oligopaints:", 
                                                      c( "Universal Mainstreet "="uni_ms", 
                                                         "Universal Backstreet "="uni_bs", 
                                                         "Mainstreet 1 " = "ms1", 
                                                         "Mainstreet 2 " = "ms2", 
                                                         "Backstreet 1 " = "bs1",
                                                         "Backstreet 2 " = "bs2"))%>%
                                     helper(type = "markdown",
                                            title = "Select what to append on Oligopaints:",
                                            icon = "info-circle",
                                            colour = "#C0C0C0",
                                            content = "upload_info",
                                            size = "m",
                                            buttonLabel = "Done"),
                                   # conditional file selection of universals
                                   
                                   #checkboxInput(inputId = "auto_uni", "Auto append the same universal Mainsteet and Backstreet sequence to all Oligopaints.", TRUE),
                                   #tippy_this("auto_uni", "<strong>When selected, OASIS will autotmatically append a unique pair of universal sequences for every chrosomosome found in your Main and/or Back Street .bed files. You do not need to upload a universal .bed file only Main and/or Back street files.)</strong>" ),
                                   conditionalPanel(
                                     condition = "input.streets.includes('uni_ms')", 
                                     fileInput(inputId = "UNI_ms", "Choose Mainstreet Universal Sequence", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed"))),
                                   conditionalPanel(
                                     condition = "input.streets.includes('uni_bs')",
                                     fileInput(inputId = "UNI_bs", "Choose Backstreet Universal Sequence", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed"))),
                                   
                                   # conditional file selection of MainStreets
                                   conditionalPanel(
                                     condition = "input.streets.includes('ms1')",
                                     fileInput(inputId = "MS1", "Choose Main Street 1", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed"))),
                                   conditionalPanel(
                                     condition = "input.streets.includes('ms2')",
                                     fileInput(inputId = "MS2", "Choose Main Street 2", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed"))),
                                   
                                   # conditional file selection of BackStreets
                                   conditionalPanel(
                                     condition = "input.streets.includes('bs1')",
                                     fileInput(inputId = "BS1", "Choose Back Street 1", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed")),
                                     checkboxInput(inputId = "same_BS1", "Backstreet1 same as Mainstreet 1", FALSE)),
                                   conditionalPanel(
                                     condition = "input.streets.includes('bs2')",
                                     fileInput(inputId = "BS2", "Choose Back Street 2", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed")),
                                     checkboxInput(inputId = "same_BS2", "Backstreet1 same as Mainstreet 2", FALSE)),

                                   
                                   
                                   

                                   hr(),
                                   actionButton("op_new", tippy("Upload", "<strong>This process might take several minutes. Before clicking make sure you have uploaded all necessary .bed files and you have properly selected the desired organism genome.</strong>"), 
                                                style="position:relative; left: 25%; right: 25%; color: #000; background-color: #F1C232; border-color: #000", 
                                                icon("arrow-circle-up"))
                               ),
                               
                               ################################### Use intersected Oligopaints  ###################################
                               
                               div(id = "op_select_div_intersected",
                                   h5("Choose .bed files containing your already intersected annealing sequences"),
                                   checkboxGroupInput(inputId = "intersected_streets", label = "Select what to append on Oligopaints:", 
                                                      c( "Universal Mainstreet "="intersected_uni_ms", 
                                                         "Universal Backstreet "="intersected_uni_bs", 
                                                         "Mainstreet 1" = "intersected_ms1", 
                                                         "Mainstreet 2" = "intersected_ms2", 
                                                         "Backstreet 1" = "intersected_bs1",
                                                         "Backstreet 2" = "intersected_bs2"))%>%
                                     helper(type = "markdown",
                                            title = "Select what to append on Oligopaints:",
                                            icon = "info-circle",
                                            colour = "#C0C0C0",
                                            content = "intersected_upload_info",
                                            size = "m",
                                            buttonLabel = "Done"),
                                   # conditional file selection of universals
                                   # conditionalPanel(
                                   #   condition = "input.intersected_streets.includes('intersected_uni_ms') || input.intersected_streets.includes('intersected_uni_bs')",
                                   #   hr(),
                                   # checkboxInput(inputId = "intersected_auto_uni", "Auto append the same universal Mainsteet and Backstreet sequence to all Oligopaints.", TRUE),
                                   #  tippy_this("intersected_auto_uni", "<strong>When selected, OASIS will autotmatically append a unique pair of universal sequences for every chrosomosome found in your Main and/or Back Street .bed files. You do not need to upload a universal .bed file only Main and/or Back street files.)</strong>" ),
                                   conditionalPanel(
                                     condition = "input.intersected_streets.includes('intersected_uni_ms')", 
                                     fileInput(inputId = "intersected_UNI_ms", "Choose Mainstreet Universal Sequence", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed"))),
                                   conditionalPanel(
                                     condition = "input.intersected_streets.includes('intersected_uni_bs')",
                                     fileInput(inputId = "intersected_UNI_bs", "Choose Backstreet Universal Sequence", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed"))
                                   ),
                                   #),
                                   
                                   # conditional file selection of MainStreets
                                   conditionalPanel(
                                     condition = "input.intersected_streets.includes('intersected_ms1')",
                                     hr(),
                                     fileInput(inputId = "intersected_MS1", "Choose Main Street 1", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed")
                                     )),
                                   conditionalPanel(
                                     condition = "input.intersected_streets.includes('intersected_ms2')",
                                     hr(),
                                     fileInput(inputId = "intersected_MS2", "Choose Main Street 2", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed")
                                     )),
                                   
                                   # conditional file selection of BackStreets
                                   conditionalPanel(
                                     condition = "input.intersected_streets.includes('intersected_bs1')",
                                     hr(),
                                     fileInput(inputId = "intersected_BS1", "Choose Back Street 1", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed")),
                                     checkboxInput(inputId = "intersected_same_BS1", "Backstreet1 same as Mainstreet 1", FALSE)
                                   ),
                                   conditionalPanel(
                                     condition = "input.intersected_streets.includes('intersected_bs2')",
                                     hr(),
                                     fileInput(inputId = "intersected_BS2", "Choose Back Street 2", multiple = FALSE,
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 ".bed")),
                                     checkboxInput(inputId = "intersected_same_BS2", "Backstreet2 same as Mainstreet 2", FALSE)
                                   ),

                                   hr(),
                                   helpText("After any change press the button."),
                                   actionButton("op_intersected", tippy("Upload", "<strong> Before clicking make sure you have uploaded all necessary .bed files and you have properly selected the desired organism genome.</strong>"), 
                                                style="position:relative; left: 25%; right: 25%; color: #000; background-color: #F1C232; border-color: #000", 
                                                icon("arrow-circle-up"))),
                               
                               
                               br(),
                               br(),
                               # ATTN (make sure that the density filtering and the coords file is properly filtered)
                               # Add content to the advanced field,
                               div(id = "advanced_filters",
                                   #h2("Advanced filtering options"),
                                   hr(),
                                   a(id = "toggleAdvanced", h5("Advanced filtering options", style="color:#f16232")), #, href = "#"
                                   shinyjs::hidden(
                                     div(id = "advanced",
                                         h5("For advanced users ONLY", style="color:white ; background-color:#f16232; font-weight: bold; text-align: center"),
                                         # fileInput(inputId = "coor_filter", "Coordinates Oligopaints-free", multiple = FALSE,
                                         #           accept = c(
                                         #             "text/csv",
                                         #             "text/comma-separated-values,text/plain",
                                         #             ".csv",
                                         #             ".bed"))%>%
                                         #   helper(type = "inline",
                                         #          title = "Select what to append on Oligopaints:",
                                         #          icon = "info-circle",
                                         #          colour = "#C0C0C0",
                                         #          # ATTN
                                         #          content = "Upload a .bed file ('chr', 'start', 'end') with the coordinates where you would not want any coverage by Oligopaints. This is useful when combining a new design with an existing one or if you would like to use Homologue Specific Oligopaints (HOPs). Another example is if you would like to expand an already existing Oligopaints library.",
                                         #          size = "m",
                                         #          buttonLabel = "Done"),
                                         # hr(),
                                         uiOutput("dynamic_street_select"),
                                         shinyjs::hidden(div(id = "dynamic_density_select",
                                         numericInput(inputId = "filter_density", "Oligopaints/Kb density threshold" ,value = NA, min = 0.1,step = 0.1)%>%
                                           helper(type = "inline",
                                                  title = "Select Oligopaints/Kb density threshold:",
                                                  icon = "info-circle",
                                                  colour = "#C0C0C0",
                                                  # ATTN
                                                  content = "Based on the chosen threshold OASIS will filter available Oligopaints and will aim for an even distribution across each genomic segment.",
                                                  size = "m",
                                                  buttonLabel = "Done"),
                                         # checkboxInput(inputId = "overrepresent", label = "Overrepresent Oligopaints for regions below desired density threshold.", value = F) %>% 
                                         #   helper(type = "inline",
                                         #          title = "Choose whether to overrepresent Oligopaints for regions below desired density threshold",
                                         #          icon = "info-circle",
                                         #          colour = "#C0C0C0",
                                         #          # ATTN
                                         #          content = "When the Oligopaints density (probes/Kb) is below the set thershold for a specific genomic region you can choose to randomly overrepresent some of the Oligopaints to reach the set density threshold.",
                                         #          size = "m",
                                         #          buttonLabel = "Done"),
                                         actionButton("filter_ops", "Filter",
                                                      style="position:relative; left: 25%; right: 25%; color: #000; background-color: #F1C232; border-color: #000", 
                                                      icon("filter")),
                                         br(),
                                         actionButton("clear_filters", tippy("Clear filters", "<strong> After clearing filter values press Filter again!</strong>"),
                                                      style="position:relative; left: 25%; right: 25%; color: #000; background-color: #f8f8f8; border-color: #f8f8f8", 
                                                      icon("trash"))%>%
                                           helper(type = "inline",
                                                  title = "Clearing any filters",
                                                  icon = "info-circle",
                                                  colour = "#C0C0C0",
                                                  # ATTN
                                                  content = "This resets values only. Press Filter again after clearing filters.",
                                                  size = "m",
                                                  buttonLabel = "Done"),
                                         ) # div close
                                         ) # shinyjs::hidden close
                                         
                                     ))),
                               
                               
                               
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                        ),
                        ###### RIGHT PANEL ######
                        
                        
                        column(9,
                               tags$p("1. ", icon("cloud-upload-alt"), strong("Upload"), "your genomic coordinates",strong("or"), "your intersected Oligopaint files."),     
                               img(src='cartoon_oligopaint_streets_OASIS.png', align = "center", width = "70%"),
                               ### EXTRA                          
                               # Show a plot of the generated distribution
                               # plotlyOutput('plot_chr'),
                               ### FIX                 -----> Remove this image ?
                               #img(src='cartoon_oligopaint_OASIS.png', align = "center", width = "80%"),
                               ################################################## START DELETE   ######################### 
                               # withSpinner(DT::dataTableOutput("test_table")) ,
                               br(),
                               conditionalPanel(
                                 condition = "input.op_intersected || input.op_new",
                               # Value box UI
                               layout_column_wrap(
                                 width = "250px",
                               value_box(
                                 title = "Total Oligopaints",
                                 value = textOutput("total_ops"),
                                 p("Mean Tm °C", textOutput("mean_Tm"), "Median Tm °C", textOutput("median_Tm")),
                                 # showcase = icon("dna")
                                 showcase = plotlyOutput('sparkline'),
                                 theme = value_box_theme(bg = "#f8f8f8"),
                                 full_screen = TRUE,
                               ),
                               value_box(
                                 title = "Mean Inter Oligopaints\n genomic distance",
                                 value = textOutput("mean_dist"), #textOutput("ops_dist"),
                                 p("Median distance", textOutput("median_dist")), #"Mean distance", textOutput("mean_dist"),
                                 # showcase = icon("dna")
                                 showcase = plotlyOutput('sparkline_dist'),
                                 theme = value_box_theme(bg = "#f8f8f8"),
                                 full_screen = TRUE,
                               ),
                               )
                               ),
                               br(),
                               br(),
                               # textOutput("test_text"),
                               ################################################## END DELETE   ######################### 
                               conditionalPanel(
                                 condition = "input.op_select == 'new'", #"input.collapseStreets.includes('new')",
                                 fluidRow(
                                   column(6, div(style = "Topleft"),
                                          conditionalPanel(
                                            condition = "input.streets.includes('uni_ms')", # && input.auto_uni==0
                                            h3(textOutput("uni_n_ms")),
                                            h4("Universal Mainstreet sequence summary"),
                                            DT::dataTableOutput("summary_UNI_ms"),
                                            h4("Universal Mainstreet sequences"),
                                            DT::dataTableOutput("table_UNI_ms"),
                                            br()),
                                          br()
                                   ),
                                   column(6,div(style = "Topright"),
                                          conditionalPanel(
                                            condition = "input.streets.includes('uni_bs') ", #&& input.auto_uni==0
                                            h3(textOutput("uni_n_bs")),
                                            h4("Universal Backstreet sequence summary"),
                                            DT::dataTableOutput("summary_UNI_bs"),
                                            h4("Universal Backstreet sequences"),
                                            DT::dataTableOutput("table_UNI_bs"),
                                            br()),
                                          br()
                                   )
                                 ), # fluidrow mainpanel
                                 fluidRow(
                                   column(6,div(style = "Topleft"),
                                          # conditionalPanel(
                                          #   condition = "input.streets.includes('ms')",
                                          #   h3(textOutput("ms_n")),
                                          #   h4("Mainstreet(s) summary"),
                                          #   DT::dataTableOutput("summary_MS"),
                                          #   h4("Mainstreet(s)"),
                                          #   DT::dataTableOutput("table_MS"),
                                          #   br()),
                                          conditionalPanel(
                                            condition = "input.streets.includes('ms1')",
                                            h3(textOutput("ms1_n")),
                                            h4("Mainstreet 1 summary"),
                                            DT::dataTableOutput("summary_MS1"),
                                            h4("Mainstreet 1"),
                                            DT::dataTableOutput("table_MS1"),
                                            br()),
                                          conditionalPanel(
                                            condition = "input.streets.includes('ms2')",
                                            h3(textOutput("ms2_n")),
                                            h4("Mainstreet 2 summary"),
                                            DT::dataTableOutput("summary_MS2"),
                                            h4("Mainstreet 2"),
                                            DT::dataTableOutput("table_MS2"),
                                            br()),
                                          br()
                                   ),
                                   column(6,div(style = "Topright"),
                                          # conditionalPanel(
                                          #   condition = "input.streets.includes('bs') && input.same_BS==0",
                                          #   h3(textOutput("bs_n")),
                                          #   h4("Backstreet(s) summary"),
                                          #   DT::dataTableOutput("summary_BS"),
                                          #   h4("Backstreet(s)"),
                                          #   DT::dataTableOutput("table_BS"),
                                          #   br()),
                                          conditionalPanel(
                                            condition = "input.streets.includes('bs1') && input.same_BS1==0",
                                            h3(textOutput("bs1_n")),
                                            h4("Backstreet 1 summary"),
                                            DT::dataTableOutput("summary_BS1"),
                                            h4("Backstreet 1"),
                                            DT::dataTableOutput("table_BS1"),
                                            br()),
                                          conditionalPanel(
                                            condition = "input.streets.includes('bs2') && input.same_BS2==0",
                                            h3(textOutput("bs2_n")),
                                            h4("Backstreet 2 summary"),
                                            DT::dataTableOutput("summary_BS2"),
                                            h4("Backstreet 2"),
                                            DT::dataTableOutput("table_BS2"),
                                            br()),
                                          br()
                                   )
                                 ) # fluidrow mainpanel
                               ), # conditional panel new
                               conditionalPanel(
                                 condition = "input.op_select == 'intersected'", #"input.collapseStreets.includes('intersected')",
                                 fluidRow(
                                   column(6,div(style = "Topleft"),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_uni_ms') ", #&& input.intersected_auto_uni==0
                                            h3(textOutput("intersected_uni_n_ms")),
                                            h4("Universal Mainstreet sequence summary"),
                                            DT::dataTableOutput("intersected_summary_UNI_ms"),
                                            h4("Universal Mainstreet sequences"),
                                            DT::dataTableOutput("intersected_table_UNI_ms"),
                                            br()),
                                          br()
                                   ),
                                   column(6,div(style = "Topright"),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_uni_bs') ", #&& input.intersected_auto_uni==0
                                            h3(textOutput("intersected_uni_n_bs")),
                                            h4("Universal Backstreet sequence summary"),
                                            DT::dataTableOutput("intersected_summary_UNI_bs"),
                                            h4("Universal Backstreet sequences"),
                                            DT::dataTableOutput("intersected_table_UNI_bs"),
                                            br()),
                                          br()
                                   )
                                 ), # fluidrow mainpanel
                                 fluidRow(
                                   column(6,div(style = "Topleft"),
                                          # conditionalPanel(
                                          #   condition = "input.intersected_streets.includes('intersected_ms')",
                                          #   h3(textOutput("intersected_ms_n")),
                                          #   h4("Mainstreet(s) summary"),
                                          #   DT::dataTableOutput("intersected_summary_MS"),
                                          #   h4("Mainstreet(s)"),
                                          #   DT::dataTableOutput("intersected_table_MS"),
                                          #   br()),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_ms1')",
                                            h3(textOutput("intersected_ms1_n")),
                                            h4("Mainstreet 1 summary"),
                                            DT::dataTableOutput("intersected_summary_MS1"),
                                            h4("Mainstreet 1"),
                                            DT::dataTableOutput("intersected_table_MS1"),
                                            br()),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_ms2')",
                                            h3(textOutput("intersected_ms2_n")),
                                            h4("Mainstreet 2 summary"),
                                            DT::dataTableOutput("intersected_summary_MS2"),
                                            h4("Mainstreet 2"),
                                            DT::dataTableOutput("intersected_table_MS2"),
                                            br()),
                                          br()
                                   ),
                                   column(6,div(style = "Topright"),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_bs') && input.intersected_same_BS==0",
                                            h3(textOutput("intersected_bs_n")),
                                            h4("Backstreet(s) summary"),
                                            DT::dataTableOutput("intersected_summary_BS"),
                                            h4("Backstreet(s)"),
                                            DT::dataTableOutput("intersected_table_BS"),
                                            br()),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_bs1') && input.intersected_same_BS1==0",
                                            h3(textOutput("intersected_bs1_n")),
                                            h4("Backstreet 1 summary"),
                                            DT::dataTableOutput("intersected_summary_BS1"),
                                            h4("Backstreet 1"),
                                            DT::dataTableOutput("intersected_table_BS1"),
                                            br()),
                                          conditionalPanel(
                                            condition = "input.intersected_streets.includes('intersected_bs2') && input.intersected_same_BS2==0",
                                            h3(textOutput("intersected_bs2_n")),
                                            h4("Backstreet 2 summary"),
                                            DT::dataTableOutput("intersected_summary_BS2"),
                                            h4("Backstreet 2"),
                                            DT::dataTableOutput("intersected_table_BS2"),
                                            br()),
                                          br()
                                   )
                                   # ,
                                   # column(12,div(style = "Bottom"),
                                   #        conditionalPanel(
                                   #          condition = "input.intersected_streets.includes('intersected_uni_ms') || input.intersected_streets.includes('intersected_ms') || input.intersected_streets.includes('intersected_uni_bs') || input.intersected_streets.includes('intersected_bs')",
                                   #          h3(textOutput("intersected_all_n")),
                                   #          h4("Whole library summary"),
                                   #          DT::dataTableOutput("intersected_summary_all"),
                                   #          h4("Whole library table"),
                                   #          DT::dataTableOutput("intersected_table_all"),
                                   #          br())
                                   # )
                                 ) # fluidrow mainpanel
                               ) # conditional panel intersected
                        )
                      ) 
             ),
             
             ########################################################################################################
             # Panel SELECT BARCODES to append  
             ###############-----------------------------------/2/-------------------------- ########################
             
             tabPanel(
               " Barcodes",
               icon = icon("fa-solid fa-2"),
               fluid = TRUE,
               fluidRow(
                 column(
                   3,
                   style="background-color: #f8f8f8;", 
                   selectInput(
                     inputId = "organism",
                     label = list(icon("pastafarianism"), 
                                  tippy(
                                    "Select organism",
                                    "<strong>What organism you are planning to use the Oligopaints for? Selecting the organism OASIS will make sure barcode sequences are orthogonal to the selected organism.</strong>",
                                    placement = "right"
                                  )),
                     choices = os_street_barcodes$Name,
                     #   c(
                     #   "Homo sapiens (Human)" = "human",
                     #   "Drosophila melanogaster (Fruit fly)" = "drosophila",
                     #   "Mus musculus (Mouse)" = "mouse"
                     # ),
                     selected = "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11"
                   ),
                   hr(),
                   h4("Select your barcodes."),
                   helpText(
                     "Please, select whether the barcodes you want to append to your Oligopaints are",
                     "OligoSTORM, for sequential imaging or OligoFISSEQ for multiplexing."
                   ),
                   hr(),
                   # DELETE LINE  uiOutput("avoid_until"),
                   uiOutput("dynamic_secs"),
                   # selectInput(
                   #   inputId = "secs",
                   #   label = tippy(
                   #     "Select OligoSTORM secondary sequences:",
                   #     "<strong>Here you can choose the secondary sequence you want to append to your bridges for OligoSTORM imaging. The order of the secondary sequences you choose here will be appended to the bridge sequences.</strong>"
                   #   ),
                   #   choices = c("sec1", "sec2", "sec3", "sec4", "sec5", "sec6"),
                   #   multiple = TRUE,
                   #   selectize = TRUE,
                   #   selected = c("sec1", "sec5", "sec6")
                   # ),
                   checkboxInput(
                     inputId = "sec405",
                     label = tippy(
                       "Alexa 405 Secondary Sequence (Activator)",
                       "<strong>When selected an extra sequence will be added where Alexa 405 containing secondary oligos will bind.</strong>"
                     ),
                     value = TRUE
                   ),
                   ##### Panels to append BC will only appear if tickboxes are selected
                   
                   
                   
                   
                   
                   
                   #Uni_MS
                   # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
                   # output$append_UNI_ms <- renderUI({
                   #   if(is.null(universals_ms_ready())){ #&& !(input$auto_uni)
                   #     print(code("Please, upload Universal Mainstreet file."))
                   #   } else if(!is.null(universals_ms_ready()))  { #&& input$auto_uni
                   #     fluidRow(column(12, hr(),
                   #                     radioButtons(inputId = "append_streets_uni_ms", label = "Choose the type of barcode you wish to append as Universal: ",
                   #                                  choices = list("Sequential OligoSTORM" = "toe_seq_im", "OligoSTORM"="seq_im"),
                   #                                  selected = "seq_im")),
                   #              tippy_this("append_streets_uni", paste0(strong("Sequential oligoSTORM"), " streets contain a toehold sequence that will allow you to displace streets, ", strong("OligoSTORM"), " streets do not contain toehold sequence, ", strong("oligoFISSEQ"), " streets contain randomly created sequences that will allow you to multiplex your imaging with sequencing reactions.") ),
                   #              checkboxInput(inputId = "uni_BS_rc", label = tippy("Reverse complement BackStreet universal barcode sequence: ", "<strong>When selected your Back Street sequence will be the reverse complement of the barcode. This will be taken into consideration when bridges are designed.</strong>" ), value = TRUE)
                   #              #column(12, DT::dataTableOutput('appended_uni')) #### REMOVE????
                   #     )
                   #   }
                   #
                   # })
                   
                   #Uni_MS
                   p(div(id = "barcodes_UNI_ms",
                              shinyjs::hidden(p(id = "barcode_error_uni_ms",
                                                print(
                                                  code("Please, upload Universal Mainstreet bed file.")
                                                ))),
                             p(
                                div(id = "barcodes_UNI_ms_options",
                                    fluidRow(column(
                                      12,
                                      hr(),
                                      radioButtons(
                                        inputId = "append_streets_uni_ms",
                                        label = "Choose the type of barcode you wish to append as Mainstreet Universal: ",
                                        choices = list("Sequential OligoSTORM" = "toe_seq_im", "OligoSTORM" =
                                                         "seq_im"),
                                        selected = character(0)
                                      )
                                    )))
                              ))),
                   #Uni_BS
                   p(div(id = "barcodes_UNI_bs",
                              shinyjs::hidden(p(id = "barcode_error_uni_bs",
                                                print(
                                                  code("Please, upload Universal Backstreet bed file.")
                                                ))),
                              p(
                                div(id = "barcodes_UNI_bs_options",
                                    fluidRow(
                                      column(
                                        12,
                                        hr(),
                                        radioButtons(
                                          inputId = "append_streets_uni_bs",
                                          label = "Choose the type of barcode you wish to append as Backstreet Universal: ",
                                          choices = list("Sequential OligoSTORM" = "toe_seq_im", "OligoSTORM" =
                                                           "seq_im"),
                                          selected = character(0)
                                        )
                                        ,
                                        checkboxInput(
                                          inputId = "uni_BS_rc",
                                          label = tippy(
                                            "Reverse complement BackStreet universal barcode sequence. ",
                                            "<strong>When selected your Back Street sequence will be the reverse complement of the barcode. This will be taken into consideration when bridges are designed.</strong>"
                                          ),
                                          value = TRUE
                                        )
                                      )
                                    ))
                              ))),
                   
                   #MS1
                   p(div(id = "barcodes_ms1",
                              shinyjs::hidden(p(id = "barcode_error_ms1",
                                                print(
                                                  code("Please, upload Mainstreet 1 file.")
                                                ))),
                              p(
                                div(id = "barcodes_ms1_options",
                                    fluidRow(column(
                                      12,
                                      hr(),
                                      radioButtons(
                                        inputId = "append_streets_ms1",
                                        label = "Choose the type of barcode you wish to append as Mainstreet 1: ",
                                        choices = choices_append,
                                        selected = character(0)
                                      )
                                    )))
                              ))),
                   #MS2
                   p(div(id = "barcodes_ms2",
                              shinyjs::hidden(p(id = "barcode_error_ms2",
                                                print(
                                                  code("Please, upload Mainstreet 2 file.")
                                                ))),
                              p(
                                div(id = "barcodes_ms2_options",
                                    fluidRow(column(
                                      12,
                                      hr(),
                                      radioButtons(
                                        inputId = "append_streets_ms2",
                                        label = "Choose the type of barcode you wish to append as Mainstreet 2: ",
                                        choices =  choices_append,
                                        selected = character(0)
                                      )
                                    )))
                              ))),
                   
                   #BS1
                   p(div(id = "barcodes_bs1",
                              shinyjs::hidden(p(id = "barcode_error_bs1",
                                                print(
                                                  code("Please, upload Backstreet 1 file.")
                                                ))),
                              p(
                                div(id = "barcodes_bs1_options",
                                    fluidRow(
                                      column(
                                        12,
                                        hr(),
                                        radioButtons(
                                          inputId = "append_streets_bs1",
                                          label = "Choose the type of barcode you wish to append as Backstreet 1: ",
                                          choices = choices_append,
                                          selected = character(0)
                                        ),
                                        checkboxInput(
                                          inputId = "bs1_rc",
                                          label = "Reverse complement BackStreet 1 OligoSTORM barcode sequence. "
                                          ,
                                          value = TRUE
                                        )
                                      )
                                    ))
                              ))),
                   #BS2
                   p(div(id = "barcodes_bs2",
                              shinyjs::hidden(p(id = "barcode_error_bs2",
                                                print(
                                                  code("Please, upload Backstreet 2 file.")
                                                ))),
                              p(
                                div(id = "barcodes_bs2_options",
                                    fluidRow(
                                      column(
                                        12,
                                        hr(),
                                        radioButtons(
                                          inputId = "append_streets_bs2",
                                          label = "Choose the type of barcode you wish to append as Backstreet 2: ",
                                          choices =  choices_append,
                                          selected = character(0)
                                        ),
                                        checkboxInput(
                                          inputId = "bs2_rc",
                                          label = "Reverse complement BackStreet 1 OligoSTORM barcode sequence. "
                                          ,
                                          value = TRUE
                                        )
                                      )
                                    ))
                              ))), 
                   
                   # Action button that will append selected barcodes
                   actionButton(
                     "append",
                     tippy(
                       "Append Barcodes",
                       "<strong>Before you press this button make sure you have uploaded all necessary .bed files and you have properly selected the specific barcode scheme you want to append to your Oligopaints.</strong>"
                     ),
                     style = "position:center; left: 25%; right: 25%; color: #000; background-color: #F1C232; border-color: #000",
                     icon("chevron-circle-right")
                   ),
                   
                   #helpText("Click the button to append barcodes to the uploaded Oligopaints."),
                   br(),
                   br(),
                   
                   hr(),
                   # Advanced filtering of barcodes
                   div(id = "advanced_append",
                       #h2("Advanced filtering options"),
                       a(id = "toggleAdvancedAppend", h5("Advanced barcoding options", style="color:#f16232")), #, href = "#"
                       shinyjs::hidden(
                         div(id = "advancedAppend",
                             h5("For advanced users ONLY", style="color:white ; background-color:#f16232; font-weight: bold; text-align: center"),
                             h3("OligoSTORM", id = "h3_white"),
                             h5(strong("OligoSTORM barcodes to avoid:")),
                             splitLayout(
                               numericInput("avoid_from", label = "From:", value = 0, step = 1),
                               numericInput("avoid_to", label = "To:", value = 0, step = 1)),
                             fileInput(inputId = "avoid_os_seq", "OligoSTORM barcodes to AVOID", multiple = F,
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",
                                         ".bed")),
                             fileInput(inputId = "custom_os_seq", "Custom OligoSTORM barcodes", multiple = F,
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",
                                         ".bed")),
                             actionButton("reset_sec", "Default Secondary values"),
                             DTOutput("sec_seq_table"),
                             br(),
                             actionButton("reset_actsec", "Default Activator values"),
                             DTOutput("actsec_seq_table"),
                             br(),
                             
                             # fileInput(inputId = "custom_sec", "Custom OligoSTORM secondary secuences", multiple = F,
                             #           accept = c(
                             #             "text/csv",
                             #             "text/comma-separated-values,text/plain",
                             #             ".csv",
                             #             ".bed")),
                             br(),
                             h3("OligoFISSEQ", id = "h3_white"),
                             # fileInput(inputId = "avoid_ofq_seq", "OligoFISSEQ barcodes to AVOID", multiple = F,
                             #           accept = c(
                             #             "text/csv",
                             #             "text/comma-separated-values,text/plain",
                             #             ".csv",
                             #             ".bed")),
                             h5("OligoFISSEQ bits"),
                             actionButton("reset_ofq", "Default OligoFISSEQ values"),
                             DTOutput("ofq_seq_table"),
                             # textInput(inputId = "custom_ofq_seq", "Custom OligoFISSEQ bits ", #multiple = F,
                             #           value = paste(c("GGTCT", "TGGTC", "AGTCA", "CGCTC"), collapse = ", ")),
                             br(),
                             # h3("lambdaFISH", id = "h3_white"),
                             # h5("lambdaFISH bits"),
                             # actionButton("reset_lambda", "Default lambdaFISH values"),
                             # DTOutput("lambda_seq_table"),
                             # br(),
                             # 
                             # actionButton("filter_barcodes", "Filter Barcodes",
                             #              style="position:center; left: 25%; right: 25%; color: #000; background-color: #F1C232; border-color: #000",
                             #              icon("filter")),
                             # br(),
                             # actionButton("clear_barcode_filters", "Clear barcode filters",
                             #              style="position:center; left: 25%; right: 25%; color: #000; background-color: #f8f8f8; border-color: #f8f8f8",
                             #              icon("trash"))
                             
                         ))),
                   
                   
                   
                   
                   ############### START DELETE               
                   
                   #                                checkboxInput("advanced_append", "Advanced appending settings", FALSE),
                   #                                conditionalPanel(
                   #                                  condition = "input.advanced_append == 1",
                   #                                  # Add content to the advanced field,
                   #                                 textInput(inputId = "avoid_os_seq", "OligoSTORM barcodes you want to avoid (separated by commas)", placeholder = "GCCTATAGCGCACTGGTGCC, GTAAGCGCCATCGCGAGTGG, GGGTGCGGTCGACTAGCTTC, GCTGTGGCCAGTGCAGGTTC,
                   # GCCTCGACGTTTCGACTGCG, GCGTTGCGTTCTCCAGACGG"),
                   #                                 textInput(inputId = "avoid_ofq_seq", "OligoFISSEQ barcodes you want to avoid (separated by commas)", placeholder = "GCCTATAGCGCACTGGTGCC, GTAAGCGCCATCGCGAGTGG, GGGTGCGGTCGACTAGCTTC, GCTGTGGCCAGTGCAGGTTC,
                   # GCCTCGACGTTTCGACTGCG, GCGTTGCGTTCTCCAGACGG") 
                   #                                  
                   #                                ),
                   ############### END DELETE   
                   
                   
                   
                   
                   
                   
                   br(),
                   br(),
                   # br(),
                   # br(),
                   # br(),
                   # br(),
                   # br(),
                   # br(),
                   # br(),
                   # br()
                 ),# closing left panel
                 ###### RIGHT PANEL ######
                 column(9,
                        "2. ",icon("dna"), "Select the type of", strong("barcodes"), "you want to append.",
                        img(src='cartoon_oligopaint_OASIS.png', align = "center", width = "80%"),
                        ################################################## START DELETE   ######################### 
                        # DT::dataTableOutput("test_table2"),
                        # br(),
                        # br(),
                        # br(),
                        # "Text:",
                        # textOutput("test_text2"),
                        ################################################## END DELETE   ######################### 
                        conditionalPanel(
                          condition = "input.streets.includes(uni_ms)", #BUG if the quote is closed nothig appears in the second tab
                          appened_opsUI(id = 'uni_ms')
                        ),
                        conditionalPanel(
                          condition = "input.streets.includes(uni_bs)",
                          appened_opsUI(id = 'uni_bs')
                        ),
                        conditionalPanel(
                          condition = "input.streets.includes(ms1)",
                          appened_opsUI(id = 'ms1')
                        ),
                        conditionalPanel(
                          condition = "input.streets.includes(ms2)",
                          appened_opsUI(id = 'ms2')
                        ),
                        conditionalPanel(
                          condition = "input.streets.includes(bs1)",
                          appened_opsUI(id = 'bs1')
                        ),
                        conditionalPanel(
                          condition = "input.streets.includes(bs2)",
                          appened_opsUI(id = 'bs2')
                        )
                         
                 ) # closing right panel
               ) # closing fluidrow 
             ), # closing tabpanel
             
             
             
             ########################################################################################################
             #Panel SELECT what to download   
             ###############-----------------------------------/3/-------------------------- ########################
             
             
             
             tabPanel(" Download", icon = icon("fa-solid fa-3"), fluid = TRUE,
                      fluidRow(
                        column(3,
                               style="background-color: #f8f8f8;", 
                               br(),
                               tags$div("3. ",icon("download"), strong("Download"), "the appended Oligopaint files.",
                                        br(),
                                        br(),
                                        strong("Happy FISHing!")),
                               br(),
                               pickerInput(
                                 inputId = "download_files",
                                 label = strong("Files to download: "), 
                                 choices = c("OASIS Intersected Oligopaints before appending" = "ops_only",
                                             "OASIS Appended Library Report" = "report",
                                             "Oligopaints order file"="oligopaints_order", 
                                             "Bridges and Toes" = "bridges_download", 
                                             "Amplification Primers" = "amplification_download" ),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE
                               ),
                               # checkboxGroupInput(inputId = "download_files", label = strong("Files to download: "),
                               #                         c("All files" = "all",
                               #                           "OASIS Appended Library Report" = "report",
                               #                           "Oligopaints order file"="Oligopaints_order", 
                               #                           "Bridges" = "bridges_download", 
                               #                           "Toes" = "toes_download", 
                               #                           "Amplification Primers" = "amplification_download" )),
                               downloadButton("download_zip_files", 
                                              tippy("Download", "<strong>Before you press this button make sure you have uploaded all necessary .bed files and you have properly selected the specific barcode scheme you want to append to your Oligopaints.</strong>"), 
                                              style="position:relative; color: #000; background-color: #F1C232; border-color: #000", icon("download")),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               br()
                        ),
                        ###### RIGHT PANEL ######
                        column(9, #h4("OASIS Appended Library Report"),
                               htmlOutput("OASISReport")%>% withSpinner()#,
                               # htmlOutput("reportOutput")
                               
                        )
                        
                        
                      ),                                
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br()
             ),
             # navbarMenu(title = "", icon = icon("power-off"),
             #   tabPanel(actionLink("stop", " STOP",icon = icon("stop")))#, 
             #   #tabPanel(actionLink("refresh", " Refresh",icon = icon("redo"))),
             #   #tabPanel(actionLink("newSession"," New session",icon = icon("plus-square")))
             # ),
             ###### RIGHT PANEL ######
             
             tags$footer(
               br(),
               hr(),
               
               
               
            a(id = "toggleScratch", list(icon('arrow-down-wide-short'), label = "Additional Oligopaints resources"), style="color:#000000; font-size: 16px;"),
            br(),
            shinyjs::hidden(
              div(id = "scratch",
               tags$a(href="https://revcomp.streamlit.app/", "Reverse Complement online tool (DNA / RNA)"),
               h5("Mine Oligopaints from scratch:"),
               tags$a(href="https://github.com/beliveau-lab/OligoMiner", "OligoMiner."),
               br(),
               tags$a(href="https://paintshop.io/", "PaintSHOP."),
               h5("OligoSTORM sequential imaging is described here:"),
               tags$a(href="https://doi.org/10.1371/journal.pgen.1007872", "Nir G, Farabella I, Pérez Estrada C, Ebeling CG, et. al., PLOS Genetics (2018)."),
               br(),
               tags$a(href="https://doi.org/10.1126/science.aau1783", "Bintu B, et. al., Science (2018)."),
               h5("OligoFISSEQ imaging is described here:"),
               tags$a(href="https://rdcu.be/c5yF9", "Nguyen H, Chattoraj S, Castillo D, et. al., Nature Methods (2020)."),
               br(),
               hr(),
               h5("For ever grateful to Huy Nguyen, Jumana Alhaj Abed and the Wu lab for their feedback on OASIS development."),
               h5("Logo designed by Hazam Jara.")
               ) # close div 
            ), # close hidden
               br(),
               img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/0/07/Harvard_Medical_School_seal.svg/1280px-Harvard_Medical_School_seal.svg.png", align = "left", width = "200px", height = "50px"),
               br(),
               br(),
               br(),
               br(),
               # h6("Disclaimer: All OASIS code was written by Antonios Lioutas. Code formatting and comments were partially done with the help of ChatGPT. Any AI output was human verified.")
            #   img(src = "HMDC_logo.png", align = "right", width = 320, height = 100)
            ) #close footer
  )
)







##////////////////////// *********************************** ********************** //////////////////////##
##////////////////////// ********************************       ********************** //////////////////////##
##////////////////////// ****************************              ********************** //////////////////////##
##////////////////////// ************************       SERVER     ************************* ////////////////////##
##////////////////////// ****************************              ********************** //////////////////////##
##////////////////////// ********************************       ********************** //////////////////////##
##////////////////////// *********************************** ********************** //////////////////////##

server <- function(input, output, session) {
  
  # ShinyHelper server function
  observe_helpers(withMathJax = TRUE, help_dir = "help_files")
  
  # how big can my input files be? 1GB
  options(shiny.maxRequestSize=1000*1024^2) 
  
  
  # # Menu Stop, Refresh and new session buttons
  # 
  # observeEvent(input$stop, {
  #   stopApp(returnValue = invisible())
  # })
  
  ########################################################################################################
  # Panel SELECT FILES SERVER
  ###############-----------------------------------/1/-------------------------- ######################## 
  
  # Create a reactive values object that will hold all uploaded and created df (reseting those is easier)
  
  # rv <- reactiveValues(intersected_uni_ms = NULL,
  #                      intersected_uni_bs = NULL,
  #                      intersected_ms1 = NULL,
  #                      intersected_ms2 = NULL,
  #                      intersected_bs1 = NULL,
  #                      intersected_bs2 = NULL,
  #                      uni_ms = NULL,
  #                      uni_bs = NULL,
  #                      ms1 = NULL,
  #                      ms2 = NULL,
  #                      bs1 = NULL,
  #                      bs2 = NULL)
  
  # filter Oligopaints based on user input as to repeats, off-target score, max k-mer
  oligopaints_data <- reactive({
    temp <- tempfile()
    download.file(default_oligopaints[default_oligopaints$Name == input$organism_oligopaints, "link"], temp, mode = "wb")
    unzip(temp, exdir = tempdir())
    file <- list.files(tempdir(), pattern = "\\.tsv$", full.names = TRUE)
    
    DT <- fread(file)
    setnames(DT, old = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"), 
             new = c("chr", "start", "end", "sequence", "Tm", "on_target", "off_target", "is_repeat", "prob", "max_kmer", "probe_strand"))
    
    DT <- DT[off_target <= input$off_target_score & 
         max_kmer <= input$k_mer_count & 
         is_repeat != as.numeric(input$avoid_repeats)]
    
  }) %>% bindCache(input$organism_oligopaints, input$off_target_score, input$k_mer_count, input$avoid_repeats)
  
  # Use bindEvent to trigger the reactive expression
  filtered_oligopaints <- eventReactive(input$op_new, {
      oligopaints_data()
  })
  
  
  # toggle advanced option to homogenize densities of OPs
  observe({
    toggle(id = "dynamic_density_select", condition = !is.null(input$selected_street), anim = T, time = 0.3)
  })
  
  
  
  # toggle advanced filtering settings
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE, time = 0.3))  
  
  # toggle advanced filtering settings
  shinyjs::onclick("toggleScratch",
                   shinyjs::toggle(id = "scratch", anim = TRUE, time = 0.3))  
  
  
  # resets all fields in a div
  observeEvent(input$clear_filters, {
    shinyjs::reset("advanced_filters") # this is the div names
  })
  
  # 
  observe({
    toggle(id = "op_select_div_new", condition = input$op_select == "new", anim = T, time = 0.3)
  })
  
  observe({
    toggle(id = "op_select_div_intersected", condition = input$op_select == "intersected", anim = T, time = 0.3)
  })
  
  # When the user toggles between new and intersected there is a reset of the input values
  observeEvent(input$op_select, {
    if(input$op_select == "new"){
      #comb_ops() <- tibble(NULL)
      shinyjs::reset("op_select_div_intersected")
      # intersected_universals_ms() <- tibble()
      reset("intersected_uni_n_ms")
      reset("intersected_summary_UNI_ms")
      reset("intersected_table_UNI_ms")
      reset("intersected_uni_n_bs")
      reset("intersected_summary_UNI_bs")
      reset("intersected_table_UNI_bs")
      reset("intersected_ms_n")
      reset("intersected_summary_MS")
      reset("intersected_table_MS")
      reset("intersected_bs_n")
      reset("intersected_summary_BS")
      reset("intersected_table_BS")
      reset("intersected_UNI_ms")
    } else if (input$op_select == "intersected") {
      shinyjs::reset("op_select_div_new")
      reset("uni_n_ms")
      reset("summary_UNI_ms")
      reset("table_UNI_ms")
      reset("uni_n_bs")
      reset("summary_UNI_bs")
      reset("table_UNI_bs")
      reset("ms_n")
      reset("summary_MS")
      reset("table_MS")
      reset("bs_n")
      reset("summary_BS")
      reset("table_BS")
    }
  })
  
  ### Check for wrong coordinates in the bed file and alert that intersecting from scratch will take a long time
  
  
  
  
  # Notification that the intersection of new Oligopaints might take some time to run
  # FIX the pop up appears after computation...
  # observeEvent(input$op_new, {
  #     showNotification(paste(emo::ji("timer"), "This process might take several minutes... do not refresh the page if you do not want to start from scratch...", emo::ji("coffee")))
  # 
  # })
  
  
  # Show a notification when there are overlapping coordinates. The user needs to select whether they want the coordinates corrected or not...
  observeEvent(c(input$UNI_ms,
                 input$intersected_UNI_ms,
                 input$UNI_bs,
                 input$intersected_UNI_bs,
                 input$MS1,
                 input$intersected_MS1,
                 input$MS2,
                 input$intersected_MS2,
                 input$BS1,
                 input$intersected_BS1,
                 input$BS2,
                 input$intersected_BS2
  ), {
    # df_ms <- tibble(fread(input$UNI_ms$datapath[1]) %>%
    #                   select(1:4))
    
    
    bed_list <- NULL
    bed_list <- list(uni_ms = if(!is.null(input$UNI_ms)) tibble(fread(input$UNI_ms$datapath[1]) %>% select(1:3)) else NULL,
                     uni_i_ms = if(!is.null(input$intersected_UNI_ms)) tibble(fread(input$intersected_UNI_ms$datapath[1]) %>% select(5:7)) else NULL,
                     uni_bs = if(!is.null(input$UNI_bs)) tibble(fread(input$UNI_bs$datapath[1]) %>% select(1:3)) else NULL,
                     uni_i_bs = if(!is.null(input$intersected_UNI_bs)) tibble(fread(input$intersected_UNI_bs$datapath[1]) %>% select(5:7)) else NULL,
                     ms1 = if(!is.null(input$MS1)) tibble(fread(input$MS1$datapath[1]) %>% select(1:3)) else NULL,
                     ms_i1 = if(!is.null(input$intersected_MS1)) tibble(fread(input$intersected_MS1$datapath[1]) %>% select(5:7)) else NULL,
                     ms2 = if(!is.null(input$MS2)) tibble(fread(input$MS2$datapath[1]) %>% select(1:3)) else NULL,
                     ms_i2 = if(!is.null(input$intersected_MS2)) tibble(fread(input$intersected_MS2$datapath[1]) %>% select(5:7)) else NULL,
                     bs1 = if(!is.null(input$BS1)) tibble(fread(input$BS1$datapath[1]) %>% select(1:3)) else NULL,
                     bs_i1 = if(!is.null(input$intersected_BS1)) tibble(fread(input$intersected_BS1$datapath[1]) %>% select(5:7)) else NULL,
                     bs2 = if(!is.null(input$BS2)) tibble(fread(input$BS2$datapath[1]) %>% select(1:3)) else NULL,
                     bs_i2 = if(!is.null(input$intersected_BS2)) tibble(fread(input$intersected_BS2$datapath[1]) %>% select(5:7)) else NULL
    ) %>%
      compact() # removes NULL elements in the list 
    
    list_merged <- NULL
    list_merged <- map_lgl(bed_list, bedtools_merge_test)
    #names(list_merged[list_merged == F])
    
    if(any(list_merged) == FALSE){
      showModal(modalDialog(
        title = "Important message",
        "Some of the coordinates you have uploaded are overlapping by at least 1bp.",
        easyClose = F,
        footer = tagList(
          radioButtons("correct_overlap","How to proceed with overlapping coordinates?", 
                       c("Remove them" = TRUE, "Do not remove them" = FALSE), 
                       selected = TRUE,
                       inline = T),
          modalButton("Submit")
          #actionButton("ok", "OK")
        )
      ))
      
     
      
      # showNotification("Attention! Universal Main Street coordinates have overlaps. Correct them and upload the correct coordinates.",
      #                  type = "error",
      #                  duration = NULL)
    }
    
  })
  
  # Check whether the user has not uploaded all the necessary bed files. If so, show a notification.
  observeEvent(c(input$new_op, input$append), {
    
    # crosscheck the streets and intersected_streets selected and the bed files uploaded 
    # if the user has selected a street without uploading a file, then the file is NULL
    # if there is any null then showModal notification to correct for this discrepancy
    
    # create a list with the uploaded bed files
    bed_list <- NULL
    bed_list <- list(uni_ms = if(!is.null(input$UNI_ms) && "uni_ms" %in% input$streets) 1 else 0,
                     uni_bs = if(!is.null(input$UNI_bs) && "uni_bs" %in% input$streets) 1 else 0,
                     ms1 = if(!is.null(input$MS1) && "ms1" %in% input$streets) 1 else 0,
                     ms2 = if(!is.null(input$MS2) && "ms2" %in% input$streets)  1 else 0,
                     bs1 = if(!is.null(input$BS1) && "bs1" %in% input$streets)  1 else 0,
                     bs2 = if(!is.null(input$BS2) && "bs2" %in% input$streets)  1 else 0
    ) %>% unlist()
    
    if(length(input$streets) != sum(bed_list)){
      showModal(modalDialog(
        title = "Missing files!",
        "You have not uploaded all the necessary bed files for the selected streets.",
        easyClose = F
        )
      )
    } 

  })
  
  # Check whether the user has not uploaded all the necessary bed files. If so, show a notification.
  observeEvent(c(input$op_intersected, input$append), {
    
    # crosscheck the streets and intersected_streets selected and the bed files uploaded 
    # if the user has selected a street without uploading a file, then the file is NULL
    # if there is any null then showModal notification to correct for this discrepancy
    
    # create a list with the uploaded bed files
    bed_list <- list(uni_i_ms = if(!is.null(input$intersected_UNI_ms) && "intersected_uni_ms" %in% input$intersected_streets) 1 else 0,
                     uni_i_bs = if(!is.null(input$intersected_UNI_bs) && "intersected_uni_bs" %in% input$intersected_streets) 1 else 0,
                     ms_i1 = if(!is.null(input$intersected_MS1) && "intersected_ms1" %in% input$intersected_streets) 1 else 0,
                     ms_i2 = if(!is.null(input$intersected_MS2) && "intersected_ms2" %in% input$intersected_streets) 1 else 0,
                     bs_i1 = if(!is.null(input$intersected_BS1) && "intersected_bs1" %in% input$intersected_streets) 1 else 0,
                     bs_i2 = if(!is.null(input$intersected_BS2) && "intersected_bs2" %in% input$intersected_streets)  1 else 0
    ) %>% unlist()
    
    if(length(input$intersected_streets) != sum(bed_list)){
      showModal(modalDialog(
        title = "Missing files!",
        "You have not uploaded all the necessary bed files for the selected streets.",
        easyClose = F
      )
      )
    }
    
  })

  
  
  
  
  ######################## BEDFILES to OPs  ######################## 
  
  
  # create a common dataframe that holds either the already intersected or the newly intersected OPs
  comb_ops <- reactive({
    # Create list with uploaded bed files
    
    bed_list <- NULL
    
    # get all available bedfiles
    if(input$op_select == "new"){
      bed_list <- list(uni_ms = if(!is.null(input$UNI_ms)) tibble(fread(input$UNI_ms$datapath[1]) %>% select(1:4)%>% 
                                                                    `colnames<-`(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms"))) %>% 
                                                                      arrange(chr_uni_ms, start_uni_ms) else NULL,
                       uni_bs = if(!is.null(input$UNI_bs)) tibble(fread(input$UNI_bs$datapath[1]) %>% select(1:4)%>% 
                                                                    `colnames<-`(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs"))) %>% 
                                                                      arrange(chr_uni_bs, start_uni_bs) else NULL,
                       ms1    = if(!is.null(input$MS1)) tibble(fread(input$MS1$datapath[1]) %>% select(1:4)%>% 
                                                                 `colnames<-`(c("chr_ms1","start_ms1", "end_ms1", "id_ms1"))) %>% 
                                                                  arrange(chr_ms1, start_ms1) else NULL,
                       ms2    = if(!is.null(input$MS2)) tibble(fread(input$MS2$datapath[1]) %>% select(1:4)%>% 
                                                                 `colnames<-`(c("chr_ms2","start_ms2", "end_ms2", "id_ms2"))) %>% 
                                                                  arrange(chr_ms2, start_ms2) else NULL,
                       bs1    = if(!is.null(input$BS1)) tibble(fread(input$BS1$datapath[1]) %>% select(1:4)%>% 
                                                                 `colnames<-`(c("chr_bs1","start_bs1", "end_bs1", "id_bs1"))) %>% 
                                                                  arrange(chr_bs1, start_bs1) else NULL,
                       bs2    = if(!is.null(input$BS2)) tibble(fread(input$BS2$datapath[1]) %>% select(1:4)%>% 
                                                                 `colnames<-`(c("chr_bs2","start_bs2", "end_bs2", "id_bs2"))) %>% 
                                                                  arrange(chr_bs2, start_bs2) else NULL
      ) %>%
        compact() # removes NULL elements in the list
    } else if(input$op_select == "intersected"){
      bed_list <- list(uni_ms = if(!is.null(input$intersected_UNI_ms)) tibble(fread(input$intersected_UNI_ms$datapath[1]) %>% select(1:9)%>% 
                                                                                `colnames<-`(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms", "chr", "start", "end", "sequence", "Tm"))),
                       uni_bs = if(!is.null(input$intersected_UNI_bs)) tibble(fread(input$intersected_UNI_bs$datapath[1]) %>% select(1:9)%>% 
                                                                                `colnames<-`(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs", "chr", "start", "end", "sequence", "Tm"))),
                       ms1 = if(!is.null(input$intersected_MS1)) tibble(fread(input$intersected_MS1$datapath[1]) %>% select(1:9)%>% 
                                                                          `colnames<-`(c("chr_ms1","start_ms1", "end_ms1", "id_ms1", "chr", "start", "end", "sequence", "Tm"))),
                       ms2 = if(!is.null(input$intersected_MS2)) tibble(fread(input$intersected_MS2$datapath[1]) %>% select(1:9)%>% 
                                                                          `colnames<-`(c("chr_ms2","start_ms2", "end_ms2", "id_ms2", "chr", "start", "end", "sequence", "Tm"))),
                       bs1 = if(!is.null(input$intersected_BS1)) tibble(fread(input$intersected_BS1$datapath[1]) %>% select(1:9)%>% 
                                                                          `colnames<-`(c("chr_bs1","start_bs1", "end_bs1", "id_bs1", "chr", "start", "end", "sequence", "Tm"))),
                       bs2 = if(!is.null(input$intersected_BS2)) tibble(fread(input$intersected_BS2$datapath[1]) %>% select(1:9)%>% 
                                                                          `colnames<-`(c("chr_bs2","start_bs2", "end_bs2", "id_bs2", "chr", "start", "end", "sequence", "Tm")))
      ) %>%
        compact() # removes NULL elements in the list
    } #else{
    #   bed_list <- NULL
    # }
    # 
    
    
    
    
    # combine all the available bed files
    
    if(input$op_select == "new" & !is.null(bed_list)){
      # select the dataframes that do not contain "_i_" in the name of the list
      # rename the columns to contain the name of the street
      # intersect them 
      # then intersect the intersected bed files with the filtered OPs
      
      
      #bed_list <- bed_list[str_detect(string = names(bed_list), pattern = "_i_", negate = T)] #%>% bind_rows(.id = "reduced")
      
      #intersect all bed files between them
      isected <- NULL
      isected_names <- NULL
      
      if(length(bed_list) > 1){
        
        isected <- intersect_coordinates(bed_list[[1]], bed_list[[2]]) %>% 
          { `if`(length(bed_list) >= 3, intersect_coordinates(., bed_list[[3]]), . ) } %>%
          { `if`(length(bed_list) >= 4, intersect_coordinates(., bed_list[[4]]), . ) } %>%
          { `if`(length(bed_list) >= 5, intersect_coordinates(., bed_list[[5]]), . ) } %>%
          { `if`(length(bed_list) >= 6, intersect_coordinates(., bed_list[[6]]), . ) } 
        
        
        isected_names <- c(names(bed_list[[2]]), names(bed_list[[1]])) %>% 
          { `if`(length(bed_list) >= 3, c(names(bed_list[[3]]), .), . ) } %>% 
          { `if`(length(bed_list) >= 4, c(names(bed_list[[4]]), .), . ) } %>% 
          { `if`(length(bed_list) >= 5, c(names(bed_list[[5]]), .), . ) } %>% 
          { `if`(length(bed_list) >= 6, c(names(bed_list[[6]]), .), . ) }
        
        
        names(isected) <- isected_names
        
        # START DELETE
        # for(i in 1:(length(bed_list)-1)){
        # 
        #   use_names <- c(names(bed_list[[(i+1)]]), names(bed_list[[i]]))
        #   out <- intersect_coordinates(y = bed_list[[(i+1)]], x = bed_list[[i]] , correct_coor = T)
        #   names(out) <- use_names
        #   isected <-bind_cols(isected, out, .name_repair = c("minimal")) 
        # }
        # END DELETE
      } else if(length(bed_list) == 1){
        df <- bed_list[[1]][1:4]
        names_df <- names(df)
        chr_df <- unique(df[[names_df[1]]])
        
        for (i in chr_df){
          x <- df[df[,names_df[1]] == i,]
          x$overlap <- as.vector((lag(x[,3]) >= x[,2])%>%replace(is.na(.), FALSE))
          x$lag_end <- lag(x[,3])
          x[x$overlap,][2] <- x[x$overlap,][6]+1
          df[df[,names_df[1]] == i,] <- x[1:4]
          
        }
        isected <- df
      }
      
      # intersect all bedfiles to ops
      filtered_oligopaints <- filtered_oligopaints() #%>% slice_sample(n =5000) %>% arrange(chr, start)
      isected_ops <- RBedtools(tool = 'intersect',
                               options = '-wa -wb', #'-loj'
                               a=from_data_frame(isected), #RBedtools(tool = 'sort', i=from_data_frame(isected)
                               b=from_data_frame(filtered_oligopaints)) %>% #RBedtools(tool = 'sort', i=from_data_frame(filtered_oligopaints))
        to_data_frame%>% 
        `colnames<-`(c(names(isected), names(filtered_oligopaints))) 
      
      # EXTRA 
      # select non duplicate columns
      #isected_ops <- isected_ops[,grep("....[0-9]$", names(isected_ops))]
      #isected_ops <- isected_ops[,!str_detect(names(isected_ops), "....[0-9]")]
      #isected_ops <- select(isected_ops, -contains("..."))
      isected_ops <- isected_ops %>% select(any_of(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms",
                                                     "chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs",
                                                     "chr_ms1","start_ms1", "end_ms1", "id_ms1",
                                                     "chr_ms2","start_ms2", "end_ms2", "id_ms2",
                                                     "chr_bs1","start_bs1", "end_bs1", "id_bs1",
                                                     "chr_bs2","start_bs2", "end_bs2", "id_bs2",
                                                     names(filtered_oligopaints))))
      
      #names(isected_ops) <- c(Reduce(c, map(bed_list, names)), names(filtered_oligopaints()))
      #names(isected_ops) <- c(names(isected), names(filtered_oligopaints()))
      
      
    } else if(input$op_select == "intersected" && !is.null(bed_list)){
      isected_ops <- Reduce(full_join, bed_list)
      
      # Check if isected_ops is not null and has rows
      if(!is.null(isected_ops) && nrow(isected_ops) > 0) {
        isected_ops <- isected_ops %>% drop_na()
      } else {
        isected_ops <- NULL
      }
    }
    
    # # FIX
    # # remove OPs from coordinates that should be OPs-free
    # if (!is.null(input$coor_filter)){
    #   coor_filter <- tibble(fread(input$coor_filter$datapath))
    #   
    #   if (ncol(coor_filter) >= 3){
    #     coor_filter <- coor_filter %>% select(1:3) %>%
    #       `colnames<-`(c("chr","start", "end"))
    #     
    #     # Ensure that both data frames are not empty
    #     if (nrow(comb_ops()) > 0 && nrow(coor_filter) > 0) {
    #       DT <- intersect_coordinates( y = oligopaints_data(), x = coor_filter, correct_coor = T, .options = '-v')
    # 
    #       names(DT) <- c(names(coor_filter), names(oligopaints_data()))
    #       print(DT)
    #       
    #       # return(as.data.frame(DT))
    #     } else {
    #       # Handle the case where one of the data frames is empty
    #       showModal(modalDialog(
    #         title = "Error",
    #         "One of the data frames (DT or coor_filter) is empty.",
    #         easyClose = TRUE,
    #         footer = NULL
    #       ))
    #     }
    #   } else {
    #     showModal(modalDialog(
    #       title = "Error",
    #       "The file you uploaded is not correct. Please make sure your file has three columns: chr, start and end.",
    #       easyClose = TRUE,
    #       footer = NULL
    #     ))
    #   }
    # } 
    

    
    return(isected_ops)
    

    # TO DELETE START?
    # # check whether the user has updated intersected files or files that need to be intersected
    #   # for intersected files
    #   # FIX Add input$selected == "intersected" is selected
    #   if(sum(str_detect(string = names(bed_list), pattern = "_i_")) > 0){
    #     
    #     # select the dataframes that contain "_i_" in the name of the list
    #     # intersect them to create the comb_ops file
    #     # This will do full join for all elements in a list
    #     # at the end it will drop any OPs that do not overlap between chromosome segments
    #       Reduce(full_join,bed_list[str_detect(string = names(bed_list), pattern = "_i_")]) %>% 
    #       drop_na()
    #     
    #     
    #     
    #   # for bed files to be intersected
    #   # FIX Add input$selected == "new" is selected
    #   } else if(sum(str_detect(string = names(bed_list), pattern = "_i_", negate = T)) > 0) {
    #     # select the dataframes that do not contain "_i_" in the name of the list
    #     # rename the columns to contain the name of the street
    #     # intersect them 
    #     # then intersect the intersected bed files with the filtered OPs
    #     
    #     bed_list <- bed_list[str_detect(string = names(bed_list), pattern = "_i_", negate = T)] #%>% bind_rows(.id = "reduced")
    # 
    #     # intersect all bed files between them
    #     isected <- NULL
    # 
    #     if(length(bed_list) > 1){
    #       for(i in 1:(length(bed_list)-1)){
    #         
    #         use_names <- c(names(bed_list[[(i+1)]]), names(bed_list[[i]]))
    #         out <- intersect_coordinates(y = bed_list[[(i+1)]], x = bed_list[[i]] , correct_coor = T)
    #         names(out) <- use_names
    #         isected <-bind_cols(isected, out, .name_repair = c("minimal"))
    #       }
    #     } else if(length(bed_list) == 1){
    #       df <- bed_list[[1]][1:4]
    #       names_df <- names(df)
    #       chr_df <- unique(df[[names_df[1]]])
    #       
    #       for (i in chr_df){
    #         x <- df[df[,names_df[1]] == i,]
    #         x$overlap <- as.vector((lag(x[,3]) >= x[,2])%>%replace(is.na(.), FALSE)) 
    #         x$lag_end <- lag(x[,3])
    #         x[x$overlap,][2] <- x[x$overlap,][6]+1
    #         df[df[,names_df[1]] == i,] <- x[1:4]
    #         
    #       }
    #       isected <- df
    #     } 
    # 
    #     # intersect all bedfiles to ops
    #     filtered_oligopaints_small <- filtered_oligopaints() %>% slice_sample(n =5000)
    #     isected_ops <- RBedtools(tool = 'intersect',
    #                              options = '-loj',
    #                              a=from_data_frame(isected),
    #                              b=RBedtools(tool = 'sort', i=from_data_frame(filtered_oligopaints_small))) %>% 
    #                              to_data_frame
    #     #names(isected_ops) <- c(Reduce(c, map(bed_list, names)), names(filtered_oligopaints()))
    #     #names(isected_ops) <- c(names(isected), names(filtered_oligopaints()))
    #     
    #     return(isected_ops)
    #   }
    
    
    
    # TO DELETE END?
    
    
    
  }) %>% 
    bindEvent(input$op_intersected,
              input$op_new)
  
  
  # homogenize the density of OPs if the user has inputted a density target input$filter_density
  # also filter the regions that the user defined as OPs-free
  comb_ops_f <- reactive({
    
    if(!is.na(input$filter_density)){
      
      #define the column name to homogenize the density of OPs
      street_to_select <- switch(input$selected_street,
                                 "intersected_uni_ms" = "id_uni_ms",
                                 "uni_ms" = "id_uni_ms",
                                 "intersected_uni_bs" = "id_uni_bs",
                                 "uni_bs" = "id_uni_bs",
                                 "intersected_ms1" = "id_ms1",
                                 "ms1" = "id_ms1",
                                 "intersected_ms2" = "id_ms2",
                                 "ms2" = "id_ms2",
                                 "intersected_bs1" = "id_bs1",
                                 "bs1" = "id_bs1",
                                 "intersected_bs2" = "id_bs2",
                                 "bs2" = "id_bs2")
      
      # function that homogenizes the density of OPs
      comb_ops() %>% 
        drop_na() %>% 
        arrange(chr, start) %>% 
        group_by(chr) %>% 
        group_modify(~ { 
          .x %>% 
            mutate(distance = start - lag(end, default = 0)) %>% 
            mutate(distance = replace_na(distance, 0))
        }) %>%
        filter(between(distance, 0, 1000000)) %>%
        ungroup() %>%
        group_by(chr, !!sym(street_to_select)) %>%
        group_split() %>%
        map(~ {
          # Calculation of size_chunk and n_probes
          size_chunk <- max(.x$end) - min(.x$start)
          n_probes <- round((size_chunk/1000) * input$filter_density, 0)
          
          # Check if there are enough rows and positive weights
          if(n_probes > 0 && nrow(.x) >= n_probes && any(.x$distance > 0)) {
            slice_sample(.x, n = min(n_probes, nrow(.x)), weight_by = .x$distance)
          } else {
            .x  # Return the data as is if conditions are not met
          }
        }) %>%
        bind_rows() %>%
        arrange(chr, start)

      
      
      
      # return(isected_ops)
    } else {
      comb_ops() 
    }
      
  }) %>% 
    bindEvent(input$filter_ops)
  
  # This is a reactive element that triggers a change when oligopaints source is changed or op_intersected upload button is pressed
  listen_source_intersected <- reactive({
    paste(input$op_intersected , input$op_select, input$filter_ops)
  })
  
  # This is a reactive element that triggers a change when oligopaints source is changed or op_new upload button is pressed
  listen_source_new <- reactive({
    paste(input$op_new , input$op_select, input$filter_ops)
  })
  
  
  ######### Universal Barcodes outputs #########
  
  ########## UNIVERSAL MAINSTREET   ########## 
  
  ###### Use genomic coordinates ###### 
  # makes uploading files for MS and BS visible when auto universals is not clicked
  # observe({
  #   toggle(id = "UNI_ms", condition = !input$auto_uni)
  # })
  # 
  # observe({
  #   toggle(id = "UNI_bs", condition = !input$auto_uni)
  # })
  
  # Intersects *New* Universal Mainstreets to filtered Oligopaints
  universals_ms <- print("")
  universals_ms <-eventReactive(input$op_new,{
    req(input$UNI_ms, input$op_new)
    validate(
      need(input$UNI_ms != "", " "),
      need("input.streets.includes('uni_ms')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
      comb_ops() %>% 
        select(any_of(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }

    
    
    
    # df_ms <- tibble(fread(input$UNI_ms$datapath[1]) %>%
    #                   select(1:4) %>%
    #                   `colnames<-`(c("chr", "start", "end", "id")))
    # filtered_oligopaints <- filtered_oligopaints()
    # to_data_frame(RBedtools(tool = 'intersect',
    #                         options = '-loj',
    #                         a= from_data_frame(df_ms),
    #                         b= from_data_frame(filtered_oligopaints))) %>%
    #   `colnames<-`(c(str_c(colnames(df_ms), rep("_uni_ms", length(colnames(df_ms)))), colnames(filtered_oligopaints()))) %>%
    #   filter(start > 0)
    
    
  }, ignoreNULL = FALSE)
  
  
  
  
  
  
  ## table
  table_UNI_ms <- eventReactive(listen_source_new(),{
    make_table(universals_ms())
  })
  output$table_UNI_ms <- DT::renderDataTable({
    validate(
      need(input$UNI_ms != "", message = FALSE),
      need("input.UNI_ms.length > 0", message = FALSE)
      
      #"Seems like you have not uploaded any Mainstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_UNI_ms()
  })
  
  ## summary
  summary_UNI_ms <- eventReactive(listen_source_new(),{
    make_summary(universals_ms())
  })
  output$summary_UNI_ms <- DT::renderDataTable({
    validate(
      need(input$UNI_ms != "", " "),
      need("input.streets.includes('uni_ms')", message = F)
    )
    summary_UNI_ms()
  })
  
  # number of files chosen
  output$uni_n_ms <- renderText({
    if(is.null(input$UNI_ms)) {
      paste("Please, upload Mainstreet Universal sequence .bed file.")
    } else{
      paste("Universal Mainstreet .bed file is uploaded.")
    }
  })

  
  ###### Use intersected oligopaints ###### 
  # makes uploading files for MS and BS visible when auto universals is not clicked
  # observe({
  #   toggle(id = "intersected_UNI_ms", condition = !input$intersected_auto_uni)
  # })
  # 
  # observe({
  #   toggle(id = "intersected_UNI_bs", condition = !input$intersected_auto_uni)
  # })
  

  
  # Creates statistics from elsewhere intersected Universal Mainstreets
  
  intersected_universals_ms <- print("")
  intersected_universals_ms <- eventReactive(listen_source_intersected(),{
    req(input$intersected_UNI_ms, input$op_intersected)
    validate(
      need(input$intersected_UNI_ms != "", " "),
      need("input.intersected_streets.includes('intersected_uni_ms')", message = F)
    )
    if(req(input$op_select) == "intersected"){
      
      if(input$filter_ops){
        comb_ops_f() %>% 
        select(any_of(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms", 
                          "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
      }else{
      comb_ops() %>% 
        select(any_of(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms", 
               "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
      }
      
      # fread(input$intersected_UNI_ms$datapath[1]) %>% 
      #   select(1:9) %>%
      #   `colnames<-`(c("chr_uni_ms","start_uni_ms", "end_uni_ms", "id_uni_ms", "chr", "start", "end", "sequence", "Tm")) #, "probe_strand"
    } else if(input$op_select != "intersected"){
      NULL
    }
  }, ignoreNULL = FALSE)
  
  
  # observeEvent(input$UNI_ms, {
  #   df_ms <- tibble(fread(input$UNI_ms$datapath[1]) %>%
  #                     select(1:4))
  #   
  #   if(!bedtools_merge_test(df_ms)){
  #     showNotification("Attention! Universal Main Street coordinates have overlaps. Correct them and upload the correct coordinates.", 
  #                      type = "error",
  #                      duration = NULL)
  #   }
  #   
  # })
  
  
 
 
  
  # TO DELETE  START
  # this was within bindEvent and it can now be removed if the element is reactive with input$upload
  
  # input$op_select,
  # input$UNI_ms,
  # input$intersected_UNI_ms,
  # input$UNI_bs,
  # input$intersected_UNI_bs,
  # input$MS1,
  # input$intersected_MS1,
  # input$MS2,
  # input$intersected_MS2,
  # input$BS1,
  # input$intersected_BS1,
  # input$BS2,
  # input$intersected_BS2
  # TO DELETE END
  
  
  
  
  ## table
  intersected_table_UNI_ms <- eventReactive(listen_source_intersected(),{
    if(req(input$op_select) == "intersected"){
      make_table(intersected_universals_ms())
    } else if(input$op_select != "intersected"){
      NULL
    }
  }, ignoreNULL = FALSE)
  output$intersected_table_UNI_ms <- DT::renderDataTable({
    validate(
      need(input$intersected_UNI_ms != "", message = FALSE),
      need("input.intersected_UNI_ms.length > 0", message = FALSE)
      
      #"Seems like you have not uploaded any Mainstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    intersected_table_UNI_ms()
    
  })
  
  ## summary
  intersected_summary_UNI_ms <- eventReactive(listen_source_intersected(),{
    if(req(input$op_select) == "intersected"){
      make_summary(intersected_universals_ms())
    } else if(input$op_select != "intersected"){
      NULL
    }
  }, ignoreNULL = FALSE)
  output$intersected_summary_UNI_ms <- DT::renderDataTable({
    validate(
      need(input$intersected_UNI_ms != "", " "),
      need("input.intersected_streets.includes('intersected_uni_ms')", message = F)
    )
    intersected_summary_UNI_ms()
  }, ignoreNULL = FALSE)
  
  # number of files chosen
  output$intersected_uni_n_ms <- renderText({
    if(is.null(input$intersected_UNI_ms)) {
      paste("Please, upload Mainstreet Universal sequence .bed file.")
    } else{
      paste("Universal Mainstreet .bed file is uploaded.")
    }
  })
  
  
  ########## UNIVERSAL BACKSTREET   ########## 
  universals_bs <- print("")
  universals_bs <-eventReactive(listen_source_new(),{
    req(input$UNI_bs, input$op_new)
    validate(
      need(input$UNI_bs != "", " "),
      need("input.streets.includes('uni_bs')", message = F)
    )
    
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs",
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    
    
    # DELETE START
    # df_bs <- tibble(fread(input$UNI_bs$datapath[1]) %>%
    #                   select(1:4) %>%
    #                   `colnames<-`(c("chr", "start", "end", "id")))
    # filtered_oligopaints <- filtered_oligopaints()
    # to_data_frame(RBedtools(tool = 'intersect',
    #                         options = '-loj',
    #                         a= from_data_frame(df_bs),
    #                         b= from_data_frame(filtered_oligopaints))) %>%
    #   `colnames<-`(c(str_c(colnames(df_bs), rep("_uni_bs", length(colnames(df_bs)))), colnames(filtered_oligopaints()))) %>%
    #   filter(start > 0)
    # DELETE END
    
  }, ignoreNULL = FALSE)
  
  
  
  
  ## table
  table_UNI_bs <- eventReactive(listen_source_new(),{
    make_table(universals_bs())
  })
  output$table_UNI_bs <- DT::renderDataTable({
    validate(
      need(input$UNI_bs != "", message = FALSE),
      need("input.UNI_bs.length > 0", message = FALSE)
      
      #"Seebs like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_UNI_bs()
  })
  
  ## summary
  summary_UNI_bs <- eventReactive(listen_source_new(),{
    make_summary(universals_bs())
  })
  output$summary_UNI_bs <- DT::renderDataTable({
    validate(
      need(input$UNI_bs != "", " "),
      need("input.streets.includes('uni_bs')", message = F)
    )
    summary_UNI_bs()
  })
  
  # number of files chosen
  output$uni_n_bs <- renderText({
    if(is.null(input$UNI_bs)) {
      paste("Please, upload Backstreet Universal sequence .bed file.")
    } else{
      paste("Universal Backstreet .bed file is uploaded.")
    }
  })
  
  
  
  ###### Use intersected oligopaints ###### 
  # makes uploading files for bs and BS visible when auto universals is not clicked
  # observe({
  #   toggle(id = "intersected_UNI_bs", condition = !input$intersected_auto_uni)
  # })
  # 
  # observe({
  #   toggle(id = "intersected_UNI_bs", condition = !input$intersected_auto_uni)
  # })
  
  
  
  # Creates statistics for elsewhere intersected Universal Backstreets
  intersected_universals_bs <- print("")
  intersected_universals_bs <-eventReactive(listen_source_intersected(),{
    req(input$intersected_UNI_bs, input$op_intersected)
    validate(
      need(input$intersected_UNI_bs != "", " "),
      need("input.intersected_streets.includes('intersected_uni_bs')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs",
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # fread(input$intersected_UNI_bs$datapath[1]) %>% 
    #   select(1:9) %>%
    #   `colnames<-`(c("chr_uni_bs","start_uni_bs", "end_uni_bs", "id_uni_bs", "chr", "start", "end", "sequence", "Tm")) #, "probe_strand"
    
  }, ignoreNULL = FALSE)
  
  
  
  
  ## table
  intersected_table_UNI_bs <- eventReactive(listen_source_intersected(),{
    make_table(intersected_universals_bs())
  })
  output$intersected_table_UNI_bs <- DT::renderDataTable({
    validate(
      need(input$intersected_UNI_bs != "", message = FALSE),
      need("input.intersected_UNI_bs.length > 0", message = FALSE)
      
      #"Seebs like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    intersected_table_UNI_bs()
  })
  
  ## summary
  intersected_summary_UNI_bs <- eventReactive(listen_source_intersected(),{
    make_summary(intersected_universals_bs())
  })
  output$intersected_summary_UNI_bs <- DT::renderDataTable({
    validate(
      need(input$intersected_UNI_bs != "", " "),
      need("input.intersected_streets.includes('intersected_uni_bs')", message = F)
    )
    intersected_summary_UNI_bs()
  })
  
  # number of files chosen
  output$intersected_uni_n_bs <- renderText({
    if(is.null(input$intersected_UNI_bs)) {
      paste("Please, upload Backstreet Universal sequence .bed file.")
    } else{
      paste("Universal Backstreet .bed file is uploaded.")
    }
  })
  
  
  
  ######### MainStreets Barcodes outputs #########  
  
  ###### Use genomic coordinates ###### 
  
  main_street_1 <-eventReactive(listen_source_new(),{
    req(input$MS1, input$op_new)
    validate(
      need(input$MS1 != "", " "),
      need("input.streets.includes('ms1')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_ms1","start_ms1", "end_ms1", "id_ms1", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_ms1","start_ms1", "end_ms1", "id_ms1", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # DELETE START
    # df_ms1 <- tibble(fread(input$MS1$datapath[1]) %>%
    #                    select(1:4) %>%
    #                    `colnames<-`(c("chr", "start", "end", "id")))
    # filtered_oligopaints <- filtered_oligopaints()
    # 
    # main_street_1 <- RBedtools(tool = 'intersect',
    #                            options = '-loj',
    #                            a= from_data_frame(df_ms1),
    #                            b= from_data_frame(filtered_oligopaints)) 
    # fread(main_street_1) %>%
    #   `colnames<-`(c(str_c(colnames(df_ms1), rep("_ms1", length(colnames(df_ms1)))), colnames(filtered_oligopaints()))) %>%
    #   filter(start > 0) %>%
    #   mutate(chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm),on_target=as.numeric(on_target),off_target= as.numeric(off_target), is_repeat= as.integer(is_repeat), max_kmer= as.integer(max_kmer),probe_strand= as.character(probe_strand))
    # # DELETE END
    
  }, ignoreNULL = FALSE)
  
  
  main_street_2 <-eventReactive(listen_source_new(),{
    validate(
      need(input$MS2 != "", " "),
      need("input.streets.includes('ms2')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_ms2","start_ms2", "end_ms2", "id_ms2", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_ms2","start_ms2", "end_ms2", "id_ms2", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # DELETE START
    # df_ms2 <- tibble(fread(input$MS2$datapath[1]) %>%
    #                    select(1:4) %>%
    #                    `colnames<-`(c("chr", "start", "end", "id")))
    # filtered_oligopaints <- filtered_oligopaints()
    # 
    # main_street_2 <- RBedtools(tool = 'intersect',
    #                            options = '-loj',
    #                            a= from_data_frame(df_ms2),
    #                            b= from_data_frame(filtered_oligopaints)) 
    # fread(main_street_2)%>%
    #   `colnames<-`(c(str_c(colnames(df_ms2), rep("_ms2", length(colnames(df_ms2)))), colnames(filtered_oligopaints()))) %>%
    #   filter(start > 0) %>%
    #   mutate(chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm),on_target=as.numeric(on_target),off_target= as.numeric(off_target), is_repeat= as.integer(is_repeat), max_kmer= as.integer(max_kmer),probe_strand= as.character(probe_strand))
    # DELETE END
    
    
  }, ignoreNULL = FALSE)
  
  
  main_streets <-eventReactive(listen_source_new(),{
    if (is.null(input$MS1) && is.null(input$MS2)){
      return("")
    } else if (!is.null(input$MS1) && is.null(input$MS2)){
      main_street_1()
    } else if (is.null(input$MS1) && !is.null(input$MS2)){
      main_street_2()
    } else if (!is.null(input$MS1) && !is.null(input$MS2)){
      bind_cols(main_street_1(), main_street_2())
      # full_join(main_street_1(), main_street_2()) %>% drop_na()
    } else {
      print("Please, upload .bed file.")
    }
    
  })
  
  ## table 
  table_MS1 <- eventReactive(listen_source_new(),{
    make_table(main_street_1())
  })
  table_MS2 <- eventReactive(listen_source_new(),{
    make_table(main_street_2())
  })
  
  
  
  output$table_MS1 <- DT::renderDataTable({
    validate(
      need(input$MS1 != "", message = FALSE),
      need("input.MS1.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_MS1()
  })
  output$table_MS2 <- DT::renderDataTable({
    validate(
      need(input$MS2 != "", message = FALSE),
      need("input.MS2.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_MS2()
  })
  
  ## summary
  summary_MS1 <- eventReactive(listen_source_new(),{
    make_summary(main_street_1()) 
  })
  summary_MS2 <- eventReactive(listen_source_new(),{
    make_summary(main_street_2()) 
  })
  
  
  output$summary_MS1 <- DT::renderDataTable({
    validate(
      need(input$MS1 != "", " "),
      need("input.streets.includes('ms1')", message = F)
    )
    summary_MS1()
  })
  output$summary_MS2 <- DT::renderDataTable({
    validate(
      need(input$MS2 != "", " "),
      need("input.streets.includes('ms2')", message = F)
    )
    summary_MS2()
  })
  # number of files chosen
  output$ms1_n <- renderText({
    if(is.null(input$MS1)) {
      paste("Please, upload Mainstreet 1 .bed file.")
    } else{
      paste("Mainstreet 1 .bed file is uploaded.") #nrow(input$MS1)
    }
  })
  output$ms2_n <- renderText({
    if(is.null(input$MS2)) {
      paste("Please, upload Mainstreet 2 .bed file.")
    } else{
      paste( "Mainstreet 2 .bed file is uploaded.") #nrow(input$MS2),
    }
  })
  
  
  ###### Use intersected Oligopaints ###### 
  
  intersected_main_street_1 <-eventReactive(listen_source_intersected(),{
    req(input$intersected_MS1, input$op_intersected)
    validate(
      need(input$intersected_MS1 != "", " "),
      need("input.intersected_streets.includes('intersected_ms1')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_ms1","start_ms1", "end_ms1", "id_ms1", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_ms1","start_ms1", "end_ms1", "id_ms1", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # fread(input$intersected_MS1$datapath[1]) %>% 
    #   `colnames<-`(c("chr_ms1","start_ms1", "end_ms1", "id_ms1", "chr", "start", "end", "sequence", "Tm")) %>%
    #   filter(start > 0) %>%
    #   mutate(chr_ms1=as.character(chr_ms1), start_ms1=as.numeric(start_ms1), end_ms1= as.numeric(end_ms1), id_ms1=as.character(id_ms1), chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm))
  }, ignoreNULL = FALSE)
  
  intersected_main_street_2 <-eventReactive(listen_source_intersected(),{
    validate(
      need(input$intersected_MS2 != "", " "),
      need("input.intersected_streets.includes('intersected_ms2')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_ms2","start_ms2", "end_ms2", "id_ms2", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_ms2","start_ms2", "end_ms2", "id_ms2", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # fread(input$intersected_MS2$datapath[1]) %>% 
    #   `colnames<-`(c("chr_ms2","start_ms2", "end_ms2", "id_ms2", "chr", "start", "end", "sequence", "Tm")) %>% 
    #   filter(start > 0) %>%
    #   mutate(chr_ms2=as.character(chr_ms2), start_ms2=as.numeric(start_ms2), end_ms2= as.numeric(end_ms2), id_ms2=as.character(id_ms2), chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm))
  }, ignoreNULL = FALSE)
  
  intersected_main_streets <-eventReactive(listen_source_intersected(),{
    if (is.null(input$intersected_MS1) && is.null(input$intersected_MS2)){
      return("")
    } else if (!is.null(input$intersected_MS1) && is.null(input$intersected_MS2)){
      intersected_main_street_1()
    } else if (is.null(input$intersected_MS1) && !is.null(input$intersected_MS2)){
      intersected_main_street_2()
    } else if (!is.null(input$intersected_MS1) && !is.null(input$intersected_MS2)){
      full_join(intersected_main_street_1(), intersected_main_street_2()) %>% drop_na()
    } else {
      print("Please, upload .bed file.")
    }
    
  })
  
  
  ## table 
  intersected_table_MS1 <- eventReactive(listen_source_intersected(),{
    make_table(intersected_main_street_1())
  })
  
  intersected_table_MS2 <- eventReactive(listen_source_intersected(),{
    make_table(intersected_main_street_2())
  })
  
  
  output$intersected_table_MS1 <- DT::renderDataTable({
    validate(
      need(input$intersected_MS1 != "", message = FALSE),
      need("input.intersected_MS1.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    intersected_table_MS1()
  })
  output$intersected_table_MS2 <- DT::renderDataTable({
    validate(
      need(input$intersected_MS2 != "", message = FALSE),
      need("input.intersected_MS2.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    intersected_table_MS2()
  })
  
  ## summary
  intersected_summary_MS1 <- eventReactive(listen_source_intersected(),{
    make_summary(intersected_main_street_1()) 
  })
  intersected_summary_MS2 <- eventReactive(listen_source_intersected(),{
    make_summary(intersected_main_street_2()) 
  })
  
  
  output$intersected_summary_MS1 <- DT::renderDataTable({
    validate(
      need(input$intersected_MS1 != "", " "),
      need("input.intersected_streets.includes('intersected_ms1')", message = F)
    )
    intersected_summary_MS1()
  })
  output$intersected_summary_MS2 <- DT::renderDataTable({
    validate(
      need(input$intersected_MS2 != "", " "),
      need("input.intersected_streets.includes('intersected_ms2')", message = F)
    )
    intersected_summary_MS2()
  })
  
  # number of files chosen
  output$intersected_ms1_n <- renderText({
    if(is.null(input$intersected_MS1)) {
      paste("Please, upload Mainstreet 1 .bed file.")
    } else{
      paste( "Mainstreet 1.bed file is uploaded.") #nrow(input$intersected_MS1),
    }
  })
  
  output$intersected_ms2_n <- renderText({
    if(is.null(input$intersected_MS2)) {
      paste("Please, upload Mainstreet 2 .bed file.")
    } else{
      paste( "Mainstreet 2.bed file is uploaded.") #nrow(input$intersected_MS2),
    }
  })
  
  ### Backstreet Barcodes outputs
  
  
  ###### Use genomic coordinates ###### 
  #### this removes the upload button when the user decides to append the same sequences as mainstreet to increase signal for example
  observe({
    toggle(id = "BS1", condition = !input$same_BS1)
    if(input$same_BS1){
      hide("barcode_error_bs1")
      updateSelectInput(session, "streets",
                        selected = c(input$streets, c("ms1", "bs1")))
    }
  })
  
  observe({
    toggle(id = "BS2", condition = !input$same_BS2)
    if(input$same_BS2){
      hide("barcode_error_bs2")
      updateSelectInput(session, "streets",
                        selected = c(input$streets, c("ms2", "bs2")))
    }
  })
  
  back_street_1 <-eventReactive(listen_source_new(),{
    req(input$BS1, input$op_new)
    validate(
      need(input$BS1 != "", " "),
      need("input.streets.includes('bs1')", message = F)
    )
    
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_bs1","start_bs1", "end_bs1", "id_bs1", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_bs1","start_bs1", "end_bs1", "id_bs1", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # DELETE START
    # df_bs1 <- tibble(fread(input$BS1$datapath[1]) %>%
    #                    select(1:4) %>%
    #                    `colnames<-`(c("chr", "start", "end", "id")))
    # filtered_oligopaints <- filtered_oligopaints()
    # 
    # back_street_1 <- RBedtools(tool = 'intersect',
    #                            options = '-loj',
    #                            a= from_data_frame(df_bs1),
    #                            b= from_data_frame(filtered_oligopaints)) 
    # fread(back_street_1) %>%
    #   `colnames<-`(c(str_c(colnames(df_bs1), rep("_bs1", length(colnames(df_bs1)))), colnames(filtered_oligopaints()))) %>%
    #   filter(start > 0) %>%
    #   mutate(chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm),on_target=as.numeric(on_target),off_target= as.numeric(off_target), is_repeat= as.integer(is_repeat), max_kmer= as.integer(max_kmer),probe_strand= as.character(probe_strand))
    # DELETE END
  }, ignoreNULL = FALSE)
  
  
  back_street_2 <-eventReactive(listen_source_new(),{
    req(input$BS2, input$op_new)
    validate(
      need(input$BS2 != "", " "),
      need("input.streets.includes('bs2')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_bs2","start_bs2", "end_bs2", "id_bs2", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_bs2","start_bs2", "end_bs2", "id_bs2", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # df_bs2 <- tibble(fread(input$BS2$datapath[1]) %>%
    #                    select(1:4) %>%
    #                    `colnames<-`(c("chr", "start", "end", "id")))
    # filtered_oligopaints <- filtered_oligopaints()
    # 
    # back_street_2 <- RBedtools(tool = 'intersect',
    #                            options = '-loj',
    #                            a= from_data_frame(df_bs2),
    #                            b= from_data_frame(filtered_oligopaints)) 
    # fread(back_street_2)%>%
    #   `colnames<-`(c(str_c(colnames(df_bs2), rep("_bs2", length(colnames(df_bs2)))), colnames(filtered_oligopaints()))) %>%
    #   filter(start > 0) %>%
    #   mutate(chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm),on_target=as.numeric(on_target),off_target= as.numeric(off_target), is_repeat= as.integer(is_repeat), max_kmer= as.integer(max_kmer),probe_strand= as.character(probe_strand))
    # 
  }, ignoreNULL = FALSE)
  
  
  back_streets <-eventReactive(listen_source_new(),{
    if (is.null(input$BS1) && is.null(input$BS2)){
      return("")
    } else if (!is.null(input$BS1) && is.null(input$BS2)){
      back_street_1()
    } else if (is.null(input$BS1) && !is.null(input$BS2)){
      back_street_2()
    } else if (!is.null(input$BS1) && !is.null(input$BS2)){
      bind_cols(back_street_1(), back_street_2())
      #full_join(back_street_1(), back_street_2()) %>% drop_na()
    } else {
      print("Please, upload .bed file.")
    }
  })
  
  
  
  
  ## table 
  table_BS1 <- eventReactive(listen_source_new(),{
    make_table(back_street_1())
  })
  table_BS2 <- eventReactive(listen_source_new(),{
    make_table(back_street_2())
  })
  
  output$table_BS1 <- DT::renderDataTable({
    validate(
      need(input$BS1 != "", message = FALSE),
      need("input.BS1.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_BS1()
  })
  output$table_BS2 <- DT::renderDataTable({
    validate(
      need(input$BS2 != "", message = FALSE),
      need("input.BS2.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_BS2()
  })
  output$table_BS1 <- DT::renderDataTable({
    validate(
      need(input$BS1 != "", message = FALSE),
      need("input.BS1.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_BS1()
  })
  output$table_BS2 <- DT::renderDataTable({
    validate(
      need(input$BS2 != "", message = FALSE),
      need("input.BS2.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    table_BS2()
  })
  
  ## summary
  summary_BS1 <- eventReactive(listen_source_new(),{
    make_summary(back_street_1()) 
  })
  summary_BS2 <- eventReactive(listen_source_new(),{
    make_summary(back_street_2()) 
  })
  
  
  output$summary_BS1 <- DT::renderDataTable({
    validate(
      need(input$BS1 != "", " "),
      need("input.streets.includes('bs1')", message = F)
    )
    summary_BS1()
  })
  output$summary_BS2 <- DT::renderDataTable({
    validate(
      need(input$BS2 != "", " "),
      need("input.streets.includes('bs2')", message = F)
    )
    summary_BS2()
  })
  # number of files chosen
  output$bs1_n <- renderText({
    if(is.null(input$BS1)) {
      paste("Please, upload Backstreet 1 .bed file.")
    } else{
      paste("Backstreet 1 .bed file is uploaded.") #nrow(input$BS1), 
    }
  })
  output$bs2_n <- renderText({
    if(is.null(input$BS2)) {
      paste("Please, upload Backstreet 2 .bed file.")
    } else{
      paste( "Backstreet 2 .bed file is uploaded.") #nrow(input$BS2),
    }
  })
  
  
  ###### Use intersected Oligopaints ###### 
  
  #### this removes the upload button when the user decides to append the same sequences as mainstreet
  # observe({
  #   toggle(id = "intersected_BS", condition = !input$intersected_same_BS)
  # })
  
  observe({
    toggle(id = "intersected_BS1", condition = !input$intersected_same_BS1)
    if(input$intersected_same_BS1){
      hide("barcode_error_bs1")
      updateSelectInput(session, "intersected_streets",
                        selected = c(input$intersected_streets, c("intersected_ms1", "intersected_bs1")))
    }
  })
  
  observe({
    toggle(id = "intersected_BS2", condition = !input$intersected_same_BS2)
    if(input$intersected_same_BS2){
      hide("barcode_error_bs2")
      updateSelectInput(session, "intersected_streets",
                        selected = c(input$intersected_streets, c("intersected_ms2", "intersected_bs2")))
    }
  })
  
  
  
  
  intersected_back_street_1 <-eventReactive(listen_source_intersected(),{
    req(input$intersected_BS1, input$op_intersected)
    validate(
      need(input$intersected_BS1 != "", " "),
      need("input.intersected_streets.includes('intersected_bs1')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_bs1","start_bs1", "end_bs1", "id_bs1", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_bs1","start_bs1", "end_bs1", "id_bs1", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # fread(input$intersected_BS1$datapath[1]) %>% 
    #   `colnames<-`(c("chr_bs1","start_bs1", "end_bs1", "id_bs1", "chr", "start", "end", "sequence", "Tm")) %>%  #, "probe_strand"     
    #   filter(start > 0) %>%
    #   mutate(chr_bs1=as.character(chr_bs1), start_bs1=as.numeric(start_bs1), end_bs1= as.numeric(end_bs1), id_bs1=as.character(id_bs1), chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm))
  }, ignoreNULL = FALSE)
  
  
  intersected_back_street_2 <-eventReactive(listen_source_intersected(),{
    req(input$intersected_BS2, input$op_intersected)
    validate(
      need(input$intersected_BS2 != "", " "),
      need("input.intersected_streets.includes('intersected_bs2')", message = F)
    )
    
    if(input$filter_ops){
      comb_ops_f() %>% 
        select(any_of(c("chr_bs2","start_bs2", "end_bs2", "id_bs2", 
                        "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }else{
    comb_ops() %>% 
      select(any_of(c("chr_bs2","start_bs2", "end_bs2", "id_bs2", 
             "chr", "start", "end", "sequence", "Tm","on_target","off_target", "is_repeat", "max_kmer","probe_strand")))
    }
    # fread(input$intersected_BS2$datapath[1]) %>% 
    #   `colnames<-`(c("chr_bs2","start_bs2", "end_bs2", "id_bs2", "chr", "start", "end", "sequence", "Tm"))  %>%  #, "probe_strand"     
    #   filter(start > 0) %>%
    #   mutate(chr_bs2=as.character(chr_bs2), start_bs2=as.numeric(start_bs2), end_bs2= as.numeric(end_bs2), id_bs2=as.character(id_bs2), chr=as.character(chr), start=as.numeric(start), end=  as.numeric(end), sequence=as.character(sequence), Tm=as.numeric(Tm))
  }, ignoreNULL = FALSE)
  
  intersected_back_streets <-eventReactive(listen_source_intersected(),{
    if (is.null(input$intersected_BS1) && is.null(input$intersected_BS2)){
      return("")
    } else if (!is.null(input$intersected_BS1) && is.null(input$intersected_BS2)){
      intersected_back_street_1()
    } else if (is.null(input$intersected_BS1) && !is.null(input$intersected_BS2)){
      intersected_back_street_2()
    } else if (!is.null(input$intersected_BS1) && !is.null(input$intersected_BS2)){
      full_join(intersected_back_street_1(), intersected_back_street_2()) %>% drop_na()
    } else {
      print("Please, upload .bed file.")
    }
    
  })
  
  ## table 
  intersected_table_BS1 <- eventReactive(listen_source_intersected(),{
    make_table(intersected_back_street_1())
  })
  intersected_table_BS2 <- eventReactive(listen_source_intersected(),{
    make_table(intersected_back_street_2())
  })
  
  
  output$intersected_table_BS1 <- DT::renderDataTable({
    validate(
      need(input$intersected_BS1 != "", message = FALSE),
      need("input.intersected_BS1.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    intersected_table_BS1()
  })
  output$intersected_table_BS2 <- DT::renderDataTable({
    validate(
      need(input$intersected_BS2 != "", message = FALSE),
      need("input.intersected_BS2.length > 0", message = FALSE)
      #"Seems like you have not uploaded any Backstreet Universal .bed file yet, or you are opting for automatic universal sequence assignment."),
    )
    intersected_table_BS2()
  })
  
  ## summary
  intersected_summary_BS1<- eventReactive(listen_source_intersected(),{
    make_summary(intersected_back_street_1()) 
  })
  intersected_summary_BS2<- eventReactive(listen_source_intersected(),{
    make_summary(intersected_back_street_2()) 
  })
  
  
  output$intersected_summary_BS1 <- DT::renderDataTable({
    validate(
      need(input$intersected_BS1 != "", " "),
      need("input.intersected_streets.includes('intersected_bs1')", message = F)
    )
    intersected_summary_BS1()
  })
  output$intersected_summary_BS2 <- DT::renderDataTable({
    validate(
      need(input$intersected_BS2 != "", " "),
      need("input.intersected_streets.includes('intersected_bs2')", message = F)
    )
    intersected_summary_BS2()
  })
  # number of files chosen
  output$intersected_bs1_n <- renderText({
    if(is.null(input$intersected_BS1)) {
      paste("Please, upload Backstreet 1 .bed file.")
    } else{
      paste("Backstreet 1 .bed file is uploaded.") #nrow(input$intersected_BS1), 
    }
  })
  output$intersected_bs2_n <- renderText({
    if(is.null(input$intersected_BS2)) {
      paste("Please, upload Backstreet 2 .bed file.")
    } else{
      paste( "Backstreet 2 .bed file is uploaded.") #nrow(input$intersected_BS2),
    }
  })
  
  
  
  ##### Chromosome plot of uploaded probes
  
  # combine all data into one big data frame
  
  #### START DELETE
  # 
  # state <- reactiveValues()
  # 
  # observe({
  #   # perform if and elses here to get the correct file...
  #   state$panel <- tail(input$collapseStreets,1)
  #   state$nrow_i_uni_ms <- universals_ms_ready() == ""
  #   state$intersected_UNI_MS <- intersected_universals_ms()
  #   state$intersected_UNI_BS <- intersected_universals_bs()
  # 
  # #  state$y <- ifelse(state$x < 4, 1, 0)
  # })
  # 
  #### END DELETE
  
  
  
  #### makes a toListen reactive element that will reactivate isolated code whenever either op_new or op_intersected of advanced filter button is clicked.
  toListen <- reactive({
    list(input$op_new,input$op_intersected, input$filter_ops, input$clear_filters)
  })
  
  
  universals_ms_ready <- reactive({
    
    toListen()
    
    isolate({
      if (!is.null(input$UNI_ms) &&  nrow(universals_ms()) > 0 && req( input$op_select == "new")){
        universals_ms()
      } else if (!is.null(input$intersected_UNI_ms) && nrow(intersected_universals_ms()) > 0 && req(input$op_select == "intersected")) {
        intersected_universals_ms() 
      } else {
        return(NULL)
      }
    })
  })
  
  
  
  universals_bs_ready <- reactive({
    
    toListen()
    
    isolate({
      if (!is.null(input$UNI_bs) &&  nrow(universals_bs()) > 0 && req( input$op_select == "new")){
        universals_bs()
      } else if (!is.null(input$intersected_UNI_bs) && nrow(intersected_universals_bs()) > 0 && req(input$op_select == "intersected")) {
        intersected_universals_bs()     
      } else {
        return(NULL)
      }
    })
  })
  
  
  main_streets_ready <- reactive({
    
    toListen()
    
    isolate({
      if (!is.null(input$MS1) || !is.null(input$MS2) &&  nrow(main_streets()) > 0 && input$op_select == "new"){
        main_streets()
      } else if (!is.null(input$intersected_MS1) || !is.null(input$intersected_MS2) && nrow(intersected_main_streets()) > 0 && input$op_select == "intersected") {
        intersected_main_streets()
      } else {
        return(NULL)
      }
    })
  })
  
  
  back_streets_ready <- reactive({
    
    toListen()
    
    isolate({
      if (!is.null(input$BS1) || !is.null(input$BS2) &&  nrow(back_streets()) > 0 && req( input$op_select == "new")){
        back_streets()
      } else if (!is.null(input$intersected_BS1) || !is.null(input$intersected_BS2) && nrow(intersected_back_streets()) > 0 && req(input$op_select == "intersected")) {
        intersected_back_streets()
        # } else if (!is.null(input$BS) &&  nrow(back_streets()) == 0 && req( input$op_select == "new" && nrow(main_streets()) > 0)){
        #   main_streets() %>%
        #     rename_at(vars(contains("ms")), ~ str_replace(.,"ms", "bs"))  
        # } else if (!is.null(input$intersected_BS) && nrow(intersected_back_streets()) > 0 && req(input$op_select == "intersected" && nrow(main_streets()) > 0)){
        #   intersected_main_streets() %>%
        #     rename_at(vars(contains("ms")), ~ str_replace(.,"ms", "bs")) 
      } else {
        return(NULL)
      }
    })
  })
  
  
  ##### Combine all Oligopaints into one DataFrame    ##### 
  
  # ops_list <- reactive({
  #   list(if(!is.null(universals_ms_ready())) universals_ms_ready(),
  #                  if(!is.null(universals_bs_ready())) universals_bs_ready(),
  #                  if(!is.null(main_streets_ready())) main_streets_ready(),
  #                  if(!is.null(back_streets_ready())) back_streets_ready()
  # ) %>%
  #   compact() # removes NULL elements in the list
  # })
  
  # FIX make event reactive?
  # comb_ops <- reactive({
  #   ops_list <- ops_list()
  #   # ops_list <- list(if(!is.null(universals_ms_ready())) universals_ms_ready(),
  #   #                  if(!is.null(universals_bs_ready())) universals_bs_ready(),
  #   #                  if(!is.null(main_streets_ready())) main_streets_ready(),
  #   #                  if(!is.null(back_streets_ready())) back_streets_ready()
  #   # ) %>% 
  #   #   compact() # removes NULL elements in the list
  #   
  #   if(length(ops_list) == 1){
  #     ops_list[[1]] %>% tibble()
  #   } else {
  #     ops_list %>% 
  #       reduce(left_join)
  #   }
  # })
  
  # FIX
  
  ##### Define mix and max densities for advanced filtering  ##### 
  
  # observe({
  #   # validate(
  #   #   need(!is.null(comb_ops()), "")
  #   # )
  #   toListen()
  #   
  #   isolate({
  #     updateNumericInput(session, "filter_density",
  #                        min = comb_ops %>% select(starts_with(density)) %>% min(), max = comb_ops %>% select(starts_with(density)) %>% max())
  #   })
  # })
  
  #################################### DELETE START
  
  # main_streets <-reactive({
  #       rbindlist(lapply(input$MS$datapath, read.table, header = input$header_MS, col.names =c("chr", "start", "end","id","chr", "start", "end", "sequence", "Tm") , sep = "\t"),
  #               use.names = TRUE, fill = TRUE)
  # })
  
  
  output$test_table <- DT::renderDataTable({
    # amp_primers()
    #intersected_main_street_2()
    #main_streets_ready()
    # comb_ops_f()
    #comb_ops()
    # make_summary(
    # fread(input$intersected_MS$datapath[1]) %>%
    #   `colnames<-`(c("chr_ms1", "start_ms1", "end_ms1","id_ms1","chr", "start", "end", "sequence", "Tm")) %>%
    #   inner_join(fread(input$intersected_MS$datapath[2]) %>%
    #                `colnames<-`(c("chr_ms2", "start_ms2", "end_ms2","id_ms2","chr", "start", "end", "sequence", "Tm"))))
    # main_streets()
    
    
    
    # inner_join(main_street_1(), main_street_2())
  })
  
  output$test_text <- renderText({
    # colnames(comb_ops())
    # print(input$download_files)
    # print(paste(nrow(comb_ops_f()), nrow(comb_ops())))
    # print(paste(is.null(input$filter_density), !is.na(input$filter_density)))
    # paste(input$append_streets_uni_ms, input$append_streets_uni_bs)
    #names(comb_ops)
    #paste(input$download_files, input$correct_overlap)
    #length(main_streets_ready() %>% select(contains(c("bs1"))) %>% names()) >= 1
    #listen_source_intersected()
    #intersected_universals_ms()
    #input$op_select
    # print(!is.null(main_street_1()))
    
  })
  
  # combo_final <- renderTable({
  #   inner_join(combo[['universals_ms_ready']], combo[['main_streets_ready']], combo[['back_streets_ready']], combo[['universals_bs_ready']])
  # })
  # 
  
  #################################### DELETE END 
  
  
  ### An intention to give an error when coordinates are overlapping between regions of the same street
  # bed_list <- eventReactive(input$op_new, {
  #   list(uni_ms = if(!is.null(universals_ms_ready())) universals_ms_ready(),
  #        uni_bs = if(!is.null(universals_bs_ready())) universals_bs_ready(),
  #        ms = if(!is.null(main_streets_ready())) main_streets_ready(),
  #        bs = if(!is.null(back_streets_ready())) back_streets_ready()
  #   ) %>% 
  #     compact()
  #   bed_list_merged <- map_lgl(bed_list, bedtools_merge_test)
  # }) 
  # 
  # observeEvent(input$op_new,{
  #   if(any(bed_list_merged) == FALSE){
  #     js_string <- 'alert("Overlapping coordinates detected correct your bedfiles and upload them again.");'
  #     session$sendCustomMessage(type='jsCode', list(value = js_string))
  #   } else{
  #     next
  #   }
  #   })
  
  
  
  # Paint the plot
  # output$plot_chr <- renderPlotly(
  #   plot1 <- plot_ly(
  #     x = x(),
  #     y = y(), 
  #     type = 'scatter',
  #     mode = 'markers')
  # )
  
  
  # Value box information creation
  
  # Tm value box
  valuebox_data <- reactive({
    
    if(input$filter_ops){
      (comb_ops_f())$Tm
    }else if(input$filter_ops == FALSE && !is.null(comb_ops())){
      (comb_ops())$Tm
    }else {
      NULL
    }
  }) %>% 
    bindEvent(toListen())
  
  output$total_ops <- renderText({
    length(valuebox_data())    
  }) %>% 
    bindEvent(toListen())
  
  output$mean_Tm <- renderText({
    validate(
      need(!is.null(valuebox_data()), "None")
    )
    round(mean(valuebox_data(), na.rm = TRUE), 2)
  }) %>% 
    bindEvent(toListen())
  
  output$median_Tm <- renderText({
    validate(
      need(!is.null(valuebox_data()), "None")
    )
    round(median(valuebox_data(), na.rm = TRUE), 2)
  }) %>% 
    bindEvent(toListen())
  
  
  output$sparkline <- renderPlotly({
    
    validate(
      need(!is.null(valuebox_data()), "")
    )
    
    data <- density(valuebox_data(), na.rm = TRUE)
    
    hover_text <- paste("Tm: ", round(data$x,2),"°C",
                        "<br>Frequency: ", round(data$y, 2))
    
    plot_ly(x = data$x, y = data$y, type = 'scatter', mode = 'lines',
            text = hover_text, hoverinfo = 'text') %>%
      add_lines(
      # x = ~x, y = ~y,
      color = I("#F1C232"), span = I(1),
      fill = 'tozeroy', alpha = 1
    ) %>%
    layout(
      showlegend = FALSE,
      xaxis = list(visible = F, showgrid = F, title = "Tm °C"),
      yaxis = list(visible = F, showgrid = F, title = "Frequency"),
      hovermode = "closest",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "black"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F) %>%
    htmlwidgets::onRender(
      "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
    ) 
    
  })%>%  
    bindEvent(toListen())
  
  
  # DISTANCE value box
  valuebox_data_dist <- reactive({

    if(input$filter_ops){
      df <- comb_ops_f()
    }else if(input$filter_ops == FALSE && !is.null(comb_ops())){
      df <- comb_ops()
    }else {
      df <- NULL
    }
    
    
    # compute the inter Oligopaints distance
    if(!is.null(df)){
      df <- df %>% 
        ungroup() %>% 
        select(chr, start, end) %>%
        drop_na() %>%
        arrange(chr, start) %>%
        group_by(chr) %>%
        group_modify(~ {
          .x %>%
            mutate(distance = start - lag(end, default = 0)) %>%
            mutate(distance = replace_na(distance, 0))
        }) %>%
        filter(between(distance, 0, 1000000)) %>% 
        pull(distance)
    }

  }) %>% 
    bindEvent(toListen())
  
  # output$ops_dist <- renderText({
  #   length(valuebox_data_dist())    
  # }) %>% 
  #   bindEvent(toListen())
  
  output$mean_dist <- renderText({
    validate(
      need(!is.null(valuebox_data_dist()), "None")
    )
    round(mean(valuebox_data_dist(), na.rm = TRUE))
  }) %>% 
    bindEvent(toListen())
  
  output$median_dist <- renderText({
    validate(
      need(!is.null(valuebox_data_dist()), "None")
    )
    round(median(valuebox_data_dist(), na.rm = TRUE))
  }) %>% 
    bindEvent(toListen())
  
  
  output$sparkline_dist <- renderPlotly({
    
    validate(
      need(!is.null(valuebox_data_dist()), "")
    )
    
    data <- density(valuebox_data_dist(), na.rm = TRUE)
    
    hover_text <- paste("Genomic distance: ", round(data$x,2),
                        "<br>Frequency: ", round(data$y, 2))
    
    plot_ly(x = data$x, y = data$y, type = 'scatter', mode = 'lines',
            text = hover_text, hoverinfo = 'text') %>%
      add_lines(
        color = I("#F1C232"), span = I(1),
        fill = 'tozeroy', alpha = 1
      ) %>%
      layout(
        showlegend = FALSE,
        xaxis = list(visible = F, showgrid = F, title = "Distance (log)", type = "log"),
        yaxis = list(visible = F, showgrid = F, title = "Frequency"),
        hovermode = "closest",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "black"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
      )
    
  })%>% 
    bindEvent(toListen())

  
  
  
  ########################################################################################################
  # Panel SELECT BARCODES to append SERVER
  ###############-----------------------------------/2/-------------------------- ######################## 
  
  
  ### START DELETE
  output$test_table2 <- DT::renderDataTable({
    # lambda_toes_lib()
    #available_os_barcodes <- available_os_barcodes()

    # amp_primers()$lambda_primers_out[[1]]
    # amp_primers()$lambda_primers_out %>% 
    #   bind_rows() %>% 
    #   pivot_longer(cols = contains("lambda"), names_to = "lambda_primers", values_to = "lambda_primers")
    # appended_oligopaints()
    # initial_data()
    # comb_ops()
    # input_sec_ofq_reactive()
    # input_sec_lambda_reactive()
    # multiplex_list_out()
    # uni_pairs <- NULL
    # uni_pairs <- create_os_pairs(data = comb_ops(), ms_input = input$append_streets_uni_ms, bs_input = input$append_streets_uni_bs, ms_id = "id_uni_ms", bs_id = "id_uni_bs", .streets = streets(), .toes = toes(), .available_os_barcodes = available_os_barcodes, .matched_streets = matched_streets()) #, .previous_df = NULL
    # if(!is.null(uni_pairs)) {
    #   available_os_barcodes <- setdiff(available_os_barcodes, unique(c(uni_pairs$ms_num, uni_pairs$bs_num)))
    # }
    # street1_pairs <- NULL
    #street1_pairs <- create_pairs(data = comb_ops(), ms_input = input$append_streets_ms1, bs_input = input$append_streets_bs1, ms_id = "id_ms1", bs_id = "id_bs1", .streets = streets(), .toes = toes(), .available_os_barcodes = available_os_barcodes, .matched_streets = matched_streets()) #,.previous_df = uni_pairs
    # if(!is.null(street1_pairs)) {
    #   available_os_barcodes <- setdiff(available_os_barcodes, unique(c(street1_pairs$ms_num, street1_pairs$bs_num, uni_pairs$ms_num, uni_pairs$bs_num)))
    # }
    # street2_pairs <- NULL
    # street2_pairs <- create_os_pairs(data = comb_ops(), ms_input = input$append_streets_ms2, bs_input = input$append_streets_bs2, ms_id = "id_ms2", bs_id = "id_bs2", .streets = streets(), .toes = toes(), .available_os_barcodes = available_os_barcodes, .matched_streets = matched_streets()) #,.previous_df = bind_rows(uni_pairs, street1_pairs)
    #uni_pairs
    #street1_pairs[["df"]]
    
    
    # bind_rows(if(!is.null(uni_pairs)) uni_pairs, if(!is.null(street1_pairs)) street1_pairs, if(!is.null(street2_pairs)) street2_pairs)
    
    # add the option in create_os pairs for the bs to be the same as the ms 
    # join the comb_ops and the barcodes and output it as the final df
    # count, create, and append OFQ barcode
    
    
    
    
    #main_streets_ready()
    #comb_ops() 
    #return(input$barcodes_UNI_ms)
    #appended_oligopaints()
    # make_summary(
    # fread(input$intersected_MS$datapath[1]) %>%
    #   `colnames<-`(c("chr_ms1", "start_ms1", "end_ms1","id_ms1","chr", "start", "end", "sequence", "Tm")) %>%
    #   inner_join(fread(input$intersected_MS$datapath[2]) %>%
    #                `colnames<-`(c("chr_ms2", "start_ms2", "end_ms2","id_ms2","chr", "start", "end", "sequence", "Tm"))))
    # main_streets()
    
    
    
    # inner_join(main_street_1(), main_street_2())
  })
  
  output$test_text2 <- renderText({
    # names(appended_oligopaints())
    # paste("ms1", is.null(input$append_streets_ms1), input$append_streets_ms1, "\n", "bs1", is.null(input$append_streets_bs1), input$append_streets_bs1)
    #return(input$MS1)
    #input$download_files
    # appending_values$available_os_barcodes
    
    #return(input$append_streets_uni_ms == "toe_seq_im" && input$append_streets_uni_ms == "seq_im")
    #return(input$append_streets_uni_ms)
    #input$barcodes_UNI_ms
    # paste(((input$append_streets_uni_ms == "toe_seq_im" | input$append_streets_uni_ms == "seq_im") && (input$append_streets_uni_bs == "toe_seq_im" | input$append_streets_uni_bs == "seq_im")),
    #     ((input$append_streets_uni_ms == "toe_seq_im" | input$append_streets_uni_ms == "seq_im") && (is.null(input$append_streets_uni_bs) | is.null(input$append_streets_uni_bs))),
    #     ((input$append_streets_uni_bs == "toe_seq_im" | input$append_streets_uni_bs == "seq_im") && (is.null(input$append_streets_uni_ms) | is.null(input$append_streets_uni_ms))))
    # 
    
    #input$download_files
    #length(main_streets_ready() %>% select(contains(c("bs1"))) %>% names()) >= 1
    #listen_source_intersected()
    #intersected_universals_ms()
    #input$op_select
    # print(!is.null(main_street_1()))
    
  })
  
  ### END DELETE
  
  
  
  
  
  
  ### import from google drive toe sequences for OligoSTORM of the selected organism
  toes <- reactive({
    
    read_csv(os_toes_barcodes[os_toes_barcodes$Name == {input$organism}, 'link'], col_names = "toes")
    # switch(input$organism,
    #        # "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11" = gsheet2tbl('https://drive.google.com/open?id=1cxeKxa8F3NDK7M856R_dAnWPXlWu8Fda6BwFoHt-whE'),
    #        "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11" = read_csv(os_toes_barcodes[os_toes_barcodes$Name == {input$organism}, 'link'], col_names = "toes"),
    #        "dm6" = gsheet2tbl('https://drive.google.com/open?id=1f3D1ysew8yQJteGRzMiMwacbvjqi3aq8jCc1NmPlQyc'),
    #        "mouse" = gsheet2tbl('https://drive.google.com/open?id=1u7V7eTMDL_rqc9wT8DR3lTjM77vUs26MkDFSDmD8hN0'))
  }) %>% 
    bindEvent(input$organism)
  
  
  ### import from google drive street sequences for OligoSTORM of the selected organism
  streets <- reactive({
    
    read_csv(os_street_barcodes[os_street_barcodes$Name == input$organism, 'link'], col_names = "streets")
    # switch(input$organism,
    #        # "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11" = gsheet2tbl('https://drive.google.com/open?id=1tkQkwv90Hfy9FmcP0fotZ3tXTEq4BUDG9yGmq3coQ1Y'),
    #        "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11" = read_csv(os_street_barcodes[os_street_barcodes$Name == input$organism, 'link'], col_names = "streets"),
    #        "dm6" = gsheet2tbl('https://drive.google.com/open?id=1cpQiOmU4yNp2EXkkmUwnErANXY-SzsxodC0BGewaUhs'),
    #        "mouse" = gsheet2tbl('https://drive.google.com/open?id=1A9i6sw_j_JmgnRTyJSNgnGfI0M8zZzCAiIh5tKTMQWA'))
  }) %>% 
    bindEvent(input$organism)
  
  
  ###--- now that streets are imported avoid_until slider updates with the correct number of total streets
  
  #### START DELETE
  # output$avoid_until <- renderUI({
  #   sliderInput("avoid_until", tippy("OligoSTORM Barcode ID to avoid until: ", "<strong>OligoSTORM barcodes you already used before and want to avoid.</strong>" ),
  #               min = 0, max = nrow(streets()),
  #               value = 0, step = 1,
  #               width = '100%')
  # })
  #### END DELETE
  
  ### import from google drive matched street table for OligoSTORM of the selected organism
  
  matched_streets <- reactive({
    
    read_csv(os_matched_streets[os_matched_streets$Name == {input$organism}, 'link'], col_names = T)
    # switch(input$organism,
    #        # "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11" = gsheet2tbl('https://drive.google.com/open?id=1Eh9ot2QJk35dw6b941g4Dqo5v-AEcXvltQKM572h7Xo'),
    #        "3K - hg38 mm10 dm6 wuhCor1 loxafr3 ce11" = read_csv(os_matched_streets[os_matched_streets$Name == {input$organism}, 'link'], col_names = T),
    #        "dm6" = gsheet2tbl('https://drive.google.com/open?id=1pg150tLPiDSln7p1k1laBf-BCCc4ZtsWRIVUgKr8j5k'),
    #        "mouse" = gsheet2tbl('https://drive.google.com/open?id=1HsyWTXCx33XeiO-V9-FLEIfVtuo-SsH17lq1oGi38pk'))
  }) %>% 
    bindEvent(input$organism)
  
  
  
  #Uni_MS
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  observe({
    toggle(id = "barcodes_UNI_ms", condition = {
       "intersected_uni_ms" %in% input$intersected_streets | "uni_ms" %in% input$streets})
    
    observe({
      toggle(id = "barcodes_UNI_ms_options", condition = {
        !is.null(universals_ms_ready())})
    })
    
    observe({
      toggle(id = "barcode_error_uni_ms", condition = {
        is.null(universals_ms_ready())})
    })
    
    # if(is.null(universals_ms_ready())){ 
    #   show("barcode_error_uni_ms")
    # } else if(!is.null(universals_ms_ready()))  {
    #   hide("barcode_error_uni_ms")
    #   show("barcodes_UNI_ms_options")
    # }
  })  
  
  #Uni_BS
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  observe({
    toggle(id = "barcodes_UNI_bs", condition = {
      "intersected_uni_bs" %in% input$intersected_streets | "uni_bs" %in% input$streets})
    
    observe({
      toggle(id = "barcodes_UNI_bs_options", condition = {
        !is.null(universals_bs_ready())})
    })
    
    observe({
      toggle(id = "barcode_error_uni_bs", condition = {
        is.null(universals_bs_ready())})
    })
    
    # if(is.null(universals_bs_ready())){
    #   show("barcode_error_uni_bs")
    # } else if(!is.null(universals_bs_ready()))  {
    #   hide("barcode_error_uni_bs")
    #   show("barcodes_UNI_bs_options")
    # }
  })  
  
  
  # MS1 append choices
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  
  observe({
    toggle(id = "barcodes_ms1", condition = {
      "intersected_ms1" %in% input$intersected_streets | "ms1" %in% input$streets})
    
    observe({
      toggle(id = "barcode_error_ms1", condition =
               {is.null(main_streets_ready()) }) 
    })
    
    observe({
      toggle(id = "barcodes_ms1_options", condition =
               {!is.null(main_streets_ready()) && length(main_streets_ready() %>% select(contains(c("ms1"))) %>% names()) >= 1})
    })
    

    
    # if(is.null(main_streets_ready())){
    #   show("barcode_error_ms1")
    #   hide("barcodes_ms1_options")
    # } else if(!is.null(main_streets_ready()) && length(main_streets_ready() %>% select(contains(c("ms1"))) %>% names()) > 1)  {
    #   hide("barcode_error_ms1")
    #   show("barcodes_ms1_options")
    # } else {
    #   show("barcode_error_ms1")
    # }
  })  

  
  # MS2 append choices
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  
  observe({
    toggle(id = "barcodes_ms2", condition = {
      "intersected_ms2" %in% input$intersected_streets | "ms2" %in% input$streets})
    
    observe({
      toggle(id = "barcode_error_ms2", condition =
               {is.null(main_streets_ready()) })
    })
    
    observe({
      toggle(id = "barcodes_ms2_options", condition =
               {!is.null(main_streets_ready()) && length(main_streets_ready() %>% select(contains(c("ms2"))) %>% names()) >= 1})
    })
    
    # if(is.null(main_streets_ready())){
    #   show("barcode_error_ms2")
    # } else if(length(main_streets_ready() %>% select(contains(c("ms2"))) %>% names()) >= 1)  {
    #   hide("barcode_error_ms2")
    #   show("barcodes_ms2_options")
    # } else {
    #   show("barcode_error_ms2")
    # }
    
  })  
  
  
  # BS1 append choices
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  
  observe({
    toggle(id = "barcodes_bs1", condition = {
      "intersected_bs1" %in% input$intersected_streets | "bs1" %in% input$streets})
    
    observe({
      toggle(id = "barcode_error_bs1", condition =
               {is.null(back_streets_ready()) })
    })
    
    observe({
      toggle(id = "barcodes_bs1_options", condition =
               {!is.null(back_streets_ready()) && length(back_streets_ready() %>% select(contains(c("bs1"))) %>% names()) >= 1})
    })
    
    # if(is.null(back_streets_ready())){
    #   show("barcode_error_bs1")
    # } else if(!is.null(back_streets_ready()) && (sum(str_detect(names(back_streets_ready()), "bs1")) >= 1) && (!input$intersected_same_BS1 | !input$same_BS1))  {
    #   hide("barcode_error_bs1")
    #   show("barcodes_bs1_options")
    # } else if(input$intersected_same_BS1 | input$same_BS1)  {
    #   hide("barcode_error_bs1")
    #   hide("barcodes_bs1_options")
    # } 
  })  
  
  # BS2 append choices
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  
  observe({
    toggle(id = "barcodes_bs2", condition = {
      "intersected_bs2" %in% input$intersected_streets | "bs2" %in% input$streets})
    
    observe({
      toggle(id = "barcode_error_bs2", condition =
               {is.null(back_streets_ready()) })
    })
    
    observe({
      toggle(id = "barcodes_bs2_options", condition =
               {!is.null(back_streets_ready()) && length(back_streets_ready() %>% select(contains(c("bs2"))) %>% names()) >= 1})
    })
    
    # if(is.null(back_streets_ready())){
    #   show("barcode_error_bs2")
    # } else if(!is.null(back_streets_ready()) && (sum(str_detect(names(back_streets_ready()), "bs2")) >= 1) && (!input$intersected_same_BS2 | !input$same_BS2))  {
    #   hide("barcode_error_bs2")
    #   show("barcodes_bs2_options")
    # } else if(input$intersected_same_BS2 | input$same_BS2)  {
    #   hide("barcode_error_bs2")
    #   hide("barcodes_bs2_options")
    # } 
  })  
  
  
  
  # observe({
  #   toggle(id = "barcodes_bs2", condition = {
  #     "intersected_bs2" %in% input$intersected_streets | "bs2" %in% input$streets})
  #   toggle(id = "barcode_error_bs2", condition = !{input$intersected_same_BS2 | input$same_BS2})
  #   bs2_options <- sum(str_detect(names(back_streets_ready()), "bs2")) >= 1
  #   toggle("barcodes_bs2_options", condition = bs2_options)
  # })  
  
  
  
  
  
  ## BS append choices
  # shows the options to the user after they have uploaded a bed file to what type of barcoding they can use for this street
  # observe({
  #   toggle(id = "barcodes_bs", condition = {c("intersected_bs1", "intersected_bs2") %in% input$intersected_streets | c("bs1", "bs2") %in% input$streets})
  #   
  #   if(is.null(back_streets_ready()) && input$same_BS1 == F | input$same_BS2 == F){
  #     
  #     # if("intersected_bs1" %in% input$intersected_streets | "bs1" %in% input$streets){
  #     #   show("barcode_error_bs1")
  #     # } else if ("intersected_bs2" %in% input$intersected_streets | "bs2" %in% input$streets){
  #     #   show("barcode_error_bs2")
  #     # }
  #     show("barcode_error_bs1")
  #     ## FIX The error does not disappear when same as Mainstreet is selected
  #   } else if(is.null(back_streets_ready()) && input$same_BS1 == T | input$same_BS2 == T) {
  #     hide("barcode_error_bs")
  #   } else if(!is.null(back_streets_ready()))  {
  #     hide("barcode_error_bs")
  #     if (length(back_streets_ready() %>% select(contains(c("bs1"))) %>% names()) >= 1) show("barcodes_bs1_options") else  hide("barcodes_bs1_options")
  #     if (length(back_streets_ready() %>% select(contains(c("bs2"))) %>% names()) >= 1) show("barcodes_bs2_options") else  hide("barcodes_bs2_options")
  #   }
  # }) 
  
  
  
  ### Prepare sequential OligoSTORM barcodes
  ### avoid numbers of OligoSTORM barcodes
  observeEvent(input$organism, {
    
    avoid_max <- nrow(streets())
    
    updateNumericInput(session, "avoid_from", max =  avoid_max)
    
    updateNumericInput(session, "avoid_to", max =  avoid_max-1, label = paste("To:", "(max = ", avoid_max,")"))
  })
  
  
  
  ### START DELETE
  # avoid_until <- eventReactive(input$append, {
  #   seq_len(input$avoid_until)
  # })
  ### END DELETE
  
  
  
  available_os_barcodes <- reactive({
    #input$append
    #isolate(
    setdiff(1:nrow(streets()), input$avoid_from:input$avoid_to)
    #)
  })
  
  
  
  ####################################
  # Create appended Oligopaints
  #-----------------------------------
  
  # generate multiplex barcodes based on whether the user wants to append to the mainstreet or backstreet
  multiplex_list_out <- eventReactive(input$append, {
    
    multiplex_list <- list()

    if (!is.null(input$append_streets_ms1) &&
        (input$append_streets_ms1 == "ofq" || input$append_streets_ms1 == "lambdaFISH")) {
      multiplex_list <- c(multiplex_list, unique(comb_ops()$id_ms1))
    } 
    
    if (!is.null(input$append_streets_ms2) &&
        (input$append_streets_ms2 == "ofq" || input$append_streets_ms2 == "lambdaFISH")) {
      multiplex_list <- c(multiplex_list, unique(comb_ops()$id_ms2))
    } 
    
    if (!is.null(input$append_streets_bs1) &&
        (input$append_streets_bs1 == "ofq" || input$append_streets_bs1 == "lambdaFISH")) {
      multiplex_list <- c(multiplex_list, unique(comb_ops()$id_bs1))
    } 
    
    if (!is.null(input$append_streets_bs2) &&
        (input$append_streets_bs2 == "ofq" || input$append_streets_bs2 == "lambdaFISH")) {
      multiplex_list <- c(multiplex_list, unique(comb_ops()$id_bs2))
    } 
    
    #load precomputed barcodes
    # sequences <- c("GGTCT","TGGTC","AGTCA","CGCTC")
    sequences <- initial_ofq_data()$Sequence
    
    barcodes_files <- list.files("./mpx", pattern = "mpx", full.names = T)
    
    # return(data.table(length(unique(multiplex_list))))
    if(!is.null(multiplex_list)){
      cycles <- 3
      
      file2open <- str_which(barcodes_files, paste0("mpx_", cycles, ".csv"))
      barcodes <- fread(barcodes_files[file2open]) %>% pull()
      print(barcodes)
      #create a while loop that runs
      # while(length(unique(multiplex_list)) > length(generate_multiplex_barcodes(cycles = cycles, sequences, min_hamming_distance = ceiling(cycles/2)))){
      while(length(unique(multiplex_list)) > length(barcodes) && cycles < 8){
      cycles <- cycles + 1
      file2open <- str_which(barcodes_files, paste0("mpx_", cycles, ".csv"))
      barcodes <- fread(barcodes_files[file2open]) %>% pull()
      }
      
      
      # barcodes <- generate_multiplex_barcodes(cycles = cycles, sequences , min_hamming_distance = ceiling(cycles/2))
      # randomize barcodes
      set.seed(42)
      barcodes_rand <- barcodes[sample(length(barcodes))]
      
      # create a data.table with the multiplex id and the barcode
      multiplex_df <- data.table(id = unique(multiplex_list), barcode = barcodes_rand[1:length(unique(multiplex_list))])
      
      return(multiplex_df)
    }
    
  })
  
  
  appended_oligopaints <- eventReactive(input$append, {
    ### create variable from reactive elements
    #avoid_sequences <- avoid_sequences()
    #avoid_until <- avoid_until()
    available_os_barcodes <- available_os_barcodes()
    
    
    
    ### import uploaded files
    if(input$filter_ops){
    comb_ops <- comb_ops_f()
    }else{
    comb_ops <- comb_ops()
    }
    ### Calculate how many barcodes are needed for each unique id of all streets
    # howmany_uni_ms <- length(unique(comb_ops$id_uni_ms)) 
    # howmany_uni_bs <- length(unique(comb_ops$id_uni_bs)) 
    # howmany_ms1 <- length(unique(comb_ops$id_ms1)) 
    # howmany_ms2 <- length(unique(comb_ops$id_ms2)) 
    # howmany_bs1 <- length(unique(comb_ops$id_bs1))
    # howmany_bs1 <- length(unique(comb_ops$id_bs2)) 
    
    
    # comb_ops_temp <- comb_ops %>% 
    #   select(contains("uni_bs")) %>% 
    #   group_by(across(contains("chr")), across(contains("id"))) %>% 
    #   group_nest()
    # 
    # comb_ops_temp
    
    # find if 
    

    
    # multiplex_df_out <- Reduce(left_join, multiplex_list)
    

    uni_pairs <- create_pairs(data = comb_ops, 
                              ms_input = input$append_streets_uni_ms, 
                              bs_input = input$append_streets_uni_bs, 
                              ms_id = "id_uni_ms", 
                              bs_id = "id_uni_bs", 
                              .streets = streets(), 
                              .toes = toes(), 
                              .available_os_barcodes = available_os_barcodes,
                              .available_multiplex_barcodes = multiplex_list_out(),
                              .matched_streets = matched_streets(),
                              input_ofq_bits = input_sec_ofq_reactive(), 
                              input_lambda_bits = input_sec_lambda_reactive(), 
                              bs_rc = input$uni_BS_rc)  
     
    if(!is.null(input$append_streets_uni_ms) && !is.null(input$append_streets_uni_bs)){
      uni_pairs_out <- right_join(tibble(uni_pairs$df), comb_ops, by = c("uni_ms" = "id_uni_ms", "uni_bs" = "id_uni_bs"))
    } else if(!is.null(input$append_streets_uni_ms) && is.null(input$append_streets_uni_bs)){
      uni_pairs_out <- right_join(tibble(uni_pairs$df), comb_ops, by = c("uni_ms" = "id_uni_ms"))
    } else if(is.null(input$append_streets_uni_ms) && !is.null(input$append_streets_uni_bs)){
      uni_pairs_out <- right_join(tibble(uni_pairs$df), comb_ops, by = c("uni_bs" = "id_uni_bs"))
    }
    
    
    
    street1_pairs <- create_pairs(data = comb_ops,
                                  ms_input = input$append_streets_ms1,
                                  bs_input = input$append_streets_bs1,
                                  ms_id = "id_ms1",
                                  bs_id = "id_bs1",
                                  .streets = streets(),
                                  .toes = toes(),
                                  .available_os_barcodes = if(!is.null(uni_pairs)) uni_pairs[["available_barcodes"]] else available_os_barcodes,
                                  .available_multiplex_barcodes = multiplex_list_out(),
                                  .matched_streets = matched_streets(),
                                  input_ofq_bits = input_sec_ofq_reactive(), 
                                  input_lambda_bits = input_sec_lambda_reactive(), 
                                  bs_rc = input$bs1_rc)
    
    if(!is.null(input$append_streets_ms1) && !is.null(input$append_streets_bs1)){
      street1_pairs_out <- right_join(tibble(street1_pairs$df), comb_ops, by = c("ms1" = "id_ms1", "bs1" = "id_bs1"))
    } else if(!is.null(input$append_streets_ms1) && is.null(input$append_streets_bs1)){
      street1_pairs_out <- right_join(tibble(street1_pairs$df), comb_ops, by = c("ms1" = "id_ms1"))
    } else if(is.null(input$append_streets_ms1) && !is.null(input$append_streets_bs1)){
      street1_pairs_out <- right_join(tibble(street1_pairs$df), comb_ops, by = c("bs1" = "id_bs1"))
    }

    street2_pairs <- create_pairs(data = comb_ops,
                                  ms_input = input$append_streets_ms2,
                                  bs_input = input$append_streets_bs2,
                                  ms_id = "id_ms2",
                                  bs_id = "id_bs2",
                                  .streets = streets(),
                                  .toes = toes(),
                                  .available_os_barcodes = if(!is.null(street1_pairs)) street1_pairs[["available_barcodes"]] else available_os_barcodes,
                                  .available_multiplex_barcodes = multiplex_list_out(),
                                  .matched_streets = matched_streets(),
                                  input_ofq_bits = input_sec_ofq_reactive(),
                                  input_lambda_bits = input_sec_lambda_reactive(), 
                                  bs_rc = input$bs2_rc)
    
    if(!is.null(input$append_streets_ms2) && !is.null(input$append_streets_bs2)){
      street2_pairs_out <- right_join(tibble(street2_pairs$df), comb_ops, by = c("ms2" = "id_ms2", "bs2" = "id_bs2"))
    } else if(!is.null(input$append_streets_ms2) && is.null(input$append_streets_bs2)){
      street2_pairs_out <- right_join(tibble(street2_pairs$df), comb_ops, by = c("ms2" = "id_ms2"))
    } else if(is.null(input$append_streets_ms2) && !is.null(input$append_streets_bs2)){
      street2_pairs_out <- right_join(tibble(street2_pairs$df), comb_ops, by = c("bs2" = "id_bs2"))
    }


    #list_out <- list(uni_pairs_out, street1_pairs)

    pairs_list <- list(uni = if(!is.null(input$append_streets_uni_ms)  || !is.null(input$append_streets_uni_bs))  uni_pairs_out,
                     ms = if(!is.null(input$append_streets_ms1)  || !is.null(input$append_streets_bs1))  street1_pairs_out,
                     bs = if(!is.null(input$append_streets_ms2)  || !is.null(input$append_streets_bs2))  street2_pairs_out) %>%
      compact()
    
    barcoded_df_out <- Reduce(left_join, pairs_list)
    
    
    # Step 1: Create the appended_oligopaint column using base R

      if (!is.null(input$append_streets_uni_ms)) {
        if (input$append_streets_uni_ms == "seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$uni_ms_toe, "tt")
        } else {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$uni_ms_street, "tt")
        }
      } else {
        barcoded_df_out$appended_oligopaint <- ""
      }
      
     if (!is.null(input$append_streets_ms2)) {
        if (input$append_streets_ms2 == "seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$ms2_toe, "tt")
        } else if (input$append_streets_ms2 == "toe_seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$ms2_street, "tt")
        } else if (input$append_streets_ms2 == "ofq") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$ms2_ofq_seq_primer, barcoded_df_out$ms2_ofq_barcode, "ttt")
        } else {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$ms2_street, "tt")
        }
      } else {
        barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, "")
      }
    
    
      if (!is.null(input$append_streets_ms1)) {
        if (input$append_streets_ms1 == "seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$ms1_toe, "tt")
        } else if (input$append_streets_ms1 == "toe_seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$ms1_street, "tt")
        } else if (input$append_streets_ms1 == "ofq") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$ms1_ofq_seq_primer, barcoded_df_out$ms1_ofq_barcode, "ttt")
        } else {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$ms1_street, "tt")
        }
      } else {
        barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,"")
      }
    
    
    barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$sequence,"tt")
    
      if (!is.null(input$append_streets_bs1)) {
        if (input$append_streets_bs1 == "seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs1_toe, "tt")
        } else if (input$append_streets_bs1 == "toe_seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs1_street, "tt")
        } else if (input$append_streets_bs1 == "ofq") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs1_ofq_seq_primer, barcoded_df_out$bs1_ofq_barcode, "ttt")
        } else {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs1_street, "tt")
        }
      } else {
        barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,"")
      }
      
      
      if (!is.null(input$append_streets_bs2)) {
        if (input$append_streets_bs2 == "seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs2_toe, "tt")
        } else if (input$append_streets_bs2 == "toe_seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs2_street, "tt")
        } else if (input$append_streets_bs2 == "ofq") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs2_ofq_seq_primer, barcoded_df_out$bs2_ofq_barcode, "ttt")
        } else {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,barcoded_df_out$bs2_street, "tt")
        }
      } else {
        barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,"")
      }
    
      if (!is.null(input$append_streets_uni_bs)) {
        if (input$append_streets_uni_bs == "seq_im") {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$uni_bs_toe)
        } else {
          barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint, barcoded_df_out$uni_bs_street)
        }
      } else {
        barcoded_df_out$appended_oligopaint <- paste0(barcoded_df_out$appended_oligopaint,"")
      }
    
    # barcoded_df_out %>% 
    # mutate(appended_oligopaint = str_pad(appended_oligopaint, round(max(str_count(appended_oligopaint))), side = "right", pad = "t"))
    return(barcoded_df_out %>% 
             mutate(appended_oligopaint = str_pad(appended_oligopaint, round(max(str_count(appended_oligopaint))), side = "right", pad = "t"))
)
    
    

    
    
    # START DELETE
    
    # mutate(appended_oligopaint = str_c(uni_ms_seq,'tt', ms1_seq,'tt',sequence,'tt', bs1_seq,'tt', uni_bs_seq)) %>% 
    #   mutate(appended_oligopaint = str_pad(appended_oligopaint, round(max(str_count(appended_oligopaint))), side = "right", pad = "t")) %>% 
    #   select(chr, start = start, end = end, sequence, Tm, on_target, off_target, repeat_seq, max_kmer, id_uni_ms, uni_ms_seq, id_ms1, ms1_seq, id_bs1, bs1_seq, id_uni_bs, uni_bs_seq, appended_oligopaint)
    
    
    
    # barcoded_df_out %>% 
    #   mutate(
    #     appended_oligopaints = str_c(
    #       if (!is.null(input$append_streets_uni_ms)) uni_ms_toe else "",
    #       if (!is.null(input$append_streets_ms1)) ms1_toe else "",
    #       if (!is.null(input$append_streets_bs1)) bs1_street else "",
    #       if (!is.null(input$append_streets_ms2)) ms2_street else "",
    #       if (!is.null(input$append_streets_bs2)) bs2_street else "",
    #       if (!is.null(input$append_streets_uni_bs)) uni_bs_street else "",
    #       sep = ""
    #     )
    #   )
    #   
     # barcoded_df_out %>% 
     #   select(contains("ofq_key")) 
     
     
     #df %>% select(any_of(c("year", "boo")))

    
    
    
    # how many OFQ sequences are needed
     #ofq_needed <- c(req(uni_pairs)$df$ms_ofq_key, req(uni_pairs)$df$bs_ofq_key,req(street1_pairs)$df$ms_ofq_key, req(street1_pairs)$df$bs_ofq_key,req(street2_pairs)$df$ms_ofq_key, req(street2_pairs)$df$bs_ofq_key)
     #ofq_max <- max(ofq_needed)
     #tibble(ofq_needed)
    # switch(ifelse(1>3, 3, 3), 
    #        invisible(pi), 
    #        pi, 
    #        "naaah")
    # 
    # 
    # ofq_barcodes <- generate_oligoFISSEQ_barcodes(cycles = 3, sequences = c("GGTCT","TGGTC","AGTCA","CGCTC"), different_last = 2)
    
    
    
    
    # bind_rows(if(!is.null(uni_pairs)) uni_pairs, if(!is.null(street1_pairs)) street1_pairs, if(!is.null(street2_pairs)) street2_pairs)
    
    
    # if((input$append_streets_uni_ms == "toe_seq_im" | input$append_streets_uni_ms == "seq_im") && (input$append_streets_uni_bs == "toe_seq_im" | input$append_streets_uni_bs == "seq_im")){
    #     uni_pairs <- comb_ops() %>%
    #       ungroup() %>%
    #       distinct(id_uni_ms, id_uni_bs) %>%
    #       rename_with(~gsub("id_uni_ms", "ms", .x, fixed = TRUE)) %>%
    #       rename_with(~gsub("id_uni_bs", "bs", .x, fixed = TRUE))
    #   }
    # 
    # if ((input$append_streets_uni_ms == "toe_seq_im" | input$append_streets_uni_ms == "seq_im") && (is.null(input$append_streets_uni_bs) | is.null(input$append_streets_uni_bs))){
    #     uni_pairs <- comb_ops() %>%
    #       ungroup() %>%
    #       distinct(id_uni_ms) %>%
    #       rename_with(~gsub("id_uni_ms", "ms", .x, fixed = TRUE))
    # }
    # 
    # if ((input$append_streets_uni_bs == "toe_seq_im" | input$append_streets_uni_bs == "seq_im") && (is.null(input$append_streets_uni_ms) | is.null(input$append_streets_uni_ms))){
    #   uni_pairs <- comb_ops() %>%
    #     ungroup() %>%
    #     distinct(id_uni_bs) %>%
    #     rename_with(~gsub("id_uni_bs", "bs", .x, fixed = TRUE))
    # }
    
    
    
    ### combine all OS pairs
    #df
    
    # 
    # os_pairs <- comb_ops %>%
    #   ungroup() %>%  
    #   distinct(ifelse(str_detect(names(final_UCE_noHOPs), "id_uni_ms"), across(contains("id_uni_ms")), NA), ifelse(str_detect(names(final_UCE_noHOPs), "id_uni_bs"), across(contains("id_uni_bs")), NA)) %>% 
    #   rename_with(~gsub("id_uni_ms", "ms", .x, fixed = TRUE)) %>% 
    #   rename_with(~gsub("id_uni_bs", "bs", .x, fixed = TRUE)) %>% 
    #   bind_rows(comb_ops %>%ungroup() %>%  
    #               distinct(across(contains("id_ms1")),across(contains("id_bs1"))) %>% 
    #               rename_with(~gsub("id_ms1", "ms", .x, fixed = TRUE)) %>% 
    #               rename_with(~gsub("id_bs1", "bs", .x, fixed = TRUE))) %>% 
    #   bind_rows(comb_ops %>%ungroup() %>%  
    #               distinct(across(contains("id_ms2")),across(contains("id_bs2"))) %>% 
    #               rename_with(~gsub("id_ms2", "ms", .x, fixed = TRUE)) %>% 
    #               rename_with(~gsub("id_bs2", "bs", .x, fixed = TRUE)) ) %>% 
    #   select(ms, bs)
    
    
    # 
    # 
    # ### Based on user's input define which barcode to append 
    # ops_list <- list(if(!is.null(universals_ms_ready())) universals_ms_ready(),
    #                  if(!is.null(universals_bs_ready())) universals_bs_ready(),
    #                  if(!is.null(main_streets_ready())) main_streets_ready(),
    #                  if(!is.null(back_streets_ready())) back_streets_ready()
    # ) %>% 
    #   compact() # removes NULL elements in the list
    # 
    # # combines all available elements in the list to one tibble
    # if(length(ops_list) == 1){
    #   ops_list[[1]] %>% tibble()
    # } else {
    #   ops_list %>% 
    #     reduce(left_join)
    # }
    
    ###### CREATING OS BARCODES
    # os_barcodes <- streets %>% 
    #   filter(!.$street %in% c(uni_msA,uni_msB,uni_msC, streets_HOPs$Street)) %>% 
    #   slice(1:(os_bs_2_noHOPs+os_bs_1_noHOPs+os_ms_1_noHOPs)) %>% 
    #   mutate(id = c(rep("ms1", os_ms_1_noHOPs), rep("bs1", os_bs_1_noHOPs), rep("bs2", os_bs_2_noHOPs)),
    #          toes = toes$toes[.$street_number]) 
    
    
    # closing of appended oligopaints  
    
    # FINAL barcoding
    
    ### FIX
    # df <- comb_ops %>% 
    #   ungroup() %>% 
    #   # universals
    #   mutate(bs_uni_num = 2, 
    #          bs_uni_seq = uni_bs,
    #          bs_uni_toe = toes$toes[2]) %>% 
    #   mutate( ms_uni_seq = case_when(
    #     str_detect(id_ms1, "A") ~ uni_msA,
    #     str_detect(id_ms1, "B") ~ uni_msB,
    #     str_detect(id_ms1, "C") ~ uni_msC)
    #   ) %>% 
    #   #select(-chr_num) %>% 
    #   arrange(chr, start) %>% 
    #   # ms2
    #   ungroup() %>% 
    #   group_by(chr, id_ms2) %>% 
    #   group_nest() %>% 
    #   bind_cols(
    #     ms2_num = OF_key_bc_selected$key,
    #     ms2_seq = OF_key_bc_selected$bc,
    #     ms_ttt = "ttt",
    #     ms_tt = "tt") %>% 
    #   unnest(cols = c(data)) %>% 
    #   # ms1
    #   ungroup() %>%
    #   arrange(chr, start) %>% 
    #   group_by(chr, id_ms1) %>% 
    #   group_nest() %>%
    #   bind_cols(
    #     ms1_num = os_barcodes[os_barcodes$id == "ms1",]$street_number,
    #     ms1_seq = os_barcodes[os_barcodes$id == "ms1",]$street,
    #     ms1_toe = os_barcodes[os_barcodes$id == "ms1",]$toes) %>% 
    #   unnest(cols = c(data)) %>% 
    #   # bs1
    #   ungroup() %>%
    #   arrange(chr, start) %>% 
    #   group_by(chr, id_bs1) %>% 
    #   nest() %>%
    #   bind_cols(
    #     bs1_num = os_barcodes[os_barcodes$id == "bs1",]$street_number,
    #     bs1_seq = os_barcodes[os_barcodes$id == "bs1",]$street,
    #     bs1_toe = os_barcodes[os_barcodes$id == "bs1",]$toes) %>% 
    #   unnest(cols = c(data)) %>% 
    #   # bs2
    #   ungroup() %>%
    #   arrange(chr, start) %>% 
    #   group_by(chr, id_bs2) %>% 
    #   nest() %>%
    #   bind_cols(
    #     bs2_num = os_barcodes[os_barcodes$id == "bs2",]$street_number,
    #     bs2_seq = os_barcodes[os_barcodes$id == "bs2",]$street,
    #     bs2_toe = os_barcodes[os_barcodes$id == "bs2",]$toes) %>% 
    #   unnest(cols = c(data))
    
    
    
  }, ignoreNULL = FALSE)
  
  
  
  
  
  ### test output area #####
  # output$appended_oligopaints <- renderText({
  #     avoid_until()
  # })
  
  
  
  # oligoSTORM_available <- eventReactive(input$append {
  #     oligoSTORM_available <- setdiff(1:nrow(streets()), seq_len(avoid_until()))
  #     oligoSTORM_available
  # }, ignoreNULL = FALSE)
  
  # oligoSTORM_barcodes <- eventReactive(input$append, {
  #     oligoSTORM_available <- oligoSTORM_available()
  # 
  # }, ignoreNULL = FALSE)
  ### Prepare oligoFISSEQ barcodes
  
  ### Prepare oligoSTORM barcodes
  
  
  ### Do all the appending to the provided .bed files of the chosen barcodes
  #isolate the imputed files so their are not constantly calculated
  
  
  ### show the final appended table!  
  # output$appended_uni <- DT::renderDataTable({
  #   appended_universals()
  # })
  
  # eventReactive(input$append, {
  # })

  
  # output$appended_oligopaints <- DT::renderDataTable({
  # 
  #   validate(
  #     need(input$append != 0, "Time to append!")
  #   )
  #   

  append_streets_uni_ms_reactive <- reactive({ input$append_streets_uni_ms }) %>% 
    bindEvent(input$append)
  append_streets_uni_bs_reactive <- reactive({ input$append_streets_uni_bs }) %>%
    bindEvent(input$append)
  append_streets_ms1_reactive <- reactive({ input$append_streets_ms1 }) %>%
    bindEvent(input$append)
  append_streets_ms2_reactive <- reactive({ input$append_streets_ms2 }) %>%
    bindEvent(input$append)
  append_streets_bs1_reactive <- reactive({ input$append_streets_bs1 }) %>%
    bindEvent(input$append)
  append_streets_bs2_reactive <- reactive({ input$append_streets_bs2 }) %>%
    bindEvent(input$append)
  

  # Create dynamic secs selectInput
  output$dynamic_secs <- renderUI({
    # Extract choices from initial_sec_data
    choices <- initial_sec_data()$bitName
    if (is.null(choices) || length(choices) == 0) {
      choices <- character(0)  # Handle the case where there are no choices
    }
    
    # Set the first element as the default selected value
    selected_value <- if (length(choices) > 0) choices[1] else NULL
    
    selectInput(
      inputId = "secs",
      label = tippy(
        "Select OligoSTORM secondary sequences:",
        "<strong>Here you can choose the secondary sequence you want to append to your bridges for OligoSTORM imaging. The order of the secondary sequences you choose here will be appended to the bridge sequences.</strong>"
      ),
      choices = choices,
      multiple = TRUE,
      selectize = TRUE,
      selected = selected_value
    )
  })
  
  
  # Get lambdaFISH and OFQ bits
  input_sec_lambda_reactive <- reactive({ initial_lambda_data() })
  input_sec_ofq_reactive <- eventReactive(input$append,{ initial_ofq_data() })
  
  # Get secondary sequences
  input_sec_reactive <- reactive({ input$secs })%>%
    bindEvent(input$append)
  initial_sec_data_reactive <- reactive({ initial_sec_data() })
  
  # Get activator secondary sequence
  input_actsec_reactive <- reactive({ input$sec405 }) %>%
    bindEvent(input$append)
  initial_actsec_data_reactive <- reactive({ initial_actsec_data() })
  
  #filter for each type of street appended
  appended_oligopaints_uni_ms <- reactive({
    appended_oligopaints() %>% select(contains("uni_ms"))
  })%>%
    bindEvent(input$append)
  
  appended_oligopaints_uni_bs <- reactive({
    appended_oligopaints() %>% select(contains("uni_bs"))
  })%>%
    bindEvent(input$append)
  
  appended_oligopaints_ms1 <- reactive({
    appended_oligopaints() %>% select(contains("ms1"))
  })%>%
    bindEvent(input$append)
  
  appended_oligopaints_ms2 <- reactive({
    appended_oligopaints() %>% select(contains("ms2"))
  })%>%
    bindEvent(input$append)
  
  appended_oligopaints_bs1 <- reactive({
    appended_oligopaints() %>% select(contains("bs1"))
  })%>%
    bindEvent(input$append)
  
  appended_oligopaints_bs2 <- reactive({
    appended_oligopaints() %>% select(contains("bs2"))
  })%>%
    bindEvent(input$append)
  
  
  uni_ms_appended_data <- appened_opsServer(id = 'uni_ms', .data = appended_oligopaints_uni_ms, type = append_streets_uni_ms_reactive, initial_actsec_data = initial_actsec_data_reactive, append_actsec = input$sec405, initial_sec_data = initial_sec_data_reactive, input_sec_data = input_sec_reactive)
  uni_bs_appended_data <- appened_opsServer(id = 'uni_bs', .data = appended_oligopaints_uni_bs, type = append_streets_uni_bs_reactive, initial_actsec_data = initial_actsec_data_reactive, append_actsec = input$sec405, initial_sec_data = initial_sec_data_reactive, input_sec_data = input_sec_reactive)
  ms1_appended_data <- appened_opsServer(id = 'ms1', .data = appended_oligopaints_ms1, type = append_streets_ms1_reactive, initial_actsec_data = initial_actsec_data_reactive, append_actsec = input$sec405, initial_sec_data = initial_sec_data_reactive, input_sec_data = input_sec_reactive)
  ms2_appended_data <- appened_opsServer(id = 'ms2', .data = appended_oligopaints_ms2, type = append_streets_ms2_reactive, initial_actsec_data = initial_actsec_data_reactive, append_actsec = input$sec405, initial_sec_data = initial_sec_data_reactive, input_sec_data = input_sec_reactive)
  bs1_appended_data <- appened_opsServer(id = 'bs1', .data = appended_oligopaints_bs1, type = append_streets_bs1_reactive, initial_actsec_data = initial_actsec_data_reactive, append_actsec = input$sec405, initial_sec_data = initial_sec_data_reactive, input_sec_data = input_sec_reactive)
  bs2_appended_data <- appened_opsServer(id = 'bs2', .data = appended_oligopaints_bs2, type = append_streets_bs2_reactive, initial_actsec_data = initial_actsec_data_reactive, append_actsec = input$sec405, initial_sec_data = initial_sec_data_reactive, input_sec_data = input_sec_reactive)

  
  bridges <- reactive({
    
    # make a list if the appended dataframes exist only
    list(uni_ms_bridges_toes = if (!is.null(input$append_streets_uni_ms)) uni_ms_appended_data() else NULL,
         uni_bs_bridges_toes = if (!is.null(input$append_streets_uni_bs)) uni_bs_appended_data() else NULL,
         ms1_bridges_toes = if (!is.null(input$append_streets_ms1)) ms1_appended_data() else NULL,
         ms2_bridges_toes = if (!is.null(input$append_streets_ms2)) ms2_appended_data() else NULL,
         bs1_bridges_toes = if (!is.null(input$append_streets_bs1)) bs1_appended_data() else NULL,
         bs2_bridges_toes = if (!is.null(input$append_streets_bs2)) bs2_appended_data() else NULL) %>% 
      compact()
  })
  
  amp_primers <- reactive({
    
    list(FWD_primers_out = if(!is.null(input$append_streets_uni_ms)) {
      uni_ms_appended_data() %>% 
        select(uni_ms, street_target_seq) %>% 
        mutate(uni_ms_FWD_primer_order = street_target_seq)
    } else {
      NULL
    },
        REV_primers_out = if(!is.null(input$append_streets_uni_bs)) {
          uni_bs_appended_data() %>% 
            select(uni_bs, street_target_seq) %>% 
            mutate(uni_bs_REV_primer_order = rc(street_target_seq),
                   uni_bs_REV_primerT7_order = str_c(T7 , rc(street_target_seq)))
    } else {
      NULL
    },
    lambda_primers_out = list(
      ms1_lambdaFISH = if(!is.null(input$append_streets_ms1)) {
        
        if(input$append_streets_ms1 == "lambdaFISH") {
          ms1_appended_data() %>% 
          select(contains("FWD_primer_lambdaFISH"), 
                 contains("REV_primer_lambdaFISH")) %>%
          distinct() %>% 
          pivot_longer(cols = -1, 
                       names_to = "lambda_sublibrary_primers", 
                       values_to = "lambda_primers_sequence") %>% 
          mutate(lambda_primers_sequence = rc(lambda_primers_sequence))
        } else {
          NULL
        }
    } else {
      NULL
    },
    if(!is.null(input$append_streets_ms2)) {
      
      if(input$append_streets_ms2 == "lambdaFISH") {
        ms2_appended_data() %>% 
          select(contains("FWD_primer_lambdaFISH"), 
                 contains("REV_primer_lambdaFISH")) %>%
          distinct() %>% 
          pivot_longer(cols = -1, 
                       names_to = "lambda_sublibrary_primers", 
                       values_to = "lambda_primers_sequence")%>% 
          mutate(lambda_primers_sequence = rc(lambda_primers_sequence))
      } else {
        NULL
      }
    } else {
      NULL
    },
    if(!is.null(input$append_streets_bs1)) {

      if(input$append_streets_bs1 == "lambdaFISH") {
        bs1_appended_data() %>% 
          select(contains("FWD_primer_lambdaFISH"), 
                 contains("REV_primer_lambdaFISH")) %>%
          distinct() %>% 
          pivot_longer(cols = -1, 
                       names_to = "lambda_sublibrary_primers", 
                       values_to = "lambda_primers_sequence")%>% 
          mutate(lambda_primers_sequence = rc(lambda_primers_sequence))
      } else {
        NULL
      }
    } else {
      NULL
    },
    if(!is.null(input$append_streets_bs2)) {
      
      if(input$append_streets_bs2 == "lambdaFISH") {
        bs2_appended_data() %>% 
          select(contains("FWD_primer_lambdaFISH"), 
                 contains("REV_primer_lambdaFISH")) %>%
          distinct() %>% 
          pivot_longer(cols = -1, 
                       names_to = "lambda_sublibrary_primers", 
                       values_to = "lambda_primers_sequence")%>% 
          mutate(lambda_primers_sequence = rc(lambda_primers_sequence))
      } else {
        NULL
      }
    } else {
      NULL
    }) %>% 
      compact()
    ) %>% 
      compact()
  })
  
  
  
  
 lambda_bridges_lib <- eventReactive(input$append,{
   req(bridges())
   
   if (!("lambdaFISH" %in% input$append_streets_ms1) &&
       !("lambdaFISH" %in% input$append_streets_ms2) &&
       !("lambdaFISH" %in% input$append_streets_bs1) &&
       !("lambdaFISH" %in% input$append_streets_bs2)) {
     return(NULL)
   } else {
     bridges() %>% 
       map(~ if(!is.null(.x)) {
         .x %>% 
           select(contains("bridge_lambdaFISH"))
       } else {
         NULL
       }) %>% 
       compact() %>% 
       bind_rows() %>% 
       pivot_longer(cols = everything(), 
                    names_to = "lambda_sublibrary_bridges", 
                    values_to = "lambda_bridges_lib") %>%
       select(lambda_bridges_lib)
   }

   
 })
 
 
 lambda_toes_lib <- eventReactive(input$append,{
   
   req(bridges())
   
   if (!("lambdaFISH" %in% input$append_streets_ms1) &&
       !("lambdaFISH" %in% input$append_streets_ms2) &&
       !("lambdaFISH" %in% input$append_streets_bs1) &&
       !("lambdaFISH" %in% input$append_streets_bs2)) {
     return(NULL)
   } else {
   bridges() %>% 
     map(~ if(!is.null(.x)) {
       .x %>% 
         select(contains("toe_lambdaFISH"))
     } else {
       NULL
     }) %>% 
     compact() %>% 
     bind_rows() %>% 
     # bind rows of all columes in one column
     pivot_longer(cols = everything(), 
                  names_to = "lambda_sublibrary_bridges", 
                  values_to = "lambda_toes_lib") %>%
     select(lambda_toes_lib)
   }
 })
   
  
  
  # toggle advanced barcode filtering settings
  shinyjs::onclick("toggleAdvancedAppend",
                   shinyjs::toggle(id = "advancedAppend", anim = TRUE, time = 0.3))  
  
  
  # resets all advanced barcode filtering fields in a div
  observeEvent(input$clear_barcode_filters, {
    shinyjs::reset("advanced_append") # this is the div names
  })
  
  observe({
    if (!is.null(input$selected_street)){
      df <- switch(input$selected_street,
                   "intersected_uni_ms" = intersected_universals_ms(),
                   "uni_ms" = universals_ms(),
                   "intersected_uni_bs" = intersected_universals_bs(),
                   "uni_bs" = universals_bs(),
                   "intersected_ms1" = intersected_main_street_1(),
                   "ms1" = main_street_1(),
                   "intersected_ms2" = intersected_main_street_2(),
                   "ms2" = main_street_2(),
                   "intersected_bs1" = intersected_back_street_1(),
                   "bs1" = back_street_1(),
                   "intersected_bs2" = intersected_back_street_2(),
                   "bs2" = back_street_2())
      
      # df is the data frame with the density_kb column
      if(!is.null(df)) {
        df <- df %>% 
          group_by(chr,across(contains("id"))) %>% 
          summarise(n = n(), size_kb = round((max(end) - min(start))/1000), density_kb = round(n/size_kb, digits =1))
        
        
        updateSliderInput(session, "filter_density",
                          min = min(df$density_kb, na.rm = TRUE),
                          max = max(df$density_kb, na.rm = TRUE),
                          label = paste("Oligopaints/Kb density threshold \n", 
                                        "min:", min(df$density_kb, na.rm = TRUE), "max:", max(df$density_kb, na.rm = TRUE)))
      }
    }

  })
  
  output$dynamic_street_select <- renderUI({
    if(input$op_new) {
      selectInput("selected_street", "Choose a street to threshold Oligopaints density:", choices = input$streets)
    } else if(input$op_intersected) {
      selectInput("selected_street", "Choose a street to threshold Oligopaints density:", choices = input$intersected_streets)
    }
  })
  
  
  
  
  ############# Secondary sequences definition ################
  # Initial data
  initial_sec_data <- reactiveVal(default_sec)
  
  
  # Render sec DataTable
  output$sec_seq_table <- renderDT({
    datatable(initial_sec_data(), editable = TRUE, options = list(
      dom = 't',
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = TRUE
    ), class = 'cell-border stripe')
  })
  
  # Observe changes in the DataTable
  observeEvent(input$sec_seq_table_cell_edit, {
    info <- input$sec_seq_table_cell_edit
    new_value <- toupper(info$value)  # Convert to uppercase
    data <- initial_sec_data()
    
    # Check if the new value contains only A, T, G, C, U
    if (grepl("^[ATGCU]+$", new_value)) {
      data[info$row, info$col] <- new_value
      initial_sec_data(data)  # Update the reactive value with new data
    } else {
      # Display modal dialog for invalid input
      showModal(modalDialog(
        title = "Invalid Input",
        "Only A, T, G, C, U characters are allowed. Your changes will not take effect.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
      # Re-render the DataTable to reflect the last valid data
      output$sec_seq_table <- renderDT({
        datatable(initial_sec_data(), editable = TRUE, options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollY = TRUE
        ), class = 'cell-border stripe')
      }, server = FALSE)  # Force client-side processing
    }
  })
  
  # Reset the data
  observeEvent(input$reset_sec, {
    initial_sec_data(default_sec) 
  })
  
  ############# 405 Secondary sequences definition ################
  # Initial data
  initial_actsec_data <- reactiveVal(default_actsec)
  
  
  # Render sec DataTable
  output$actsec_seq_table <- renderDT({
    datatable(initial_actsec_data(), editable = TRUE, options = list(
      dom = 't',
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = TRUE
    ), class = 'cell-border stripe')
  })
  
  # Observe changes in the DataTable
  observeEvent(input$actsec_seq_table_cell_edit, {
    info <- input$actsec_seq_table_cell_edit
    new_value <- toupper(info$value)  # Convert to uppercase
    data <- initial_actsec_data()
    
    # Check if the new value contains only A, T, G, C, U
    if (grepl("^[ATGCU]+$", new_value)) {
      data[info$row, info$col] <- new_value
      initial_actsec_data(data)  # Update the reactive value with new data
    } else {
      # Display modal dialog for invalid input
      showModal(modalDialog(
        title = "Invalid Input",
        "Only A, T, G, C, U characters are allowed. Your changes will not take effect.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
      # Re-render the DataTable to reflect the last valid data
      output$actsec_seq_table <- renderDT({
        datatable(initial_actsec_data(), editable = TRUE, options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollY = TRUE
        ), class = 'cell-border stripe')
      }, server = FALSE)  # Force client-side processing
    }
  })
  
  # Reset the data
  observeEvent(input$reset_actsec, {
    initial_actsec_data(default_actsec) 
  })
  
  
  
  ############# OligoFISSEQ BITs definition ################
  # Initial data
  initial_ofq_data <- reactiveVal(default_ofq)
  
  
  # Render ofq DataTable
  output$ofq_seq_table <- renderDT({
    datatable(initial_ofq_data(), editable = TRUE, options = list(
      dom = 't',
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = TRUE
    ), class = 'cell-border stripe')
  })
  
  # Observe changes in the DataTable
  observeEvent(input$ofq_seq_table_cell_edit, {
    info <- input$ofq_seq_table_cell_edit
    new_value <- toupper(info$value)  # Convert to uppercase
    data <- initial_ofq_data()
    
    # Check if the new value contains only A, T, G, C, U
    if (grepl("^[ATGCU]+$", new_value)) {
      data[info$row, info$col] <- new_value
      initial_ofq_data(data)  # Update the reactive value with new data
    } else {
      # Display modal dialog for invalid input
      showModal(modalDialog(
        title = "Invalid Input",
        "Only A, T, G, C, U characters are allowed. Your changes will not take effect.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
      # Re-render the DataTable to reflect the last valid data
      output$ofq_seq_table <- renderDT({
        datatable(initial_ofq_data(), editable = TRUE, options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          scrollY = TRUE
        ), class = 'cell-border stripe')
      }, server = FALSE)  # Force client-side processing
    }
  })
  
  # Reset the data
  observeEvent(input$reset_ofq, {
    initial_ofq_data(default_ofq) 
  })
  
  
  
  
  ############# lambdaFISH BITs definition ################
  # Initial data
  initial_lambda_data <- reactiveVal(default_lambda)
  

# Render lambda DataTable
output$lambda_seq_table <- renderDT({
  datatable(initial_lambda_data(), editable = TRUE, options = list(
    dom = 't',
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    autoWidth = TRUE,
    scrollX = TRUE,
    scrollY = TRUE
  ), class = 'cell-border stripe')
})

# Observe changes in the DataTable
observeEvent(input$lambda_seq_table_cell_edit, {
  info <- input$lambda_seq_table_cell_edit
  new_value <- toupper(info$value)  # Convert to uppercase
  data <- initial_lambda_data()
  
  # Check if the new value contains only A, T, G, C, U
  if (grepl("^[ATGCU]+$", new_value)) {
    data[info$row, info$col] <- new_value
    initial_lambda_data(data)  # Update the reactive value with new data
  } else {
    # Display modal dialog for invalid input
    showModal(modalDialog(
      title = "Invalid Input",
      "Only A, T, G, C, U characters are allowed. Your changes will not take effect.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    # Re-render the DataTable to reflect the last valid data
    output$lambda_seq_table <- renderDT({
      datatable(initial_lambda_data(), editable = TRUE, options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      ), class = 'cell-border stripe')
    }, server = FALSE)  # Force client-side processing
  }
})

# Reset the data
observeEvent(input$reset_lambda, {
  initial_lambda_data(default_lambda) 
})

# create  input$organism reactive
ops_organism <- reactive({
  input$organism
}) %>% 
  bindEvent(input$append)


  
  ########################################################################################################
  # Panel SELECT Downloads SERVER
  ###############-----------------------------------/3/-------------------------- ######################## 

  # OASIS HTML report
  output$OASISReport <- renderUI({ 
    validate(
      need(input$append != 0, "No Oligopaints appended yet... go back to 'Upload' / 'Barcodes' tabs.")
    )
    
    # Access the current value of the reactive expression bridges
    # params <- reactiveValues(bridges = NULL,
    #                          amp_primers = NULL)

    # Set up parameters to pass to Rmd document
    params <- list(bridges = bridges(),
                   amp_primers = amp_primers(),
                   valuebox_data = valuebox_data(),
                   valuebox_data_dist = valuebox_data_dist(),
                   ops_organism =  ops_organism()
                   )
    
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy('OASIS_report_2.Rmd', tempReport, overwrite = TRUE)
    
    rmarkdown_output <- rmarkdown::render(
      tempReport,
      params = list(
        bridges = bridges(),
        amp_primers = amp_primers(),
        valuebox_data = valuebox_data(),
        valuebox_data_dist = valuebox_data_dist(),
        ops_organism = ops_organism()
      ),
      envir = globalenv()#new.env(parent = globalenv())
    )
    includeMarkdown(knitr::knit(rmarkdown_output))
    
  })

  output$download_zip_files <- downloadHandler(
    filename = function() {
      paste0("OASIS_output_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create a temporary directory to store the files
      temp_dir <- tempdir()
      temp_files <- list()
      
      if ("ops_only" %in% input$download_files) {
        ops_file <- file.path(temp_dir, "intersected_oligopaints_NOappending.csv")
        if(input$filter_ops){
          comb_ops <- comb_ops_f() %>% 
            select(any_of(c("chr", "start", "end", "sequence", "Tm", "on_target", "off_target", "is_repeat", "prob", "max_kmer", "probe_strand")))
        }else{
          comb_ops <- comb_ops() %>% 
            select(any_of(c("chr", "start", "end", "sequence", "Tm", "on_target", "off_target", "is_repeat", "prob", "max_kmer", "probe_strand")))
        }
        write.csv(comb_ops, ops_file, row.names = FALSE)
        temp_files <- c(temp_files, ops_file)
      }
      
      if ("report" %in% input$download_files) {
        tempReport <- file.path(tempdir(), "OASIS_report.Rmd")
        file.copy('OASIS_report_2.Rmd', tempReport, overwrite = TRUE)
        
        rendered_report <- rmarkdown::render(
          tempReport,
          params = list(
            bridges = bridges(),
            amp_primers = amp_primers(),
            valuebox_data = valuebox_data(),
            valuebox_data_dist = valuebox_data_dist()
          ),
          envir = globalenv()
        )
        
        # Assuming the output from render is the path to the HTML file
        temp_files <- c(temp_files, rendered_report)
      }
      
      
      if ("oligopaints_order" %in% input$download_files && is.null(lambda_bridges_lib())) {
        oligo_file <- file.path(temp_dir, "OASIS_OPs_order.csv")
        write.csv(appended_oligopaints()["appended_oligopaint"], oligo_file, row.names = FALSE, col.names = FALSE)
        temp_files <- c(temp_files, oligo_file)
      }
      
      #create the lambdaFISH bridges order file
      if ("oligopaints_order" %in% input$download_files && !is.null(lambda_bridges_lib())) {
        lambda_bridges_file <- file.path(temp_dir, "lambda_bridges.csv")
        write.csv(lambda_bridges_lib()[["lambda_bridges_lib"]], lambda_bridges_file, row.names = FALSE)
        temp_files <- c(temp_files, lambda_bridges_file)
      }

      #create the lambdaFISH toes order file
      if ("oligopaints_order" %in% input$download_files && !is.null(lambda_toes_lib())) {
        lambda_toes_file <- file.path(temp_dir, "lambda_toes.csv")
        write.csv(lambda_toes_lib()[["lambda_toes_lib"]], lambda_toes_file, row.names = FALSE)
        temp_files <- c(temp_files, lambda_toes_file)
      }
      
      if ("bridges_download" %in% input$download_files) {
        for (name in names(bridges())) {
          bridge_file <- file.path(temp_dir, paste0(name, ".csv"))
          write.csv(bridges()[[name]], bridge_file, row.names = FALSE)
          temp_files <- c(temp_files, bridge_file)
        }
      }
      
      if ("amplification_download" %in% input$download_files) {
        for (name in names(amp_primers())) {
          amp_file <- file.path(temp_dir, paste0(name, ".csv"))
          write.csv(amp_primers()[[name]], amp_file, row.names = FALSE)
          temp_files <- c(temp_files, amp_file)
        }
      }
      
      # Create the zip file
      zip(file, files = temp_files, flags = "-j")  # -j to junk the paths
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server) #display.mode="showcase" # this will highlight the code running upon interaction with the app
# runApp(appDir="~/Google Drive/HMS/general_lab/Vutara_related/scripts/appending/appending/appending/", port = 5050, host="0.0.0.0")
