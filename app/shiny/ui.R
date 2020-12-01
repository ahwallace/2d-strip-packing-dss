
navbarPage(
  title = "2D Strip Packing DSS",
  id = "packR",
  selected = "Instances",
  theme = shinytheme("yeti"),
  collapsible = TRUE,
  
  # Include shinyDashboard css
  useShinydashboard(),
  # Include shinyalerts (popups)
  useShinyalert(),
  
  tabPanel(
    title = "Instances",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            width = 12,
            align = "center",
            h4("Choose Problem Instance"),
            hr()
          )
        ),
        pickerInput(
          inputId = "author",
          label = "Search instances by author",
          choices = list(
            Benchmarks = papers,
            Personal = personal
          ),
          choicesOpt = list(
            subtext = year_published
          ),
          options = list(
            size = 7
          )
        ),
        selectInput(
          inputId = "instance",
          label = "Choose problem instance to analyse",
          choices = NULL,
          selectize = FALSE,
          size = 10
        ),
        br(),
        fileInput(
          inputId = "file_input",
          label = "Upload",
          multiple = FALSE, 
          placeholder = "Problem Instance",
          accept = c(".csv")
        ),
        textInput(
          inputId = "save_as",
          label = "Save instance as",
          placeholder = "Name of Instance"
        ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            helpText("Note: File name must end with '.csv'", style = "color: #00a3cb"),
            actionButton(
              inputId = "save",
              label = "Save",
              icon = icon("save")
            ),
            actionButton(
              inputId = "delete",
              label = "Delete",
              icon =icon("trash-alt")
            )
          )
        )
      ),
      mainPanel(
        wellPanel(
          fluidRow(
            column(
              width = 12, 
              align = "center",
              h4("Summary of Problem Instance"),
              hr()
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              DT::dataTableOutput("instance_summary")
            ),
            column(
              width = 6,
              pickerInput(
                inputId = "sort_data_vis",
                label = "Sort instance for visualisation",
                choices = c(
                  "Default Order",
                  "Decreasing Width", 
                  "Decreasing Height", 
                  "Decreasing Area",
                  "Increasing Width",
                  "Increasing Height",
                  "Increasing Area"
                ),
                options = list(
                  size = 7
                )
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::valueBox(
            value = textOutput("strip_width"),
            subtitle = "Strip Width",
            icon = icon("arrows-alt-h")
          ),
          shinydashboard::valueBox(
            value = textOutput("optimal"),
            subtitle = "Optimal Height",
            icon = icon("thumbs-up")
          ),
          shinydashboard::valueBox(
            value = textOutput("n_items"),
            subtitle = "Number of Items",
            icon = shiny::icon("clipboard-list")
          )
        ),
        uiOutput(
          outputId = "display_instance"
        ),
        fluidRow(
          column(
            width = 12,
            align = "center",
            br(),
            actionButton(
              inputId = "go_to_algo",
              label = "Next Step",
              icon = icon("arrow-right"),
              style = "color: #fff; background-color: #f14c19"
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Algorithms",
    sidebarLayout(
      sidebarPanel(
        h4("Packing Algorithms"),
        radioGroupButtons(
          inputId = "heuristic",
          label = "Choose heuristic algorithm",
          choices = c("Constructive", "Improved Best-Fit"),
          direction = "horizontal",
          justified = TRUE
        ),
        pickerInput(
          inputId = "sort_data",
          label = "Sort instance (heuristic only)",
          choices = c(
            "Default Order" = NA,
            "Decreasing Width" = "dec.W", 
            "Decreasing Height" = "dec.H", 
            "Decreasing Area" = "dec.A",
            "Increasing Width" = "inc.W",
            "Increasing Height" = "inc.H",
            "Increasing Area" = "inc.A"
          ),
          options = list(
            size = 7
          )
        ),
        br(),
        p("Metaheuristics"),
        radioGroupButtons(
          inputId = "meta_heuristic",
          label = "Choose metaheuristic",
          choices = c("None Selected", "Genetic Algorithm", "Simulated Annealing")
        ),
        br(),
        p("Maximum Algorithm Run Time"),
        fluidRow(
          column(
            width = 3,
            numericInput(
              inputId = "max_time_h", 
              label = "Hours", 
              value = 0, 
              min = 0, 
              step = 1,
              width = "100%"
            )
          ),
          column(
            width = 1,
            p(":")
          ),
          column(
            width = 3,
            numericInput(
              inputId = "max_time_m", 
              label = "Minutes", 
              value = 0, 
              min = 0, 
              max = 59,
              step = 1,
              width = "100%"
            )
          ),
          column(
            width = 1,
            p(":")
          ),
          column(
            width = 3,
            numericInput(
              inputId = "max_time_s", 
              label = "Seconds", 
              value = 0, 
              min = 0, 
              max = 59,
              step = 1,
              width = "100%"
            )
          ),
          fluidRow(
            column(
              width = 12,
              helpText("Warning: Metaheuristics may take a long time to run, we advise setting a time limit.", style = "color: #f14c19")
            )
          )
        )
      ),
      mainPanel(
        wellPanel(
          tabsetPanel(
            id = "algo_params",
            tabPanel(
              title = "Overview",
              wellPanel(
                fluidRow(
                  h4("Input Summary")
                ),
                br(),
                fluidRow(
                  column(
                    width = 6,
                    align = "center",
                    p("Algorithm"),
                    verbatimTextOutput("selected_algorithm")
                  ),
                  column(
                    width = 6,
                    align = "center",
                    p("Instance"),
                    verbatimTextOutput("selected_instance")
                  )
                )
              )
            ),
            tabPanel(
              title = "GA Parameters",
              wellPanel(
                fluidRow(
                  h4("Hybrid Genetic Algorithm Parameters")
                ),
                br(),
                fluidRow(
                  column(
                    width = 3,
                    p("General"),
                    hr(),
                    numericInput(
                      inputId = "ga_pop_size",
                      label = "Population size",
                      value = 30,
                      min = 10,
                      max = 200,
                      step = 10,
                      width = "50%"
                    ),
                    numericInput(
                      inputId = "ga_n_gen",
                      label = "Number of generations",
                      value = 50,
                      min = 10,
                      max = 1000,
                      step = 10,
                      width = "50%"
                    )
                  ),
                  column(
                    width = 3,
                    p("Selection"),
                    hr(),
                    radioButtons(
                      inputId = "ga_selection_type", 
                      label = "Type", 
                      choiceNames = c("Roulette Wheel", "Tournament"),
                      choiceValues = c("roulette", "tournament")
                    ),
                    uiOutput("tournament_size")
                  ),
                  column(
                    width = 3,
                    p("Crossover"),
                    hr(),
                    radioButtons(
                      inputId = "ga_crossover_type",
                      label = "Type",
                      choiceNames = c("Partially Matched", "Order"),
                      choiceValues = c("pmx", "ox")
                    ),
                    sliderInput(
                      inputId = "ga_crossover_prob",
                      label = "Crossover probability",
                      min = 0,
                      max = 1,
                      value = 0.7,
                      step = 0.01,
                      round = FALSE
                    )
                  ),
                  column(
                    width = 3,
                    p("Mutation"),
                    hr(),
                    sliderInput(
                      inputId = "ga_mutation_prob",
                      label = "Mutation probability",
                      min = 0,
                      max = 0.1,
                      value = 0.01,
                      step = 0.001
                    )
                  )
                )
              )
            ),
            tabPanel(
              title = "SA Parameters",
              wellPanel(
                fluidRow(
                  h4("Simulated Annealing Parameters")
                ),
                br(),
                fluidRow(
                  column(
                    width = 4,
                    p("Parameters"),
                    hr(),
                    fluidRow(
                      column(
                        width = 6,
                        numericInput(
                          inputId = "sa_n_iter",
                          label = "Number of iterations", 
                          min = 10,
                          max = 1000,
                          value = 100,
                          step = 10
                        )
                      ),
                      column(
                        width = 6,
                        numericInput(
                          inputId = "sa_init_temp",
                          label = "Initial temperature",
                          min = 1,
                          value = 100,
                          step = 20
                        )
                      )
                    ),
                    helpText("Note: Initial temperature must be sufficiently large", style = "color: #00a3cb"),
                    radioButtons(
                      inputId = "sa_schedule_type",
                      label = "Schedule Type",
                      choiceNames = c("Geometric", "Linear", "Logarithmic", "Lundy Mees"),
                      choiceValues = c("geometric", "linear", "logarithmic", "lundymees")
                    ),
                    uiOutput(
                      outputId = "sa_cooling_param_output"
                    ),
                    sliderInput(
                      inputId = "sa_temp_lb",
                      label = "Temperature lower bound",
                      min = 0.0001,
                      max = 10,
                      value = 10
                    ),
                    sliderInput(
                      inputId = "sa_epoch",
                      label = "Length of epoch",
                      min = 0, 
                      ma = 10,
                      value = 10,
                      step = 0.1
                    )
                  ),
                  column(
                    width = 8,
                    p("Temperature"),
                    hr(),
                    wellPanel(
                      shinycssloaders::withSpinner(
                        plotOutput("sa_temp_plot"),
                        hide.ui = FALSE
                      )
                    ),
                    helpText("Temperature profile depends on:", style = "color: #00a3cb"),
                    helpText("1. Schedule type", style = "color: #00a3cb"),
                    helpText("2. Cooling parameter", style = "color: #00a3cb"),
                    helpText("3. Number of iterations", style = "color: #00a3cb"),
                    helpText("4. Initial Temperature", style = "color: #00a3cb")
                  )
                )
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              align = "center",
              actionButton(
                inputId = "run_algorithm",
                label = "Run Algorithm",
                style = "color: #fff; background-color: #f14c19"
              )
            )
          )
        ),
        wellPanel(
          tabsetPanel(
            id = "run_panels",
            tabPanel(
              title = "Information",
              wellPanel(
                br(),
                fluidRow(
                  column(
                    width = 4,
                    shinydashboard::valueBox(
                      value = textOutput("run_time"),
                      subtitle = "Run Time",
                      icon = shiny::icon("hourglass-half"),
                      width = NULL
                    )
                  ),
                  column(
                    width = 4,
                    shinydashboard::valueBox(
                      value = textOutput("packing_height"),
                      subtitle = "Packed Height",
                      icon = shiny::icon("arrows-alt-v"),
                      width = NULL
                    )
                  ),
                  column(
                    width = 4,
                    shinydashboard::valueBox(
                      value = textOutput("optimal_height_run"),
                      subtitle = "Optimal Height",
                      icon = shiny::icon("thumbs-up"),
                      width = NULL
                    )
                  )
                ),
                hr(),
                fluidRow(
                  shinycssloaders::withSpinner(
                    plotlyOutput("run_plot"),
                    hide.ui = FALSE
                  )
                )
              )
            ),
            tabPanel(
              title = "Additional",
              wellPanel(
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    h5("Layout"),
                    hr(),
                    DT::dataTableOutput(
                      outputId = "layout_info"
                    )
                  )
                )
              )
            ),
            tabPanel(
              title = "Metaheuristic",
              wellPanel(
                br(),
                fluidRow(
                  textOutput(
                    outputId = "meta_stopping_criteria"
                  )
                ),
                hr(),
                uiOutput(
                  outputId = "meta_plots"
                )
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Compare",
    wellPanel(
      fluidRow(
        column(
          width = 12,
          align = "center", 
          radioGroupButtons(
            inputId = "n_compare", 
            label = "Number of Algorithms to Compare", 
            choices = c("Two", "Three")
          )
        )
      ),
      uiOutput(
        outputId = "compare_inputs"
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          p("Note: Metaheuristic parameters inherited from 'Algorithms' section", style = "color: #00a3cb")
        )
      ),
      fluidRow(
        column(
          width = 4,
          p("Maximum Run Time Per Algorithm"),
          fluidRow(
            column(
              width = 3,
              numericInput(
                inputId = "compare_max_time_h", 
                label = "Hours", 
                value = 0, 
                min = 0, 
                step = 1,
                width = "100%"
              )
            ),
            column(
              width = 1,
              p(":")
            ),
            column(
              width = 3,
              numericInput(
                inputId = "compare_max_time_m", 
                label = "Minutes", 
                value = 0, 
                min = 0, 
                max = 59,
                step = 1,
                width = "100%"
              )
            ),
            column(
              width = 1,
              p(":")
            ),
            column(
              width = 3,
              numericInput(
                inputId = "compare_max_time_s", 
                label = "Seconds", 
                value = 0, 
                min = 0, 
                max = 59,
                step = 1,
                width = "100%"
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "center",
          actionButton(
            inputId = "run_compare",
            label = "Compare",
            style = "color: #fff; background-color: #f14c19"
          )
        )
      )
    ),
    wellPanel(
      tabsetPanel(
        id = "compare_output",
        tabPanel(
          title = "Information",
          uiOutput(
            outputId = "compare_outputs"
          )
        ),
        tabPanel(
          title = "Plots",
          uiOutput(
            outputId = "compare_plots"
          )
        )
      )
    )
  ),
  tabPanel(
    title = "About",
    
    column(
      width = 4,
      h4("Help"),
      br(),
      p("The application is structured so that a user moves through the application sequentially."),
      p("The user starts by either selecting or uploading data in the 'Instances' section. Once an instance has been chosen, they can move onto the 'Algorithms' 
            section where they are given the choice either a heuristic algorithm or a combination of heuristic 
            and metaheuristic to use on their chosen problem instance. If a user selects a metaheuristic, they will need to input the 
            required parameters."),
      p("Once a user has chosen the algorithms and parameters, they can run
            the algorithm on the selected instance. A plot of the packing layout as well as other information such as the hieght of the packing solution is shown to the user"),
      p("A user may also wish to compare different algorithms against eachother on their chosen instance in the 'Compare' section."),
      hr(),
      h4("Data"),
      p("Data uploaded by users must have the following structure:"),
      p("1. First line: width of the strip, optimal height 
        (If optimal height is unknown - optimal height value must equal -999)"),
      p("2. Other lines: reference, height, width"),
      p("3. Data must be in .csv format"),
      img(src='instance_format.png', height="60%", width="60%", align = "center"),
      hr()
    ),
    column(
      width = 4,
      h4("Overview"),
      br(),
      p("Strip packing is a complex combinatorial problem with a lot of real world applications. 
            This software focuses on orthogonal, fixed orientation, two-dimensional strip packing."),
      br(),
      p("The aim of this application is to assist researchers in studying various heuristic 
            and metaheuristic implementations."),
      hr(),
      h4("Heuristic Algorithms"),
      br(),
      p("Heuristic algorithms are simple rule-based packing algorithms that allow users to 
            quickly obtain good quality (although not necessarily optimal) solutions to packing problems."),
      p("Constructive heuristic scoring rule for the case when h₁ ≥ h₂:"),
      img(src='ch.png', height="100%", width="100%", align = "center"),
      br(),
      p("Improved Best-Fit heuristic scoring rule:"),
      img(src='bf.png', height="100%", width="100%", align = "center"),
      br(),
      p("Both the Constructive and Improved Best-Fit heuristics rely on a two-stage packing 
            procedure whereby a suitable space is found and thereafter an item to fit into the
            space is chosen basd on the above mentioned scoring rules. The item with with the highest 
            score for the chosen space will be placed."),
      p("Both methods rely on the bottom-left criteria where an item will be placed in the lowest,
            left most available space. If no item can fit into the bottom-left space, the next lowest, left-most
            space is evauted until all items are packed."),
      p("Items in the the Constructive and Improved Best-Fit heuristics are placed next to the 
            tallest neighbour. That is, the item is placed on the side of the space with the largest height."),
      hr()
    ),
    column(
      width = 4,
      h4("Metaheuristic Algorithms"),
      br(),
      p("Metaheuristic algorithms combine a heursitic algorithm with a search for the best 
            initial order of items. The metaheuristic searhces a set of item permutations while 
            the heuristic algorithm is used to decode the item permutations into packing solutions. The best 
            packing solution of the heuristic algorithm given the initial ordering of items found by the metaheuristic is returned."),
      br(),
      h5("Hybrid Genetic Algorithm"),
      p("Is based on the process of natural selection where an initial population of solutions
            taking the form of packing permutations are evolved over time using the 
            gentics-inspired operators of crossover and mutation."),
      br(),
      h5("Hybrid Simulated Annealing"),
      p("Is based on the physical process of annealing. In broad terms, 
            the simulated annealing (SA) algorithm typically commences with 
            an initialisation of the starting parameters – a random initial 
            solution and a relatively largestarting temperature. 
            At each iteration, a neighbour of the current solution is 
            randomly generated by some suitably defined mechanism. 
            The objective function is evaluated atthis new point and thereafter 
            the decision to accept or reject this as the new solution is performed 
            probabilistically. Whilst running through the SA algorithm iteratively 
            the temperature is cooled."),
      hr(),
      h4("Acknowledgments"),
      br(),
      p("This software was developed by Aidan Wallace and Leslie Wu, under the supervision of 
        Dr. R. Georgina Rakotonirainy, in partial fulfilment of an Honours in Statistical 
        Sciences from the University of Cape Town."),
      hr(),
      h4("References"),
      br(),
      p("1.", tags$a(href = "https://doi.org/10.1287/opre.1040.0109", "E. K. Burke, G. Kendall, and G. Whitwell, A new placement heuristic for the orthogonal stock-cutting problem, Operations Research, 52 (2004), pp. 655–671")),
      p("4.", tags$a(href = "https://doi.org/10.1023/A:1012590107280", "E. Hopper and B. C. Turton, A review of the application of meta-heuristic algorithms to 2d strip packing problems, Artificial Intelligence Review, 16 (2001), pp. 257–300.")),
      p("3.", tags$a(href = "https://doi.org/10.1016/j.cor.2016.11.024", "S. C. Leung, D. Zhang, and K. M. Sim, A two-stage intelligent search algorithm for the two-dimensional strip packing problem, European Journal of Operational Research, 215 (2011), pp. 57–69.")),
      p("5.", tags$a(href = "http://hdl.handle.net/10019.1/104853", "R. G. Rakotonirainy, Metaheuristic solution of the two-dimensional strip packing problem, PhD thesis, Stellenbosch University, Stellenbosch, 2018.")),
      p("2.", tags$a(href = "https://doi.org/10.1016/j.cor.2016.11.024", "L. Wei, Q. Hu, S. C. Leung, and N. Zhang, An improved skyline based heuristic for the 2d strip packing problem and its efficient implementation, Computers & Operations Research, 80 (2017), pp. 113–127.")),
      hr()
    )
  )
)
