
function(input, output, session){
  
  # Show instructions at startup
  popup_1 <- reactiveVal()
  observe({
    shinyalert(
      title = "Welcome",
      text = ("Welcome to our strip packing application. This introduction will help guide you through the app. \n
              Click 'next' to go through the tutorial or 'skip' if this is not your first time. \n"),
      showConfirmButton = TRUE,
      confirmButtonText = "next", 
      showCancelButton = TRUE,
      cancelButtonText = "skip",
      callbackR = function(x){if(x){popup_1(1)}}
    )
  })
  
  popup_2 <- reactiveVal()
  observe({
    if(!is.null(popup_1())){
      shinyalert(
        title = "Instances",
        text = ("The first step of the application will be choosing a problem instance (dataset) to analyse. This can be done in the 'Instances' section. \n
                On the left hand side of the page, you will be able to choose between a number of pre loaded benchmark instances (from past papers in literature) or upload your own problem instance (for more information see 'About'). \n
                A summary of your chosen instance is shown on the right hand side. Information includes a summary of the items, the strip width, optimal height (if known) and the number of items in the instance. \n
                At the bottom of the page, a table of the items is displayed as well as a plot of the items in the instance (items plotted next to each other). The plot items can be arranged according to increasing/decreasing width, height or area."),
        confirmButtonText = "next",
        callbackR = function(x){popup_2(1)}
      )
    }
    popup_1(NULL)
  })
  
  popup_3 <- reactiveVal()
  observe({
    if(!is.null(popup_2())){
      shinyalert(
        title = "Algorithms",
        text = ("Once a problem instance has been chosen, you can move onto the 'Algorithms' section by either using the navigation bar on top or the 'next' button at the bottom of the page. \n
                In this section, you will be presented with a number of strip packing algorithms to choose from. You can also set the paramters of the various algorithms from this page. \n
                Once an algorithm and appropriate parameters are chosen, you can run the algorithm on the problem instance. After the algorithm has run, the output of the packing algorithm is shown at the bottom of the page."),
        confirmButtonText = "next",
        callbackR = function(x){popup_3(x)}
      )
    }
    popup_2(NULL)
  })
  
  observe({
    if(!is.null(popup_3())){
      shinyalert(
        title = "Compare",
        text = ("The application also allows you to compare packing algorithms (heuristic and metaheuristic combinations or ordered heuristics) against each other in the 'Compare' section. \n
                The 'Compare' section allows you to run different combinations of algorithms on the same problem instance and easily compare outputs."),
        confirmButtonText = "get started"
      )
    }
    popup_3(NULL)
  })
  
  # Read in problem instance
  instance <- eventReactive(input$instance, {
    instance <- read.csv(paste(instances_path, input$author, input$instance, sep = "/"), header = F, sep = ",")
    colnames(instance) <- c("id", "height", "width")
    instance
  })
  
  # Number of items in instance
  num_items <- reactive({nrow(instance()[-1, ])})
  
  # Strip width of instance
  strip_width <- reactive({as.numeric(instance()[1, 1])})
  
  # Optimal height of instance
  instance_opt <- reactive({
    opt <- as.numeric(instance()[1, 2])
    ifelse(opt == -999, "Unknown", opt)
  })
  
  # Update the problem instance choices based on the author and paper selected
  instance_choices <- reactive({
    list.files(paste(instances_path, input$author, sep = "/"), pattern = ".csv")
  })
  
  # Update choices
  observeEvent(input$author, {
    # Convert vector to list
    show <- lapply(instance_choices(), function(x){x})
    
    # Get names for files
    if(!identical(instance_choices(), character(0))){
      names(show) <- unlist(lapply(strsplit(instance_choices(), split = "\\."), function(x){x[1]}))
    }
    updateSelectInput(
      session,
      inputId = "instance",
      choices = show
    )
  })
  
  # Get name for outputs
  pretty_name <- reactive({
    if(!identical(instance_choices(), character(0))){
      names <- list.files(paste(instances_path, input$author, sep = "/"), pattern = ".csv")
      names_clean <- unlist(lapply(strsplit(names, split = "\\."), function(x){x[1]}))
      names_clean[which(names == input$instance)]
    }
  })
  
  observeEvent(input$go_to_algo, {
    updateNavbarPage(session, "strip_packer", selected = "Algorithms")
  })
  
  # Update text input when a file is uploaded
  observeEvent(input$file_input, {
    updateTextInput(session, "save_as", value = input$file_input$name)
  })
  
  # Check to see if file is in correct format and save
  observeEvent(input$save, {
    req(input$file_input)
    req(input$save_as)
    
    if(endsWith(input$save_as, ".csv")){
      
      # Current file names in Personal
      current_names <- list.files(paste(instances_path, "Personal", sep = "/"), pattern = ".csv")
      
      # Read in .csv
      upload <- NULL
      try(upload <- read.csv(input$file_input$datapath, header = F, sep = ","), 
           silent = TRUE)
      
      if(is.null(upload)){
        shinyalert(
          title = "Error",
          text = "Failed to upload file. Unknown error occured",
          closeOnClickOutside = TRUE,
          type = "error"
        )
      }
      
      validate(need(!is.null(upload), "Failed to upload file"))
      
      show_modal_spinner(text = "Saving...")
      
      if(ncol(upload) != 3){
        remove_modal_spinner()
        # Check for number of columns
        shinyalert(
          title = "Error",
          text = "File does not contain three columns",
          closeOnClickOutside = TRUE, 
          type = "error"
        )
      } else if(length(unique(upload[-1, 1])) != nrow(upload[-1, ])){
        remove_modal_spinner()
        # Check to see if id column correct
        shinyalert(
          title = "Error",
          text = "Duplicate items in ID column",
          closeOnClickOutside = TRUE, 
          type = "error"
        )
      } else if(any(upload[-1, 3] > upload[1, 1])){
        remove_modal_spinner()
        # Check to see if any item widths greater than strip width
        shinyalert(
          title = "Error",
          text = "Some item widths are greater than the strip width",
          closeOnClickOutside = TRUE, 
          type = "error"
        )
      } else {
        # Check current names
        if(input$save_as %in% current_names){
          remove_modal_spinner()
          shinyalert(
            title = "Warning",
            text = paste("File name", input$save_as, "already exists. Do you want to overwrite file?"),
            type = "warning", 
            showCancelButton = TRUE, 
            showConfirmButton = TRUE,
            callbackR = function(x){overwrite(x)}
          )
        } else{
          # Test file to check if it runs correctly
          names(upload) <- c("id", "height", "width")
          test <- NULL
          try(test <- heuristic(upload[-1, ], upload[1, 1], ch_score, NA)$height, 
              silent = TRUE)
          
          remove_modal_spinner()
          
          if(is.null(test)){
            shinyalert(
              title = "Error",
              text = "Failed to upload file. Incorrect file specification. Please check contents 
              of file to ensure that they match the specified file format (see 'About' for more details).",
              closeOnClickOutside = TRUE,
              type = "error"
            )
          }
          
          validate(need(!is.null(test), "Failed to upload file"))
          
          # Add upload
          write.table(upload, file = paste(instances_path, "Personal", input$save_as, sep = "/"), row.names = FALSE, col.names = FALSE, sep = ",")
          updatePickerInput(session, inputId = "author", selected = "Jakobs")
          updatePickerInput(session, inputId = "author", selected = "Personal")
          
          shinyalert(
            title = "Success",
            text = "File uploaded successfully", 
            closeOnClickOutside = TRUE,
            type = "success"
          )
        }
      }
      
    }else{
      shinyalert(
        title = "Error",
        text = "File does not end with '.csv'. Please make sure file is in .csv format",
        closeOnClickOutside = TRUE, 
        type = "error"
      )
    }
  })
  
  # Show blank results plot on startup
  observe({
    if(input$run_algorithm == 0){
      output$run_plot <- renderPlotly({
        p <- ggplot(NULL) + 
          labs(x = "Instance Width", y = "Height", title = "Layout:") + 
          theme_classic() + 
          theme(plot.background = element_rect(fill = "#fafafa"),
                panel.background = element_rect(fill = "#fafafa"),
                plot.title = element_text(size = 14, face = "bold", hjust = 0),
                axis.title = element_text(size = 12))
        ggplotly(p)
      })
    }
  })
  
  # Overwrite file if needed
  overwrite <- reactiveVal()
  observe({
    req(overwrite())
    if(overwrite()){
      show_modal_spinner(text = "Saving...")
      # Test file to check if it runs correctly
      upload <- read.csv(input$file_input$datapath, header = F, sep = ",")
      names(upload) <- c("id", "height", "width")
      test <- NULL
      try(test <- heuristic(upload[-1, ], upload[1, 1], ch_score, NA)$height, 
          silent = TRUE)
      
      remove_modal_spinner()
      
      if(is.null(test)){
        shinyalert(
          title = "Error",
          text = "Failed to upload file. Unknown error occured",
          closeOnClickOutside = TRUE,
          type = "error"
        )
      }
      
      validate(need(!is.null(test), "Failed to upload file"))
      
      # Add upload
      write.table(upload, 
                  file = paste(instances_path, "Personal", input$save_as, sep = "/"), 
                  row.names = FALSE, col.names = FALSE, sep = ",")
      updatePickerInput(session, inputId = "author", selected = "Jakobs")
      updatePickerInput(session, inputId = "author", selected = "Personal")
      
      shinyalert(
        title = "Success",
        text = "File uploaded successfully", 
        closeOnClickOutside = TRUE,
        type = "success"
      )
    }
    overwrite(NULL)
  })
  
  # Provide warning when deleting file
  delete <- reactiveVal()
  observeEvent(input$delete, {
    if(input$author == "Personal"){
      shinyalert(
        title = "Warning",
        text = paste("Are you sure you want to delete", input$instance),
        showCancelButton = TRUE,
        showConfirmButton = TRUE,
        type = "warning",
        callbackR = function(x){delete(x)}
      )
    } else {
      shinyalert(
        title = "Error",
        text = "Only 'Personal' files may be deleted",
        closeOnClickOutside = TRUE,
        type = "error"
      )
    }
  })
  
  # Detele file
  observe({
    req(delete())
    if(delete()){
      file.remove(paste(instances_path, input$author, input$instance, sep = "/"))
      updatePickerInput(session, inputId = "author", selected = "Jakobs")
      updatePickerInput(session, inputId = "author", selected = "Personal")
    }
    delete(NULL)
  })
  
  # Update infoBoxes
  observe({
    if(is.null(input$instance)){
      output$strip_width <- renderText({"0"})
      output$n_items <- renderText({"0"})
      output$optimal <- renderText({"0"})
      output$optimal_height_run <- renderText({"0"})
    } else {
      output$strip_width <- renderText({strip_width()})
      output$n_items <- renderText({num_items()})
      output$optimal_height_run <- renderText({instance_opt()})
      output$optimal <- renderText(instance_opt())
    }
  })
  
  # Summary of instance
  output$instance_summary <- DT::renderDataTable({
    d <- data.frame(
      "Min" = c(min(instance()[-1, 2]), min(instance()[-1, 3])),
      "1st Quartile" = c(quantile(instance()[-1, 2], probs = 0.25), quantile(instance()[-1, 3], probs = 0.25)),
      "Median" = c(median(instance()[-1, 2]), median(instance()[-1, 3])),
      "3rd Quartile" = c(quantile(instance()[-1, 2], probs = 0.75), quantile(instance()[-1, 3], probs = 0.75)),
      "Max" = c(max(instance()[-1, 2]), max(instance()[-1, 3])),
      "Mean" = c(mean(instance()[-1, 2]), mean(instance()[-1, 3])),
      row.names = c("Height", "Width"),
      check.names = FALSE
    )
    
    DT::datatable(d, options = list(scrollX = TRUE, searching = FALSE, pageLength = 2, dom = "t"))
  })
  
  observe({
    req(input$instance)
    # Sort items for visualisation
    sorting_vis <- reactive({
      switch(input$sort_data_vis,
             "Decreasing Width" = list("width", TRUE),
             "Decreasing Height" = list("height", TRUE),
             "Decreasing Area" = list("area", TRUE),
             "Increasing Width" = list("width", FALSE),
             "Increasing Height" = list("height", FALSE),
             "Increasing Area" = list("area", FALSE)
      )
    })
    
    if(num_items() <= 500){
      # Show table for instance with 500 or less items
      if(num_items() <= 50){
        # Show plot for instances with 50 or less items
        output$display_instance <- renderUI({
          wellPanel(
            tabsetPanel(
              id = "instance_vis",
              tabPanel(
                title = "Table",
                br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("instance_vis_table"),
                  proxy.height = "50px"
                )
              ),
              tabPanel(
                title = "Plot",
                plotlyOutput("instance_vis_plot")
              )
            )
          )
        })
        
        # Plot problem instance if n_items <= 50
        output$instance_vis_plot <- renderPlotly(
          if(input$sort_data_vis == "Default Order"){
            visualise_instance(isolate(instance()[-1, ]), isolate(pretty_name()))
          } else{
            visualise_instance(sort_fun(isolate(instance()[-1, ]), sorting_vis()), isolate(pretty_name()))
          }
        )
      } else {
        
        output$display_instance <- renderUI({
          wellPanel(
            tabsetPanel(
              id = "instance_vis",
              tabPanel(
                title = "Table",
                br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("instance_vis_table"),
                  proxy.height = "50px"
                )
              ),
              tabPanel(
                title = "Plot",
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    br(),
                    p("Will only plot instances with less than 50 items")
                  )
                )
              )
            )
          )
        })
        
      }
      
      output$instance_vis_table <- DT::renderDataTable(
        DT::datatable(
          data = isolate(t(instance()[-1, ])),
          colnames = "",
          rownames = c("Item Id", "Height", "Width"),
          options = list(scrollX = TRUE, searching = FALSE, pageLength = 3, dom = "t")
        )
      )
    } else {
      output$display_instance <- renderUI({
        wellPanel(
          fluidRow(
            column(
              width = 12,
              align = "center",
              p("Will only display instances with less than 500 items")
            )
          )
        )
      })
    }
  })
  
  # Update ui to show metaheuristic controls
  observe({
    if(input$meta_heuristic == "Genetic Algorithm"){
      updateTabsetPanel(session, "algo_params", selected = "GA Parameters")
    } else if(input$meta_heuristic == "Simulated Annealing"){
      updateTabsetPanel(session, "algo_params", selected = "SA Parameters")
    } else if(input$meta_heuristic == "None Selected"){
      updateTabsetPanel(session, "algo_params", selected = "Overview")
    }
    
    if(input$meta_heuristic == "None Selected"){
      output$selected_algorithm <- renderPrint({
        cat("Heuristic: ", input$heuristic, "\n",
            "Order: ", sort_to_words(input$sort_data), "\n",
            "", sep = "")
      })
    } else {
      output$selected_algorithm <- renderPrint({
        cat("Heuristic: ", input$heuristic, "\n",
            "Metaheuristic: ", input$meta_heuristic, "\n",
            "", "\n", sep = "")
      })
    }
  })
  
  output$packing_height <- renderText({"0"})
  output$run_time <- renderText({"0 s"})
  
  output$selected_instance <- renderPrint({
    cat("Name: ", pretty_name(), "\n",
        "Strip Width: ", strip_width(), "\n",
        "# of Items: ", num_items(), "\n")
  })
  
  # Show tournament size if tournament selection chosen
  output$tournament_size <- renderUI({
    if(input$ga_selection_type == "roulette"){
      return()
    }
    fluidRow(
      column(
        offset = 1,
        width = 12,
        numericInput(
          inputId = "tournament_size", 
          label = "Tournament size", 
          value = round(0.25*isolate(input$ga_pop_size)), 
          min = 3, 
          max = isolate(input$ga_pop_size)-1,
          step = 1, 
          width = "50%"
        ),
        helpText ("Tournament size < Population size", style = "color: #00a3cb")
      )
    )
  })
  
  # Update SA cooling parameter based on schedule type
  output$sa_cooling_param_output <- renderUI({
    if(input$sa_schedule_type == "geometric"){
      sliderInput(
        inputId = "sa_cooling_param",
        label = "Cooling parameter",
        min = 0.7,
        max = 0.999,
        value = 0.9,
        step = 0.01
      )
    } else if(input$sa_schedule_type == "linear"){
      sliderInput(
        inputId = "sa_cooling_param",
        label = "Cooling parameter",
        min = 0.5*(input$sa_init_temp - input$sa_temp_lb)/input$sa_n_iter,
        max = 2*(input$sa_init_temp - input$sa_temp_lb)/input$sa_n_iter,
        value = (input$sa_init_temp - input$sa_temp_lb)/input$sa_n_iter,
        step = 0.01
      )
    } else if(input$sa_schedule_type == "logarithmic"){
      sliderInput(
        inputId = "sa_cooling_param",
        label = "Cooling parameter",
        min = 1,
        max = 3,
        value = 1.1,
        step = 0.01
      )
    } else{
      # Lundy Mees
      sliderInput(
        inputId = "sa_cooling_param",
        label = "Cooling parameter",
        min = 0.0001,
        max = 0.05,
        value = 0.01,
        step = 0.0001
      )
    }
  })
  
  observe({
    # Update temperature lower bound based in initial temperature
    updateSliderInput(
      session, 
      "sa_temp_lb", 
      max = input$sa_init_temp/50,
      value = 0.1 * (input$sa_init_temp/50),
      step = 0.001
    )
    
    # Update length of epoch based on instance size
    updateSliderInput(
      session, 
      "sa_epoch", 
      max = round(10 * num_items()),
      value = round(0.5 * num_items()),
      step = 1
    )
  })
  
  # Values for temperature plot
  temp_plot <- reactive({
    req(input$sa_n_iter)
    req(input$sa_init_temp)
    req(input$sa_cooling_param)
    req(input$sa_schedule_type)
    
    temp <- c(input$sa_init_temp)
    for(i in 2:input$sa_n_iter){
      temp[i] <- temperature(temp[1], temp[i-1], input$sa_cooling_param, i, input$sa_schedule_type)
    }
    # temp
    data.frame(temp = temp, iter = 1:input$sa_n_iter)
  })
  
  # Temperature plot
  output$sa_temp_plot <- renderPlot({
    ggplot(temp_plot(), aes(x = iter, y = temp)) + geom_line(size = 1.4, colour = "#f5735f") + 
      labs(x = "Iteration", y = "Temperature", title = "Temperature Profile") + 
      theme_classic() + 
      theme(plot.background = element_rect(fill = "#fafafa"),
            panel.background = element_rect(fill = "#fafafa"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12))
  })
  
  # Show algorithm output
  observeEvent(input$run_algorithm, {
    
    if(is.null(input$instance)){
      shinyalert(
        title = "Warning",
        text = "Please make sure to select a problem instance",
        type = "warning",
        closeOnClickOutside = TRUE
      )
    }
    
    req(input$instance)
    
    if(is.na(input$max_time_h) | is.na(input$max_time_m) | is.na(input$max_time_s)){
      shinyalert(
        title = "Warning",
        text = "Please make sure to set a valid time limit (set all values to 0 for no time limit)"
      )
    }
    
    req(input$max_time_h)
    req(input$max_time_m)
    req(input$max_time_s)
    
    score_algo <- reactive({
      switch(isolate(input$heuristic),
             "Constructive" = ch_score,
             "Improved Best-Fit" = bf_score
      )
    })
    
    if(input$meta_heuristic == "Genetic Algorithm"){
      req(input$ga_pop_size)
      req(input$ga_n_gen)
      req(input$ga_selection_type)
      req(input$ga_crossover_type)
      req(input$ga_crossover_prob)
      req(input$ga_mutation_prob)
      
      if(input$ga_selection_type == "tournament"){
        req(input$tournament_size)
      }
      
      # Add progress bar to indicate calculation taking place
      show_modal_progress_line(text = paste("Calculating: Generation 1 of", input$ga_n_gen))
      
      run_time <- system.time(
        hga <- hybrid_ga(data = instance()[-1, ], 
                         strip_width = strip_width(), 
                         instance_optimal = instance_opt(), 
                         FUN = score_algo(), 
                         population_size = input$ga_pop_size,
                         n_generations = input$ga_n_gen,
                         crossover_prob = input$ga_crossover_prob,
                         mutation_prob = input$ga_mutation_prob,
                         selection = input$ga_selection_type,
                         crossover = input$ga_crossover_type,
                         stop_time = c(input$max_time_h, input$max_time_m, input$max_time_s),
                         tournament_size = input$tournament_size)
      )
      
      best_order <- instance()[-1, ]
      if(is.vector(hga$best_order)){
        best_order <- best_order[hga$best_order, ]
      } else{
        best <- which.min(hga$best_fitness)
        best_order <- best_order[hga$best_order[, best], ]
      }
      
      out <- heuristic(best_order, strip_width(), score_algo(), init_sort = NA)
      layout <- out$layout
      
      stop_criteria_ga <- switch(hga$stop_criteria,
                              "time" = "Time limit reached",
                              "optimal" = "Optimal height found",
                              # "improvement" = "No improvement in average population fitness over previous two generations",
                              "generations" = "Maximum number of generations reached")
      
      output$meta_stopping_criteria <- renderText(paste("Stopping Criterion:", stop_criteria_ga))
      
      # Plot optimisation paths if number generations >= 3
      if(length(hga$best_fitness) >= 3){
        
        output$meta_plots <- renderUI({
          fluidRow(
            tabsetPanel(
              id = "meta_optimisation_plots",
              tabPanel(
                title = "Best Fitness",
                plotOutput(
                  outputId = "meta_best_plot"
                )
              ),
              tabPanel(
                title = "Average Fitness",
                plotOutput(
                  outputId = "meta_avg_plot"
                )
              )
            )
          )
        })
        
        output$meta_best_plot <- renderPlot({
          df <- data.frame(bf = hga$best_fitness, gen = 1:length(hga$best_fitness))
          
          ggplot(df, aes(x = gen, y = bf)) + geom_line(size = 1.4, colour = "#f5735f") + 
            labs(x = "Generation", y = "Best Packed Height", title = "Hybrid GA Optimisation Path") + 
            theme_classic() + 
            theme(plot.background = element_rect(fill = "#fafafa"),
                  panel.background = element_rect(fill = "#fafafa"),
                  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 12))
        })
        
        output$meta_avg_plot <- renderPlot({
          df <- data.frame(mf = hga$mean_fitness, gen = 1:length(hga$mean_fitness))
          
          ggplot(df, aes(x = gen, y = mf)) + geom_line(size = 1.4, colour = "#f5735f") + 
            labs(x = "Generation", y = "Average Packed Height", title = "Hybrid GA Optimisation Path") + 
            theme_classic() + 
            theme(plot.background = element_rect(fill = "#fafafa"),
                  panel.background = element_rect(fill = "#fafafa"),
                  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 12))
        })
      } else{
        output$meta_plots <- renderUI({return()})
      }
      
      # Remove progress bar to indicate calculation finished
      remove_modal_progress()
      
    } else if(input$meta_heuristic == "Simulated Annealing"){
      req(input$sa_n_iter)
      req(input$sa_init_temp)
      req(input$sa_schedule_type)
      req(input$sa_cooling_param)
      req(input$sa_temp_lb)
      req(input$sa_epoch)
      
      # Add progress bar to indicate calculation taking place
      show_modal_progress_line(text = paste("Calculating: Iteration 1 of", input$sa_n_iter))
      
      run_time <- system.time(
        sa <- hybrid_sa(data = instance()[-1, ], 
                        strip_width = strip_width(), 
                        instance_optimal = instance_opt(), 
                        FUN = score_algo(), 
                        init_temp = input$sa_init_temp, 
                        alpha = input$sa_cooling_param, 
                        schedule = input$sa_schedule_type, 
                        epoch_management = input$sa_epoch, 
                        stop_iter = input$sa_n_iter, 
                        stop_time = c(input$max_time_h, input$max_time_m, input$max_time_s), 
                        lb_temp = input$sa_temp_lb)
      )
      
      best_order <- instance()[-1, ]
      best_order <- best_order[sa$best_order, ]
      
      out <- heuristic(best_order, strip_width(), score_algo(), init_sort = NA)
      layout <- out$layout
      
      stop_criteria_sa <- switch(sa$stop_criteria,
                                 "time" = "Time limit reached",
                                 "optimal" = "Optimal height found",
                                 "temperature" = "Temperature lower bound reached",
                                 "iterations" = "Maximum number of iterations reached")
      
      output$meta_stopping_criteria <- renderText(paste("Stopping Criterion:", stop_criteria_sa))
      
      # Plot optimisation paths if number generations >= 3
      if(length(sa$fitness_progress) >= 3){
        
        output$meta_plots <- renderUI({
          fluidRow(
            wellPanel(
              plotOutput(
                outputId = "meta_best_plot"
              )
            )
          )
        })
        
        output$meta_best_plot <- renderPlot({
          df <- data.frame(fp = sa$fitness_progress, iter = 1:length(sa$fitness_progress))
          
          ggplot(df, aes(x = iter, y = fp)) + geom_line(size = 1.4, colour = "#f5735f") + 
            labs(x = "Iteration", y = "Average Packed Height", title = "Hybrid SA Fitness Progress") + 
            theme_classic() + 
            theme(plot.background = element_rect(fill = "#fafafa"),
                  panel.background = element_rect(fill = "#fafafa"),
                  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  axis.title = element_text(size = 12))
        })
        
      } else{
        output$meta_plots <- renderUI({return()})
      }
      
      # Remove progress bar to indicate calculation finished
      remove_modal_progress()
      
    } else if(input$meta_heuristic == "None Selected"){
      
      # Add spinner to indicate calculation taking place
      show_modal_spinner(text = "Processing...")
      
      run_time <- system.time(
        out <- heuristic(instance()[-1, ], strip_width(), score_algo(), init_sort = input$sort_data)
      )
      
      layout <- out$layout
      
      output$meta_plots <- renderUI({return()})
      output$meta_stopping_criteria <- renderText("")
      
      # Remove spinner to indicate calculation finished
      remove_modal_spinner()
    }
    
    output$run_time <- renderText({paste(round(run_time[3], 3), "s", sep = " ")})
    
    output$packing_height <- renderText({out$height})
    output$optimal_height_run <- renderText({instance_opt()})
    
    output$layout_info <- DT::renderDataTable(
      DT::datatable(
        data = cbind(1:nrow(out$layout), out$layout),
        colnames = c("Packing Order", "Bottom-Left X", "Bottom-Left Y", "Top-Right X", "Top-Right Y", "Item Id"),
        rownames = FALSE,
        options = list(searching = FALSE, pageLength = 10, 
                       columnDefs = list(list(className = 'dt-center', targets = 0:5)))
      )
    )
    
    output$run_plot <- renderPlotly({
      vis_output(layout, isolate(input$heuristic), isolate(input$meta_heuristic))
    })
  
  })
  
  # Number of algorithms to compare UI
  output$compare_inputs <- renderUI({
    
    if(input$n_compare == "Two"){
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = "compare_heuristic_1",
            label = "Heuristic:",
            choices = c("Constructive", "Improved Best-Fit")
          ),
          pickerInput(
            inputId = "compare_sort_1",
            label = "Sort instance (heuristic only)",
            choices = list(
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
          pickerInput(
            inputId = "compare_meta_1",
            label = "Metaheuristic:",
            choices = c("None Selected", "Genetic Algorithm", "Simulated Annealing")
          )
        ),
        column(
          width = 6,
          pickerInput(
            inputId = "compare_heuristic_2",
            label = "Heuristic:",
            choices = c("Constructive", "Improved Best-Fit"), 
            selected = "Improved Best-Fit"
          ),
          pickerInput(
            inputId = "compare_sort_2",
            label = "Sort instance (heuristic only)",
            choices = list(
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
          pickerInput(
            inputId = "compare_meta_2",
            label = "Metaheuristic:",
            choices = c("None Selected", "Genetic Algorithm", "Simulated Annealing")
          )
        )
      )
      
    } else if (input$n_compare == "Three"){
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "compare_heuristic_1",
            label = "Heuristic:",
            choices = c("Constructive", "Improved Best-Fit")
          ),
          pickerInput(
            inputId = "compare_sort_1",
            label = "Sort instance (heuristic only)",
            choices = list(
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
          pickerInput(
            inputId = "compare_meta_1",
            label = "Metaheuristic:",
            choices = c("None Selected", "Genetic Algorithm", "Simulated Annealing")
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "compare_heuristic_2",
            label = "Heuristic:",
            choices = c("Constructive", "Improved Best-Fit"), 
            selected = "Improved Best-Fit"
          ),
          pickerInput(
            inputId = "compare_sort_2",
            label = "Sort instance (heuristic only)",
            choices = list(
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
          pickerInput(
            inputId = "compare_meta_2",
            label = "Metaheuristic:",
            choices = c("None Selected", "Genetic Algorithm", "Simulated Annealing")
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "compare_heuristic_3",
            label = "Heuristic:",
            choices = c("Constructive", "Improved Best-Fit")
          ),
          pickerInput(
            inputId = "compare_sort_3",
            label = "Sort instance (heuristic only)",
            choices = list(
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
          pickerInput(
            inputId = "compare_meta_3",
            label = "Metaheuristic:",
            choices = c("None Selected", "Genetic Algorithm", "Simulated Annealing")
          )
        )
      )
    }
    
  })
  
  output$compare_outputs <- renderUI({
    
    if(input$n_compare == "Two"){
      wellPanel(
        fluidRow(
          column(
            width = 6,
            verbatimTextOutput(
              outputId = "compare_info_1"
            )
          ),
          column(
            width = 6,
            verbatimTextOutput(
              outputId = "compare_info_2"
            )
          )
        )
      )
    } else{
      wellPanel(
        fluidRow(
          column(
            width = 4,
            verbatimTextOutput(
              outputId = "compare_info_1"
            )
          ),
          column(
            width = 4,
            verbatimTextOutput(
              outputId = "compare_info_2"
            )
          ),
          column(
            width = 4,
            verbatimTextOutput(
              outputId = "compare_info_3"
            )
          )
        )
      )
    }
    
  })
  
  output$compare_plots <- renderUI({
    
    if(input$n_compare == "Two"){
      wellPanel(
        tabsetPanel(
          id = "output_compare_plots",
          tabPanel(
            title = "Slot 1",
            plotlyOutput(
              outputId = "compare_plot_1"
            )
          ),
          tabPanel(
            title = "Slot 2",
            plotlyOutput(
              outputId = "compare_plot_2"
            )
          )
        )
      )
    } else{
      wellPanel(
        tabsetPanel(
          id = "output_compare_plots",
          tabPanel(
            title = "Slot 1",
            plotlyOutput(
              outputId = "compare_plot_1"
            )
          ),
          tabPanel(
            title = "Slot 2",
            plotlyOutput(
              outputId = "compare_plot_2"
            )
          ),
          tabPanel(
            title = "Slot 3",
            plotlyOutput(
              outputId = "compare_plot_3"
            )
          )
        )
      )
    }
    
  })
  
  observeEvent(input$run_compare, {
    
    if(is.null(input$instance)){
      shinyalert(
        title = "Warning",
        text = "Please make sure to select a problem instance",
        type = "warning",
        closeOnClickOutside = TRUE
      )
    }
    
    req(input$instance)
    
    if(is.na(input$compare_max_time_h) | is.na(input$compare_max_time_m) | is.na(input$compare_max_time_s)){
      shinyalert(
        title = "Warning",
        text = "Please make sure to set a valid time limit (set all values to 0 for no time limit)"
      )
    }
    
    req(input$compare_max_time_h)
    req(input$compare_max_time_m)
    req(input$compare_max_time_s)
    
    # Compare 2 or 3 algorithms
    n <- ifelse(input$n_compare == "Two", 2, 3)
    
    # Progress bar
    show_modal_progress_line(text = paste("Calculating: Algorithm 1 of", n))
    
    ############### Slot 1 ###############
    # Heuristic score function to use for slot 1
    score_compare_1 <- reactive({
      switch(isolate(input$compare_heuristic_1),
             "Constructive" = ch_score,
             "Improved Best-Fit" = bf_score,
             "Improved Constructive" = improved_score)
    })
    
    # Slot 1 results
    if(input$compare_meta_1 == "None Selected"){
      # Heuristic for slot 1
      run_time_compare_1 <- system.time(
        out_compare_1 <- heuristic(instance()[-1, ], 
                                   strip_width(), 
                                   score_compare_1(), 
                                   init_sort = input$compare_sort_1)
      )
      
      stop_crit_1 <- "NULL"
      
    } else if(input$compare_meta_1 == "Genetic Algorithm"){
      # GA for slot 1
      req(input$ga_n_gen)
      
      run_time_compare_1 <- system.time(
        hga_compare_1 <- hybrid_ga(instance()[-1, ], 
                                   strip_width(), 
                                   instance_opt(), 
                                   score_compare_1(), 
                                   input$ga_pop_size,
                                   input$ga_n_gen,
                                   input$ga_crossover_prob,
                                   input$ga_mutation_prob,
                                   input$ga_selection_type,
                                   input$ga_crossover_type,
                                   c(input$compare_max_time_h, input$compare_max_time_m, input$compare_max_time_s),
                                   input$tournament_size,
                                   1)
      )
      
      # Best order from GA
      best_order_compare_1 <- instance()[-1, ]
      if(is.vector(hga_compare_1$best_order)){
        # If GA terminates on first iteration
        best_order_compare_1 <- best_order_compare_1[hga_compare_1$best_order, ]
      } else{
        # If GA terminates after first iteration
        best_compare_1 <- which.min(hga_compare_1$best_fitness)
        best_order_compare_1 <- best_order_compare_1[hga_compare_1$best_order[, best_compare_1], ]
      }
      
      out_compare_1 <- heuristic(best_order_compare_1, strip_width(), score_compare_1(), init_sort = NA)
      stop_crit_1 <- stop_to_words(hga_compare_1$stop_criteria)
      
    } else if(input$compare_meta_1 == "Simulated Annealing"){
      # SA for slot 1
      
      req(input$sa_n_iter)
      req(input$sa_init_temp)
      req(input$sa_schedule_type)
      req(input$sa_cooling_param)
      req(input$sa_temp_lb)
      req(input$sa_epoch)
      
      run_time_compare_1 <- system.time(
        sa_compare_1 <- hybrid_sa(instance()[-1, ], 
                                  strip_width(), 
                                  instance_opt(), 
                                  score_compare_1(), 
                                  input$sa_init_temp, 
                                  input$sa_cooling_param, 
                                  input$sa_schedule_type, 
                                  input$sa_epoch, 
                                  input$sa_n_iter, 
                                  c(input$compare_max_time_h, input$compare_max_time_m, input$compare_max_time_s),
                                  input$sa_temp_lb,
                                  1)
      )
      
      best_order_compare_1 <- instance()[-1, ]
      best_order_compare_1 <- best_order_compare_1[sa_compare_1$best_order, ]
      
      out_compare_1 <- heuristic(best_order_compare_1, strip_width(), score_compare_1(), init_sort = NA)
      stop_crit_1 <- stop_to_words(sa_compare_1$stop_criteria)
    }
    
    # Display slot 1
    output$compare_info_1 <- renderPrint(
      cat("Heuristic: ", isolate(input$compare_heuristic_1), "\n",
          "Initial Order: ", isolate(sort_to_words(input$compare_sort_1)), "\n",
          "Metaheuristic: ", isolate(input$compare_meta_1), "\n",
          "Run Time: ", paste(round(run_time_compare_1[3], 3), "s"), "\n",
          "Packed Height: ", isolate(out_compare_1$height), " \n",
          "Stopping Criterion: ", stop_crit_1, sep = "")
    )
    # Plot slot 1
    output$compare_plot_1 <- renderPlotly(vis_output(out_compare_1$layout, 
                                                     isolate(input$compare_heuristic_1),
                                                     isolate(input$compare_meta_1)))
    
    update_modal_progress(value = 1/n, text = paste("Calculating: Algorithm 2 of", n))
    ############### Slot 2 ###############
    # Heuristic score function to use for slot 2
    score_compare_2 <- reactive({
      switch(isolate(input$compare_heuristic_2),
             "Constructive" = ch_score,
             "Improved Best-Fit" = bf_score,
             "Improved Constructive" = improved_score)
    })
    # Slot 2 results
    if(input$compare_meta_2 == "None Selected"){
      # Heuristic for slot 2
      run_time_compare_2 <- system.time(
        out_compare_2 <- heuristic(instance()[-1, ], 
                                   strip_width(), 
                                   score_compare_2(), 
                                   init_sort = input$compare_sort_2)
      )
      stop_crit_2 <- "NULL"
      
    } else if(input$compare_meta_2 == "Genetic Algorithm"){
      # GA for slot 2
      req(input$ga_n_gen)
      
      run_time_compare_2 <- system.time(
        hga_compare_2 <- hybrid_ga(instance()[-1, ], 
                                   strip_width(), 
                                   instance_opt(), 
                                   score_compare_2(), 
                                   input$ga_pop_size,
                                   input$ga_n_gen,
                                   input$ga_crossover_prob,
                                   input$ga_mutation_prob,
                                   input$ga_selection_type,
                                   input$ga_crossover_type,
                                   c(input$compare_max_time_h, input$compare_max_time_m, input$compare_max_time_s),
                                   input$tournament_size,
                                   2)
      )
      
      # Best order from GA
      best_order_compare_2 <- instance()[-1, ]
      if(is.vector(hga_compare_2$best_order)){
        # If GA terminates on first iteration
        best_order_compare_2 <- best_order_compare_2[hga_compare_2$best_order, ]
      } else{
        # If GA terminates after first iteration
        best_compare_2 <- which.min(hga_compare_2$best_fitness)
        best_order_compare_2 <- best_order_compare_2[hga_compare_2$best_order[, best_compare_2], ]
      }
      
      out_compare_2 <- heuristic(best_order_compare_2, strip_width(), score_compare_2(), init_sort = NA)
      stop_crit_2 <- stop_to_words(hga_compare_2$stop_criteria)
      
    } else if(input$compare_meta_2 == "Simulated Annealing"){
      # SA for slot 2
      req(input$sa_n_iter)
      req(input$sa_init_temp)
      req(input$sa_schedule_type)
      req(input$sa_cooling_param)
      req(input$sa_temp_lb)
      req(input$sa_epoch)
      
      run_time_compare_2 <- system.time(
        sa_compare_2 <- hybrid_sa(instance()[-1, ], 
                                  strip_width(), 
                                  instance_opt(), 
                                  score_compare_2(),
                                  input$sa_init_temp,
                                  input$sa_cooling_param,
                                  input$sa_schedule_type,
                                  input$sa_epoch,
                                  input$sa_n_iter,
                                  c(input$compare_max_time_h, input$compare_max_time_m, input$compare_max_time_s),
                                  input$sa_temp_lb,
                                  2)
      )
      
      best_order_compare_2 <- instance()[-1, ]
      best_order_compare_2 <- best_order_compare_2[sa_compare_2$best_order, ]
      
      out_compare_2 <- heuristic(best_order_compare_2, strip_width(), score_compare_2(), init_sort = NA)
      stop_crit_2 <- stop_to_words(sa_compare_2$stop_criteria)
    }
    
    # Display slot 2
    output$compare_info_2 <- renderPrint(
      cat("Heuristic: ", isolate(input$compare_heuristic_2), "\n",
          "Initial Order: ", isolate(sort_to_words(input$compare_sort_2)), "\n",
          "Metaheuristic: ", isolate(input$compare_meta_2), "\n",
          "Run Time: ", paste(round(run_time_compare_2[3], 3), "s"), "\n",
          "Packed Height: ", isolate(out_compare_2$height), " \n",
          "Stopping Criterion: ", stop_crit_2, sep = "")
    )
    
    # Plot slot 2
    output$compare_plot_2 <- renderPlotly(vis_output(out_compare_2$layout,
                                                     isolate(input$compare_heuristic_2),
                                                     isolate(input$compare_meta_2)))
    
    ############### Slot 3 ###############
    if(n == 3){
      
      update_modal_progress(2/n, text = "Calculating: Algorithm 3 of 3")
      # Heuristic score function to use for slot 3
      score_compare_3 <- reactive({
        switch(isolate(input$compare_heuristic_3),
               "Constructive" = ch_score,
               "Improved Best-Fit" = bf_score,
               "Improved Constructive" = improved_score)
      })
      # Slot 3 results
      if(input$compare_meta_3 == "None Selected"){
        # Heuristic for slot 3
        run_time_compare_3 <- system.time(
          out_compare_3 <- heuristic(instance()[-1, ], 
                                     strip_width(), 
                                     score_compare_3(), 
                                     init_sort = input$compare_sort_3)
        )
        stop_crit_3 <- "NULL"
        
      } else if(input$compare_meta_3 == "Genetic Algorithm"){
        # GA for slot 3
        req(input$ga_n_gen)
        
        run_time_compare_3 <- system.time(
          hga_compare_3 <- hybrid_ga(instance()[-1, ], 
                                     strip_width(), 
                                     instance_opt(), 
                                     score_compare_3(), 
                                     input$ga_pop_size,
                                     input$ga_n_gen,
                                     input$ga_crossover_prob,
                                     input$ga_mutation_prob,
                                     input$ga_selection_type,
                                     input$ga_crossover_type,
                                     c(input$compare_max_time_h, input$compare_max_time_m, input$compare_max_time_s),
                                     input$tournament_size,
                                     3)
        )
        
        # Best order from GA
        best_order_compare_3 <- instance()[-1, ]
        if(is.vector(hga_compare_3$best_order)){
          # If GA terminates on first iteration
          best_order_compare_3 <- best_order_compare_3[hga_compare_3$best_order, ]
        } else{
          # If GA terminates after first iteration
          best_compare_3 <- which.min(hga_compare_3$best_fitness)
          best_order_compare_3 <- best_order_compare_3[hga_compare_3$best_order[, best_compare_3], ]
        }
        
        out_compare_3 <- heuristic(best_order_compare_3, strip_width(), score_compare_3(), init_sort = NA)
        stop_crit_3 <- stop_to_words(hga_compare_3$stop_criteria)
        
      } else if(input$compare_meta_3 == "Simulated Annealing"){
        # SA for slot 3
        req(input$sa_n_iter)
        req(input$sa_init_temp)
        req(input$sa_schedule_type)
        req(input$sa_cooling_param)
        req(input$sa_temp_lb)
        req(input$sa_epoch)
        
        run_time_compare_3 <- system.time(
          sa_compare_3 <- hybrid_sa(instance()[-1, ],
                                    strip_width(),
                                    instance_opt(),
                                    score_compare_3(),
                                    input$sa_init_temp,
                                    input$sa_cooling_param,
                                    input$sa_schedule_type,
                                    input$sa_epoch,
                                    input$sa_n_iter,
                                    c(input$compare_max_time_h, input$compare_max_time_m, input$compare_max_time_s),
                                    input$sa_temp_lb,
                                    3)
        )
        
        best_order_compare_3 <- instance()[-1, ]
        best_order_compare_3 <- best_order_compare_3[sa_compare_3$best_order, ]
          
        out_compare_3 <- heuristic(best_order_compare_3, strip_width(), score_compare_3(), init_sort = NA)
        stop_crit_3 <- stop_to_words(sa_compare_3$stop_criteria)
      }
      
      # Display slot 3
      output$compare_info_3 <- renderPrint({
        cat("Heuristic: ", isolate(input$compare_heuristic_3), "\n",
            "Initial Order: ", isolate(sort_to_words(input$compare_sort_3)), "\n",
            "Metaheuristic: ", isolate(input$compare_meta_3), "\n",
            "Run Time: ", paste(round(run_time_compare_3[3], 3), "s"), "\n",
            "Packed Height: ", isolate(out_compare_3$height), " \n",
            "Stopping Criterion: ", stop_crit_3, sep = "")
      })
      
      # Plot slot 3
      output$compare_plot_3 <- renderPlotly({vis_output(out_compare_3$layout,
                                                        isolate(input$compare_heuristic_3),
                                                        isolate(input$compare_meta_3))})
      
    }
    
    remove_modal_progress()
  })
  
  # Hybrid GA Function
  
  hybrid_ga <- function(data, strip_width, instance_optimal, FUN, population_size, n_generations, crossover_prob, mutation_prob, selection = c("roulette", "tournament"), crossover = c("pmx", "ox"), stop_time = NULL, tournament_size = NULL, compare = NULL){
    # data = data.frame if items id, height, width
    # strip_width = strip width
    # instance_optimal = the optimal height if known, otherwise -999
    # FUN = heuristic function to calculate fitness
    # population_size = size of population
    # n_generations = number of generations
    # crossover_prob = probability of crossover
    # mutation_prob = probability of mutation
    # elitism = keep best solution for subsequent generation or not
    # stop_time = maximum run time of the genetic algorithm (vector with hours:minutes:seconds)
    # tournament_size = the size of the tournament if tournament selection implemented
    # compare = value if algorithm is being used in shiny compare section
    
    # Set stopping time stop_time units from current 
    if(sum(stop_time) != 0){stop_run <- Sys.time() + sum(stop_time*c(60^2, 60, 1))}
    
    orders <- get_population(data, population_size, TRUE)
    
    best_order <- matrix(NA, nrow(data), (n_generations+1))
    best_fitness <- c()
    avg_fitness <- c()
    
    fitness <- get_fitness(data, orders, FUN, strip_width)
    
    gen <- 1
    while(gen <= n_generations){
      
      for(i in 1:population_size){
        
        # Select parents and apply crossover
        if(runif(1) <= crossover_prob){
          
          # Select 2 individuals for parents using roulette wheel selection
          if(selection == "roulette"){
            parentsIndex <- roulette_selection(fitness)
          } else if(selection == "tournament"){
            parentsIndex <- tournament_selection(fitness, tournament_size)
          }
          
          # Parents
          ch1 <- orders[, parentsIndex[1]]
          ch2 <- orders[, parentsIndex[2]]
          
          if(crossover == "pmx"){
            cx <- pmx(ch1, ch2)
          } else if(crossover == "ox"){
            cx <- ox(ch1, ch2)
          }
          
          # Calculate fitness of offspring
          offspring1Fitness <- heuristic(data[cx$ch1, ], strip_width, FUN)$height
          offspring2Fitness <- heuristic(data[cx$ch2, ], strip_width, FUN)$height
          
          # Replace worst individuals in population with offspring if offspring is fitter
          if(offspring1Fitness < max(fitness)){
            ind <- which.max(fitness)
            orders[, ind] <- cx$ch1
            fitness[ind] <- offspring1Fitness
          }
          
          if(offspring2Fitness < max(fitness)){
            ind <- which.max(fitness)
            orders[, ind] <- cx$ch2
            fitness[ind] <- offspring2Fitness
          }
        }
        
        # Mutation
        if(runif(1) <= mutation_prob){
          mut <- swap_mutation(data, orders, fitness, FUN, strip_width)
          orders <- mut$orders
          fitness <- mut$fitness
        }
      }
      
      # Save best permutation and associated fitness value
      best_order[, gen] <- orders[, which.min(fitness)]
      best_fitness[gen] <- min(fitness)
      avg_fitness[gen] <- mean(fitness)
      
      # Stopping conditions
      # Time limit
      if(sum(stop_time) != 0){
        if(Sys.time() > stop_run){
          stop_criteria <- "time"
          break
        }
      }
      
      # Optimal solution found
      if(best_fitness[gen] == instance_optimal){
        stop_criteria <- "optimal"
        break
      }
      
      # Max number of generations reached
      if(gen == n_generations){
        stop_criteria <- "generations"
      }
      
      # Update generations counter
      gen <- gen+1
      
      # Update progress bar in shiny app
      if(!is.null(compare)){
        update_modal_progress(gen/n_generations, text = paste("Calculating: Algorithm", compare, "- Generation", gen, "of", n_generations))
      } else {
        update_modal_progress(gen/n_generations, text = paste("Calculating: Generation", gen, "of", n_generations))
      }
    }
    
    return(list(best_order = best_order[, 1:gen], best_fitness = best_fitness, mean_fitness = avg_fitness, stop_criteria = stop_criteria))
  }
  
  # Hybrid SA Function
  hybrid_sa <- function(data, strip_width, instance_optimal, FUN, init_temp, alpha, schedule, epoch_management, stop_iter = NULL, stop_time = NULL, lb_temp = 0, compare = NULL){
    # data = data.frame of items id, height, width
    # strip_width = strip width
    # instance_optimal = the optimal height if known, otherwise -999
    # FUN = heuristic function to calculate fitness
    # init_temp = initial temperature
    # alpha = cooling rate parameter
    # schedule = procedure that governs the decrease of the temperature
    # epoch_management = number of state transitions for each temperature
    # stop_iter = maximum number of iterations of the algorithm
    # stop_time = maximum run time of the genetic algorithm (vector with hours:minutes:seconds)
    # lb_temp = lower bound on temperature
    # compare = value if algorithm is being used in shiny compare section
    
    # Set stopping time stop_time units from current
    if(sum(stop_time) != 0){stop_run <- Sys.time() + sum(stop_time*c(60^2, 60, 1))}
    
    N <- nrow(data)
    
    # Initial packing order
    best_packing <- packing_order <- init_packing <- apply(cbind(data$id), 2, sample)
    best_fitness <- fitness <- get_fitness(data, init_packing, FUN, strip_width)
    
    temp <- init_temp
    
    # Algorithm progression
    temps <- c()
    probs <- c()
    mean_fitness <- c()
    
    iter <- 1
    while(iter <= stop_iter){
      
      temps[iter] <- temp
      
      # Vector for storing fitness of each epoch iteration
      epoch_fitness_vals <- rep(0, epoch_management)
      
      # Cycling through epoch_management transitions at temperature(iter)
      for(i in 1:epoch_management){
        
        epoch_fitness_vals[i] <- fitness
        
        # Neighbourhood structure of permutations
        ind1 <- sample(1:N, 1); ind2 <- sample((1:N)[-ind1], 1)
        swap <- c(ind1,ind2)
        nb_solution <- packing_order
        nb_solution[swap,] <- nb_solution[rev(swap),]
        
        # Height of proposed neighbour solution
        nb_fitness <- get_fitness(data, nb_solution, FUN, strip_width)
        
        # Is the neighbour an improvement?
        if(nb_fitness <= best_fitness){
          probs[(iter-1)*epoch_management+i] <- 1
          packing_order <- nb_solution
          fitness <- nb_fitness
        }
        else{
          energy <- nb_fitness - fitness
          probs[(iter-1)*epoch_management+i] <- exp(-energy/temp)
          if(runif(1) <= probs[(iter-1)*epoch_management+i]){
            packing_order <- nb_solution
            fitness <- nb_fitness
          }
        }
        
        if(fitness <= best_fitness){
          best_fitness <- fitness; best_packing <- packing_order 
        }
      }
      
      # Update temperature
      temp <- temperature(init_temp, temp, alpha, iter, schedule)
      
      # Average fitness for epoch
      mean_fitness[iter] <- mean(epoch_fitness_vals)
      
      # Stopping conditions
      if(sum(stop_time) != 0){
        if(Sys.time() > stop_run){
          stop_criteria <- "time"
          break
        }
      }
      
      if(best_fitness == instance_optimal){
        stop_criteria <- "optimal"
        break
      }
      
      if(temp <= lb_temp){
        stop_criteria <- "temperature"
        break
      }
      
      if(iter == stop_iter){
        stop_criteria <- "iterations"
      }
      
      iter <- iter+1
      
      # Update progress bar in shiny app
      if(!is.null(compare)){
        update_modal_progress(iter/stop_iter, text = paste("Calculating: Algorithm", compare, "- Iteration", iter, "of", stop_iter))
      } else {
        update_modal_progress(iter/stop_iter, text = paste("Calculating: Iteration", iter, "of", stop_iter))
      }
    }
    
    return(list(best_order = best_packing, best_height = best_fitness, stop_criteria = stop_criteria, 
                temperature = temps, fitness_progress = mean_fitness, probabilities = probs))
  }
  
  # Make sure app closes properly
  session$onSessionEnded(function(){
    stopApp()
    q("no")
  })
  
}