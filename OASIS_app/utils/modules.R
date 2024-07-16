appened_opsUI <- function(id) {
  tagList(
    uiOutput(NS(id, "dynamic_title")),  # Dynamic title
    DT::dataTableOutput(NS(id, 'street_label'))
  )
}


appened_opsServer <- function(id, 
                              .data = NULL, 
                              type = NULL,
                              initial_actsec_data = initial_actsec_data_reactive, 
                              append_actsec = input$sec405,
                              initial_sec_data = NULL, 
                              input_sec_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    input_data <- .data     
    input_type <- type
    to_select <- id
    input_sec <- input_sec_data
    initial_sec <- initial_sec_data
    initial_actsec <- initial_actsec_data
    
    
    # TODO
    # fix the new read in of streets (toes and matched streets seem to work well)
    # select the OPs before appending to save as a seperate file
    # make secondary sequences appear at side panel if "toe_seq_im", "seq_im" are selected
    # make 405 appear at side panel if "toe_seq_im", "seq_im" are selected
    # move the sec and 405 right above the append barcodes button
    # probability of similar barcode for nearby regions
    # multiplex_list_out make sure it depends on available OFQ and lambda inputs
    # to OFQ add sequencing primers sequences

    
    

    
    
    # DONE
    # make sure there is an id column
    # if there is no input from the user then append 156 to the bridges
    # add amplification primers to lambdaFISH
    # construct the os bridges from the secondary inputs the user selected
    # construct the toe sequences 
    # add 405 if is selected by the user
    # make sure the reverse complementarity is respected
    # construct the lambda bridges
    # select the correct columns to show bridges panel
    # create the amplification primers table
    # fix the download handler
    # see why the report shows cropped? - something is up with the seq_im (typo at the select statement)
    # create the lambdaFISH bridges and then combine them to a library order file
    # same for lambdaFISH toe sequences
    # use precomputed mpx files instead of calculating them on the fly
    # BUG: when pressing clear filters the total number of OPs does not update in the value box
    # BUG: when pressing clear filter the number of OPs should go back to the original number for comb_ops()
    # allow for users to upload their oligopaints file 
    
    
    
    
    
    
    
    
    
    # create the output table
    out_data <- reactive({
      req(input_type() %in% c("toe_seq_im", "seq_im", "ofq", "lambdaFISH"))
      
      validate(
        need(!is.null(input_sec()), "Select secondary sequences"))
      
      
      if (is.null(input_data())) {
        return(NULL)
      }
      
      input_sec_value <- input_sec()
      

      if (is.null(input_sec_value)) {
        # Handle the NULL case, for example, return the original data or a modified version of it
        input_sec_value <- c("sec1", "sec5", "sec6")
      }
      
      input_sec_seq <- initial_sec()[initial_sec()$bitName %in% input_sec_value,]$Sequence
      input_actsec_seq <- initial_actsec()[1,]$Sequence
      
      if (!is.null(input_type()) && input_type() == "toe_seq_im"){
        data_streets <- input_data() %>%
          ungroup() %>% 
          # select(contains(id)) %>%
          distinct_at(vars(any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2"))), .keep_all = TRUE) %>%
          mutate(
            secondary = rep(input_sec_value, length.out = n()),
            secondary_seq = rep(input_sec_seq, length.out = n())
          ) %>% 
          mutate(
            street = get(names(.)[str_detect(names(.), pattern = "street")][1]),
            toe = get(names(.)[str_detect(names(.), pattern = "toe")][1])
          ) %>%
          mutate(
            street_target_seq = street,
            bridge_to_order = str_c(rc(toe), ifelse(append_actsec, rc(input_actsec_seq), ""), rc(secondary_seq)),
            toe_to_order = toe
          ) %>% 
          select(contains("chr") | contains("start") | contains("end") | 
                   any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | any_of(c("n", "size_kb", "density_kb")) |
                   "secondary" | "secondary_seq" | "street_target_seq" | contains("num") | "bridge_to_order" | "toe_to_order")
        
        # data_stats <- input_data() %>%
        #   ungroup() %>% 
        #   group_by(chr, across(any_of(c("uni_ms", "uni_bs", "ms1", "ms2", "bs1", "bs2")))) %>% 
        #   summarise(
        #     n = n(),
        #     size_kb = round((max(end) - min(start))/1000),
        #     density_kb = round(n/size_kb, digits =1)
        #   ) %>%
        #   ungroup()
        # 
        # result_data <- left_join(data_streets, data_stats)%>% 
        #   select(contains("chr") | contains("start") | contains("end") | 
        #            any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | "n" | "size_kb" | "density_kb" |
        #            "secondary" | "secondary_seq" | "street_target_seq" | "bridge_to_order" | "toe_to_order")
        
      } else if (!is.null(input_type()) && input_type() == "seq_im"){
        data_streets <- input_data() %>% 
          ungroup() %>% 
          # select(contains(id)) %>%
          distinct_at(vars(any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2"))), .keep_all = TRUE)%>%
          mutate(secondary = rep(input_sec_value, length.out = n()),
                 secondary_seq =  rep(input_sec_seq, length.out = n())) %>% 
          mutate(street = get(names(.)[str_detect(names(.), pattern = "street")][1]),
                 toe = get(names(.)[str_detect(names(.), pattern = "toe")][1])) %>%
          mutate(street_target_seq = toe,
                 bridge_to_order = str_c(rc(street),ifelse(append_actsec, rc(input_actsec_seq), "") ,rc(secondary_seq)),
                 toe_to_order = rc(toe)) %>% 
          select(contains("chr") | contains("start") | contains("end") | any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | any_of(c("n", "size_kb", "density_kb")) | 
                   "secondary" | "secondary_seq" | "street_target_seq" | contains("num") | "bridge_to_order"| "toe_to_order")
        
        # data_stats <- input_data() %>%
        #   ungroup() %>% 
        #   group_by(chr, across(any_of(c("uni_ms", "uni_bs", "ms1", "ms2", "bs1", "bs2")))) %>% 
        #   summarise(
        #     n = n(),
        #     size_kb = round((max(end) - min(start))/1000),
        #     density_kb = round(n/size_kb, digits =1)
        #   ) %>%
        #   ungroup()
        # 
        # result_data <- left_join(data_streets, data_stats)%>% 
        #   select(contains("chr") | contains("start") | contains("end") | 
        #            any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | "n" | "size_kb" | "density_kb" |
        #            "secondary" | "secondary_seq" | "street_target_seq" | "bridge_to_order" | "toe_to_order")
        
      } else if (!is.null(input_type()) && input_type() == "ofq"){
        data_streets <-  input_data() %>%
          ungroup() %>% 
          # select(contains(id)) %>%
          distinct_at(vars(any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2"))), .keep_all = TRUE)%>%
          select(contains("chr") | contains("start") | contains("end") | any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | any_of(c("n", "size_kb", "density_kb")) | 
                   contains("_seq_primer") | contains("_ofq_key") | contains("_ofq_barcode"))
        
        # data_stats <- input_data() %>%
        #   ungroup() %>% 
        #   group_by(chr, across(any_of(c("uni_ms", "uni_bs", "ms1", "ms2", "bs1", "bs2")))) %>% 
        #   summarise(
        #     n = n(),
        #     size_kb = round((max(end) - min(start))/1000),
        #     density_kb = round(n/size_kb, digits =1)
        #   ) %>%
        #   ungroup()
        # 
        # result_data <- left_join(data_streets, data_stats)%>% 
        #   select(contains("chr") | contains("start") | contains("end") | 
        #            any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | "n" | "size_kb" | "density_kb" |
        #            contains("_seq_primer") | contains("_ofq_key") | contains("_ofq_barcode"))
        
      } else if (!is.null(input_type()) && input_type() == "lambdaFISH"){
        
        
        data_streets <- input_data() %>%
          ungroup() %>%
          # select(contains(id)) %>%
          distinct_at(vars(any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2"))), .keep_all = TRUE)%>%
          mutate(street_target_seq = get(names(.)[str_detect(names(.), pattern = "street")][1]))


        # find all the columns that contain "_FWD_primer_lambdaFISH"
        data_streets_fwd <- data_streets %>%
          select(contains("_FWD_primer_lambdaFISH")) %>% 
          names()
        # mutate(streets_fwd = get(names(.)[str_detect(names(.), pattern = "_FWD_primer_lambdaFISH")][1])) %>%
        #   pull()
        
        print(data_streets_fwd)

        # find all the columns that contain "_toe_"
        data_streets_toe <- data_streets %>%
          select(contains("_toe"))%>% 
          names()
        # mutate(lambda_toe = get(names(.)[str_detect(names(.), pattern = "_toe")][1])) %>%
        #   pull()
        
        print(data_streets_toe)

        # find all the columns that contain "sec_lambdaFISH"
        data_streets_sec <- data_streets %>%
          select(contains("sec_lambdaFISH"))%>% 
          names()
        # mutate(lambda_sec = get(names(.)[str_detect(names(.), pattern = "sec_lambdaFISH")][1])) %>%
        #   pull()
        
        print(data_streets_sec)

        # find all the columns that contain "_REV_primer_lambdaFISH"
        data_streets_rev <- data_streets %>%
          select(contains("_REV_primer_lambdaFISH"))%>% 
          names()
        # mutate(streets_rev = get(names(.)[str_detect(names(.), pattern = "_REV_primer_lambdaFISH")][1])) %>%
        #   pull()
        
        print(data_streets_rev)
        
        cycles <- nchar((data_streets %>% select(contains("_key")) %>% pull())[1])
        print(cycles)
        
        for (i in 1:cycles) {
          data_streets <- data_streets %>%
            mutate(!!paste0("bridge_lambdaFISH", i) := 
                     mapply(function(fwd, toe, sec, rev) {
                       paste0(fwd, rc(toe), sec, rev)
                     }, 
                     .[[data_streets_fwd]], 
                     .[[data_streets_toe]], 
                     .[[data_streets_sec[i]]], 
                     .[[data_streets_rev[i]]])
            )
        }
        
        data_streets <- data_streets %>%
          mutate(toe_lambdaFISH = 
                   mapply(function(fwd, toe, rev) {
                     paste0(fwd, toe, rev)
                   }, 
                   .[[data_streets_fwd]], 
                   .[[data_streets_toe]], 
                   .[[data_streets_rev[(cycles+1)]]])
          )


        data_streets <- data_streets %>%
          select(contains("chr") | contains("start") | contains("end") | any_of(c("uni_ms", "uni_bs", "ms1","ms2","bs1","bs2")) | any_of(c("n", "size_kb", "density_kb")) 
                 | street_target_seq | contains("_lambdaFISH_key")
                 | contains("bridge_lambdaFISH") | contains("toe_lambdaFISH")
                 | contains("_FWD_primer_lambdaFISH") | contains("_REV_primer_lambdaFISH") | contains("_primer_lambdaFISHtoe")
                 )


        
      } else {
        NULL
      }
    })
    
    # Reactive expression to check if there is data
    has_data <- reactive({
      !is.null(out_data()) && nrow(out_data()) > 0
    })
    
    # Dynamically render the title based on data availability
    output$dynamic_title <- renderUI({
      if (has_data()) {
        title <- switch(id,
                        "uni_ms" = "Universal Mainstreet",
                        "uni_bs" = "Universal Backstreet",
                        "ms1" = "Mainstreet 1",
                        "ms2" = "Mainstreet 2",
                        "bs1" = "Backstreet 1",
                        "bs2" = "Backstreet 2",
                        NULL)  # Default case
        
        if (!is.null(title)) {
          h3(title, class = "data-table-title")
        }
      }
    })
    
    # Define the proxy for the datatable
    proxy <- dataTableProxy(id)
    
    
    # Render the datatable
    output$street_label <- DT::renderDataTable({
      datatable(
        out_data(),
        editable = F,  # Make the datatable editable
        extensions = c('Scroller','FixedColumns'),
        options = list(
          deferRender = FALSE,
          # scroller = TRUE, # Gives a problem with the spacing of the last row
          # dom = 't',
          # scrollX = "100%",
          # scrollY = "300px",
          fixedColumns = TRUE
        )
      )
    })
    
    # Observe any editing events on the datatable
    observeEvent(input[[paste0(id, "_cell_edit")]], {
      info <- input[[paste0(id, "_cell_edit")]]
      # Update the data based on the changes made by the user
      newdata <- out_data()
      newdata[info$row, info$col] <- info$value
      # Replace the old data with the updated data
      replaceData(proxy, newdata, resetPaging = FALSE, rownames = FALSE)
      out_data <- newdata
    })
    
    return(out_data)
    
  })
}

# renderOASISReport <- function(input, output, session, data = list()) {
#   output$reportOutput <- renderUI({
#     # Create a new environment and assign variables from `data` to it
#     rmd_env <- new.env()
#     for (name in names(data)) {
#       assign(name, data[[name]], envir = rmd_env)
#     }
#     
#     # Render the OASIS RMD to HTML with the environment containing the variables
#     html_content <- rmarkdown::render("OASIS_report_2.Rmd", output_format = "html_document", envir = rmd_env, clean = FALSE)
#     
#     # Return the rendered HTML content
#     # tags$iframe(src = html_content, width = "100%", height = "600px", frameborder = "0")
#   })
# }

