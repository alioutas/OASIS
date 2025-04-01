require(tidyverse)


append_MS_BS <- function(MS = MS, BS = BS) {
  # have both MS and BS
  if(!is.null(MS) && !is.null(BS)){
    MS_BS <- cbind(MS, BS)
    MS_BS_grouped <- MS_BS %>% 
      group_by( start,end,id_MS,id_BS) %>% #chr,
      group_nest()
    # both MS and BS OS
    if (input$append_streets_MS == "seq_im" | input$append_streets_MS == "toe_seq_im"){
      if (input$append_streets_BS == "seq_im" | input$append_streets_BS == "toe_seq_im") {
        MS_BS_grouped_names <- tibble("id_MS" = MS_BS_grouped$id_MS, "id_BS" = MS_BS_grouped$id_BS) 
        MS_BS_pairs<- MS_BS_grouped_names %>% group_by(id_MS) %>% distinct(id_BS) %>% count(id_MS)
        M_BS_seq_pairs <-tibble()
        for(i in 1:nrow(MS_BS_pairs)){
          # mainstreet barcode
          j <- min(total_streets)
          total_streets <- setdiff(total_streets, j)
          # backstreet barcodes
          k <- matched_streets %>% 
            filter(key == j) %>% 
            arrange(main)
          k<- sort(intersect(k$main, total_streets))
          k <- k[1:as.numeric(MS_BS_pairs[i,"n"])]
          total_streets <- setdiff(total_streets, k)
          new_pairs <- tibble(ms_number = rep(j, as.numeric(MS_BS_pairs[i,"n"])) ,bs_number = k)
          M_BS_seq_pairs <- bind_rows(M_BS_seq_pairs, new_pairs)
          M_BS_seq_pairs
        }               
        MS_BS_pairs <- bind_cols(MS_BS_grouped_names %>% group_by(id_BS) %>% distinct(id_MS) %>% arrange(id_MS), M_BS_seq_pairs) 
        # streets or toes?
        MS_BS_pairs$MS_seq <- if (input$append_streets_MS == "seq_im") {
          streets$streets[MS_BS_pairs$MS_number]
        } else if(input$append_streets_MS == "toe_seq_im") {
          toes$toe[MS_BS_pairs$MS_number]  
        }
        #to be RC or not to RC? Reverse Complementary
        MS_BS_pairs$BS_seq <- if (input$append_streets_BS == "seq_im") {
          if (input$uni_BS_rc == TRUE) {rc(streets$streets[MS_BS_pairs$BS_number])} else if (input$uni_BS_rc == FALSE) {streets$streets[MS_BS_pairs$BS_number]
          } else if(input$append_streets_BS == "toe_seq_im") {
            if (input$uni_BS_rc == TRUE) {rc(toes$toe[MS_BS_pairs$BS_number])} else if (input$uni_BS_rc == FALSE) {toes$toe[MS_BS_pairs$BS_number]
            }
          }
        }  
        MS_BS_grouped_joined <- left_join(MS_BS_grouped, MS_BS_pairs, by = c("id_MS", "id_BS"))
        
        MS_BS_appended <- MS_BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, id_BS, BS_number, BS_seq, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T"), BS_seq))
        # MS OS and BS OF
      } else if (input$append_streets_BS == "olfsq") {
        MS_BS <- cbind(MS, BS)
        MS_BS_grouped <- MS_BS %>% 
          group_by( start,end,id_MS,id_BS) %>% #chr,
          group_nest()
        MS_BS_grouped_names <- tibble("id_MS" = MS_BS_grouped$id_MS, "id_BS" = MS_BS_grouped$id_BS) 
        MS_BS_pairs<- MS_BS_grouped_names %>% group_by(id_MS) %>% distinct(id_BS) %>% count(id_MS)
        # create OF barcodes
        if (nrow(MS_BS_pairs) < 36) {
          cycles <- 3
        } else if (between(nrow(MS_BS_pairs),36,108)) {
          cycles <- 4
        } else if (between(nrow(MS_BS_pairs),108,324)) {
          cycles <- 5
        } else if (between(nrow(MS_BS_pairs),324,4056)) {
          cycles <- 6
        } else if (between(nrow(MS_BS_pairs),4056,16092)) {
          cycles <- 7
        } else if (between(nrow(MS_BS_pairs),16092, 63720)) {
          cycles <- 8
        } else if (between(nrow(MS_BS_pairs),63720, 262144)) {
          cycles <- 9
        } else if (between(nrow(MS_BS_pairs),262144, 1048572)) {
          cycles <- 10
        }
        OF_barcodes <- generate_oligoFISSEQ_barcodes(cycles = cycles)
        
        M_BS_seq_pairs <-tibble()
        M_BS_seq_pairs <- tibble(MS_number = min(available_barcodes):((min(available_barcodes)-1)+length(MS_BS_pairs$id_MS)) ,
                                 BS_number = 1:length(MS_BS_pairs$id_MS))
        MS_BS_pairs <- bind_cols(MS_BS_grouped_names %>% group_by(id_BS) %>% distinct(id_MS) %>% arrange(id_MS), M_BS_seq_pairs) 
        # streets or toes?
        MS_BS_pairs$MS_seq <- if (input$append_streets_MS == "seq_im") {
          streets$streets[MS_BS_pairs$MS_number]
        } else if(input$append_streets_MS == "toe_seq_im") {
          toes$toe[MS_BS_pairs$MS_number]  
        }
        
        MS_BS_pairs$BS_seq <- OF_barcodes$sequence[MS_BS_pairs$BS_number]
        MS_BS_pairs$BS_key <- OF_barcodes$key[MS_BS_pairs$BS_number]
        MS_BS_grouped_joined <- left_join(MS_BS_grouped, MS_BS_pairs, by = c("id_MS", "id_BS"))
        MS_BS_appended <- MS_BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, id_BS, BS_number, BS_seq, BS_key,  temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T"), BS_seq))
        # 
      }
      # MS OF and BS OS
    } else if (input$append_streets_MS == "olfsq") {
      if (input$append_streets_BS == "seq_im" | input$append_streets_BS == "toe_seq_im"){
        MS_BS <- cbind(MS, BS)
        MS_BS_grouped <- MS_BS %>% 
          group_by(start,end,id_MS,id_BS) %>% #chr, 
          group_nest()
        MS_BS_grouped_names <- tibble("id_MS" = MS_BS_grouped$id_MS, "id_BS" = MS_BS_grouped$id_BS) 
        MS_BS_pairs<- MS_BS_grouped_names %>% group_by(id_MS) %>% distinct(id_BS) %>% count(id_MS)
        # create OF barcodes
        if (nrow(MS_BS_pairs) < 36) {
          cycles <- 3
        } else if (between(nrow(MS_BS_pairs),36,108)) {
          cycles <- 4
        } else if (between(nrow(MS_BS_pairs),108,324)) {
          cycles <- 5
        } else if (between(nrow(MS_BS_pairs),324,4056)) {
          cycles <- 6
        } else if (between(nrow(MS_BS_pairs),4056,16092)) {
          cycles <- 7
        } else if (between(nrow(MS_BS_pairs),16092, 63720)) {
          cycles <- 8
        } else if (between(nrow(MS_BS_pairs),63720, 262144)) {
          cycles <- 9
        } else if (between(nrow(MS_BS_pairs),262144, 1048572)) {
          cycles <- 10
        }
        OF_barcodes <- generate_oligoFISSEQ_barcodes(cycles = cycles)
        
        M_BS_seq_pairs <-tibble()
        M_BS_seq_pairs <- tibble(MS_number = min(available_barcodes):((min(available_barcodes)-1)+length(MS_BS_pairs$id_MS)) ,
                                 BS_number = 1:length(MS_BS_pairs$id_MS))
        MS_BS_pairs <- bind_cols(MS_BS_grouped_names %>% group_by(id_BS) %>% distinct(id_MS) %>% arrange(id_MS), M_BS_seq_pairs) 
        # streets or toes?
        MS_BS_pairs$BS_seq <- if (input$append_streets_BS == "seq_im") {
          streets$streets[MS_BS_pairs$BS_number]
        } else if(input$append_streets_BS == "toe_seq_im") {
          toes$toe[MS_BS_pairs$BS_number]  
        }
        
        MS_BS_pairs$MS_seq <- OF_barcodes$sequence[MS_BS_pairs$BS_number]
        MS_BS_pairs$MS_key <- OF_barcodes$key[MS_BS_pairs$BS_number]
        MS_BS_grouped_joined <- left_join(MS_BS_grouped, MS_BS_pairs, by = c("id_MS", "id_BS"))
        MS_BS_appended <- MS_BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, MS_key, id_BS, BS_number, BS_seq, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T"), BS_seq))
        # MS OF and BS OF    
      } else if (input$append_streets_BS == "olfsq") {
        MS_BS <- cbind(MS, BS)
        MS_BS_grouped <- MS_BS %>% 
          group_by( start,end,id_MS,id_BS) %>% #chr,
          group_nest()
        MS_BS_grouped_names <- tibble("id_MS" = MS_BS_grouped$id_MS, "id_BS" = MS_BS_grouped$id_BS) 
        MS_BS_pairs<- MS_BS_grouped_names %>% group_by(id_MS) %>% distinct(id_BS) %>% count(id_MS)
        # create OF barcodes
        if ((nrow(MS_BS_pairs)*2) < 36) {
          cycles <- 3
        } else if (between((nrow(MS_BS_pairs)*2),36,108)) {
          cycles <- 4
        } else if (between((nrow(MS_BS_pairs)*2),108,324)) {
          cycles <- 5
        } else if (between((nrow(MS_BS_pairs)*2),324,4056)) {
          cycles <- 6
        } else if (between((nrow(MS_BS_pairs)*2),4056,16092)) {
          cycles <- 7
        } else if (between((nrow(MS_BS_pairs)*2),16092, 63720)) {
          cycles <- 8
        } else if (between((nrow(MS_BS_pairs)*2),63720, 262144)) {
          cycles <- 9
        } else if (between((nrow(MS_BS_pairs)*2),262144, 1048572)) {
          cycles <- 10
        }
        OF_barcodes <- generate_oligoFISSEQ_barcodes(cycles = cycles)
        
        M_BS_seq_pairs <-tibble()
        M_BS_seq_pairs <- tibble(MS_number = 1:length(MS_BS_pairs$id_MS),
                                 BS_number = (length(MS_BS_pairs$id_MS)+1):(nrow(MS_BS_pairs)*2))
        MS_BS_pairs <- bind_cols(MS_BS_grouped_names %>% group_by(id_BS) %>% distinct(id_MS) %>% arrange(id_MS), M_BS_seq_pairs) 
        MS_BS_pairs$MS_seq <- OF_barcodes$sequence[MS_BS_pairs$MS_number]
        MS_BS_pairs$MS_key <- OF_barcodes$key[MS_BS_pairs$MS_number]
        MS_BS_pairs$BS_seq <- OF_barcodes$sequence[MS_BS_pairs$BS_number]
        MS_BS_pairs$BS_key <- OF_barcodes$key[MS_BS_pairs$BS_number]
        MS_BS_grouped_joined <- left_join(MS_BS_grouped, MS_BS_pairs, by = c("id_MS", "id_BS"))
        MS_BS_appended <- MS_BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, MS_key, id_BS, BS_number, BS_seq, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T"), BS_seq))
        
      } 
    }
    # only MS
  } else if(!is.null(MS) && is.null(BS)){
    # only MS OS
    if (input$append_streets_MS == "seq_im" | input$append_streets_MS == "toe_seq_im"){
      MS_grouped <- MS %>% 
        group_by(id_MS) %>% 
        group_nest()
      MS_grouped_names <- tibble("id_MS" = MS_grouped$id_MS, "MS_number" =  min(available_barcodes):((min(available_barcodes)-1)+length(MS_grouped$id_MS)))
      
      # streets or toes?
      MS_grouped_names$MS_seq <- if (input$append_streets_MS == "seq_im") {
        streets$streets[MS_grouped_names$MS_number]
      } else if(input$append_streets_MS == "toe_seq_im") {
        toes$toe[MS_grouped_names$MS_number]  
      }
      
      if (input$same_BS == FALSE){
        MS_grouped_joined <- left_join(MS_grouped, MS_grouped_names, by = "id_MS")
        MS_BS_appended <- MS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS$sequence))), side = c("left"), pad = "T")))
        # only MS same BS
      }    else if (input$same_BS == TRUE) {
        MS_grouped_names <- MS_grouped_names %>% mutate("id_BS" = id_MS, "BS_number" = MS_number,"BS_seq" = MS_seq )
        MS_grouped_joined <- left_join(MS_grouped, MS_grouped_names, by = "id_MS")
        MS_BS_appended <- MS_BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, id_BS, BS_number, BS_seq, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T"), BS_seq))
      }
      # only MS OF  
    } else if (input$append_streets_BS == "olfsq") {
      MS_grouped <- MS %>% 
        group_by(id_MS) %>% 
        group_nest()
      MS_grouped_names <- tibble("id_MS" = MS_grouped$id_MS) 
      # create OF barcodes
      if (nrow(MS_grouped_names) < 36) {
        cycles <- 3
      } else if (between(nrow(MS_grouped_names),36,108)) {
        cycles <- 4
      } else if (between(nrow(MS_grouped_names),108,324)) {
        cycles <- 5
      } else if (between(nrow(MS_grouped_names),324,4056)) {
        cycles <- 6
      } else if (between(nrow(MS_grouped_names),4056,16092)) {
        cycles <- 7
      } else if (between(nrow(MS_grouped_names),16092, 63720)) {
        cycles <- 8
      } else if (between(nrow(MS_grouped_names),63720, 262144)) {
        cycles <- 9
      } else if (between(nrow(MS_grouped_names),262144, 1048572)) {
        cycles <- 10
      }
      OF_barcodes <- generate_oligoFISSEQ_barcodes(cycles = cycles)
      
      if (input$same_BS == FALSE){
        M_S1_seq_pairs <-tibble()
        M_S1_seq_pairs <- tibble(MS_number = 1:length(MS_pairs$id_MS))
        MS_pairs <- bind_cols(MS_grouped_names %>% group_by(id_MS)  %>% arrange(id_MS), M_S1_seq_pairs) 
        MS_pairs$MS_seq <- OF_barcodes$sequence[MS_pairs$MS_number]
        MS_pairs$MS_key <- OF_barcodes$key[MS_pairs$MS_number]
        MS_grouped_joined <- left_join(MS_grouped, MS_pairs, by = "id_MS")
        MS_BS_appended <- MS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, MS_key, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T")))
        # only MS same BS
      } else if (input$same_BS == TRUE) {
        M_S1_seq_pairs <-tibble()
        M_S1_seq_pairs <- tibble(MS_number = 1:length(MS_pairs$id_MS))
        MS_pairs <- bind_cols(MS_grouped_names %>% group_by(id_MS)  %>% arrange(id_MS), M_S1_seq_pairs) 
        MS_pairs$MS_seq <- OF_barcodes$sequence[MS_pairs$MS_number]
        MS_pairs$MS_key <- OF_barcodes$key[MS_pairs$MS_number]
        MS_pairs <- MS_pairs %>% mutate("id_BS" = id_MS, "BS_number" = MS_number, "BS_seq" = MS_seq, "BS_key" = MS_key)
        MS_grouped_joined <- left_join(MS_grouped, MS_pairs, by = "id_MS")
        MS_BS_appended <- MS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_MS, MS_number, MS_seq, MS_key, id_BS, BS_number, BS_seq, BS_key, temp) %>%
          mutate(appended_oligopaint = str_c(MS_seq, str_pad(sequence, round(max(str_count(MS_BS$sequence))), side = c("both"), pad = "T"), BS_seq))
      }
    }
    # only BS    
  } else if(is.null(MS) && !is.null(BS)){
    if (input$append_streets_B1 == "seq_im" | input$append_streets_BS == "toe_seq_im"){
      BS_grouped <- BS %>% 
        group_by(id_BS) %>% 
        group_nest()
      BS_grouped_names <- tibble("id_BS" = BS_grouped$id_BS, "BS_number" =  min(available_barcodes):((min(available_barcodes)-1)+length(BS_grouped$id_BS)))
      
      # streets or toes?
      BS_grouped_names$BS_seq <- if (input$append_streets_BS == "seq_im") {
        streets$streets[BS_grouped_names$BS_number]
      } else if(input$append_streets_BS == "toe_seq_im") {
        toes$toe[BS_grouped_names$BS_number]  
      }
      BS_grouped_joined <- left_join(BS_grouped, BS_grouped_names, by = "id_BS")
      MS_BS_appended <- BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_BS, BS_number, BS_seq, temp) %>%
        mutate(appended_oligopaint = str_c(BS_seq, str_pad(sequence, round(max(str_count(BS$sequence))), side = c("left"), pad = "T")))
      # only BS OF  
    } else if (input$append_streets_BS == "olfsq") {
      BS_grouped <- BS %>% 
        group_by(id_BS) %>% 
        group_nest()
      BS_grouped_names <- tibble("id_BS" = BS_grouped$id_BS) 
      # create OF barcodes
      if (nrow(BS_grouped_names) < 36) {
        cycles <- 3
      } else if (between(nrow(BS_grouped_names),36,108)) {
        cycles <- 4
      } else if (between(nrow(BS_grouped_names),108,324)) {
        cycles <- 5
      } else if (between(nrow(BS_grouped_names),324,4056)) {
        cycles <- 6
      } else if (between(nrow(BS_grouped_names),4056,16092)) {
        cycles <- 7
      } else if (between(nrow(BS_grouped_names),16092, 63720)) {
        cycles <- 8
      } else if (between(nrow(BS_grouped_names),63720, 262144)) {
        cycles <- 9
      } else if (between(nrow(BS_grouped_names),262144, 1048572)) {
        cycles <- 10
      }
      OF_barcodes <- generate_oligoFISSEQ_barcodes(cycles = cycles)
      M_S1_seq_pairs <-tibble()
      M_S1_seq_pairs <- tibble(BS_number = 1:length(BS_pairs$id_BS))
      BS_pairs <- bind_cols(BS_grouped_names %>% group_by(id_BS)  %>% arrange(id_BS), M_S1_seq_pairs) 
      BS_pairs$BS_seq <- OF_barcodes$sequence[BS_pairs$BS_number]
      BS_pairs$BS_key <- OF_barcodes$key[BS_pairs$BS_number]
      BS_grouped_joined <- left_join(BS_grouped, BS_pairs, by = "id_BS")
      MS_BS_appended <- BS_grouped_joined %>% unnest(cols = c(data)) %>% select(chr, start, end, sequence, id_BS, BS_number, BS_seq, BS_key, temp) %>%
        mutate(appended_oligopaint = str_c(BS_seq, str_pad(sequence, round(max(str_count(BS_BS$sequence))), side = c("both"), pad = "T")))
    }
  }
  MS_BS_appended
}
