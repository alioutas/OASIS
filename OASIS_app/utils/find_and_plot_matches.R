library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringi)
library(RColorBrewer)
library(parallel)
library(plotly)

# Reverse complement DNA sequence   -------------------------------------------------------------------

rc <- function (z)
{
  rc1 <- function(zz) {
    s <- strsplit(zz, split = "")[[1]]
    s <- rev(s)
    dchars <- strsplit("ACGTMRWSYKVHDBNI", split = "")[[1]]
    comps <- strsplit("TGCAKYWSRMBDHVNI", split = "")[[1]]
    s <- s[s %in% dchars]
    s <- dchars[match(s, comps)]
    s <- paste0(s, collapse = "")
    return(s)
  }
  z <- toupper(z)
  tmpnames <- names(z)
  res <- unname(sapply(z, rc1))
  if (!is.null(attr(z, "quality"))) {
    strev <- function(x) sapply(lapply(lapply(unname(x),
                                              charToRaw), rev), rawToChar)
    attr(res, "quality") <- unname(sapply(attr(z, "quality"),
                                          strev))
  }
  names(res) <- tmpnames
  return(res)
}
# import data

# df <- read_csv('/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/HMS/genome_project/WGI2_lib/output/WGI2.0/WGI2.0_appended_ops_steps1_13_ORDER.txt', col_names = 'ops')
# df<- read_csv('/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI2_lib/output/WGI2.0/WGI2.0_appended_ops_steps1_13_GLess_IDT11_ORDER.txt', col_names = 'appended_oligopaint')

# query_seq <- c('GCACGCAGGTTGGTCGGTAC',
#                'CTACCGTTCGTGGAGGCACC',
#                'GCCTGGAGAAGCCAACGGAG',
#                'GCGTGCGTAAGCCCATGTCG',
#                'CTCATGCGTGGTCACCCGTC',
#                'CGAGCCCGGAGACGCGATAG',
#                'CAAGGGCGCCTACCCGATTC',
#                'CGGAGTACCTCGCGCTCAAC',
#                'GCCTCCGCATCTGCGAATCC',
#                'CGGTGCTTAGTGCGTGCAGG',
#                'CGTTCGGTTCTCCGGTCACC')
# #
# query_seq <- c('GATCGGGTCCCACAACCACG', #43
#                'GTGCCAGGCAACCCGTACAG', #44
#                 'GAACGGCGTCACGCTGAGAG', #45
#                'CGACCGGACACACCTCCTCC', #46
#                'CTATCAGGGCAACCCGCAGG') #47
# 
# 
# query_name <- c('I_am_43I_am_43I_am_43',
#                 'I_am_44I_am_44I_am_44',
#                 'I_am_45I_am_45I_am_45',
#                 'I_am_46I_am_46I_am_46',
#                 'I_am_47I_am_47I_am_47')

# query_df <- tibble(query_seq, query_name)


# df <- read_tsv("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/collaborations/Maria-Elena\ Torres\ Padilla/20250126_iLADs/output/chr10_mm10_LAD_iLAD_Oligopaints_order.txt", col_names = F)
# names(df) <- "appended_oligopaint"
# query_df <- read_tsv("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/collaborations/Maria-Elena\ Torres\ Padilla/20250126_iLADs/output/chr10_mm10_LAD_iLAD_Oligopaints_barcodes.txt", col_names = F )
# names(query_df) <- c("query_seq","query_name")


# df <- read_csv("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-01-22/OASIS_all_datatable.csv") %>% select(appended_oligopaint) %>% distinct(appended_oligopaint)
# query_df <- read_csv("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-01-22/bs2_bridges_toes.csv") %>% select(bs2, street_target_seq)
# names(query_df) <- c("query_name", "query_seq")

# query_df <- tibble()
# query_df <- rbind(query_df, read_delim("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/ms3_bridges_toes.csv") %>%
#                 select(street_target_seq, ms3) %>%
#                 rename(query_seq = street_target_seq,query_name = ms3)
# )
# query_df <- rbind(query_df, read_delim("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/ms1_bridges_toes.csv") %>%
#                 select(street_target_seq, ms1) %>%
#                 rename(query_seq = street_target_seq,query_name = ms1)
# )
# query_df <- rbind(query_df, read_delim("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/ms2_bridges_toes.csv") %>%
#   select(street_target_seq, ms2) %>%
#   rename(query_seq = street_target_seq,query_name = ms2)
# )
# 
# query_df <- rbind(query_df, read_delim("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/bs1_bridges_toes.csv") %>%
#                     select(street_target_seq, bs1) %>%
#                     rename(query_seq = street_target_seq,query_name = bs1)
# )
# 
# query_df <- rbind(query_df, read_delim("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/bs2_bridges_toes.csv") %>%
#                     select(street_target_seq, bs2) %>%
#                     rename(query_seq = street_target_seq,query_name = bs2)
# )
# 
# query_df <- rbind(query_df, read_delim("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/bs3_bridges_toes.csv") %>%
#                     select(street_target_seq, bs3) %>%
#                     rename(query_seq = street_target_seq, query_name = bs3)
# )
# 
# df <- read_tsv("/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My Drive/2.Areas/HMS/genome_project/WGI3_lib/output/WGI3_0_OASIS_output_2025-02-06/OASIS_all_datatable_unique.tsv", col_select = "appended_oligopaint")
# 
# write_delim(df,"/Users/alioutas/Google Drive/My Drive/1.Projects/GitHub/demo_data/df.tsv" ,delim = "\t", col_names = T)
# write_delim(query_df,"/Users/alioutas/Google Drive/My Drive/1.Projects/GitHub/demo_data/query_df.tsv" ,delim = "\t", col_names = T)


find_and_plot_matches <- function(df, query_df) {
  numberOfCores <- ceiling(detectCores()*0.75)
  
  # Ensure necessary packages are available
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Please install stringi: install.packages('stringi')")
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Please install RColorBrewer: install.packages('RColorBrewer')")
  }
  
  # Add row index as ID if not present
  if (!is.null(df) && !"id" %in% names(df)) {
    df$id <- seq_len(nrow(df))
  }
  # Check for required column
  if (!"appended_oligopaint" %in% names(df)) {
    stop("df must have a column 'appended_oligopaint'")
  }
  
  # Calculate maximum oligo length
  appended_len <- nchar(df$appended_oligopaint)
  max_length <- max(appended_len, na.rm = TRUE)
  
  # Function to process each query
  process_query <- function(query_seq) {
    # locs <- round(stringi::stri_locate_first_fixed(df$appended_oligopaint, query_seq) / max_length, 1)
    locs <- stringi::stri_locate_first_fixed(df$appended_oligopaint, query_seq) / max_length
    
    
    locs_temp <- as_tibble(locs) %>%
      mutate(id = row_number()) %>%
      filter(!is.na(start) & !is.na(end)) %>%
      arrange(start, end, id) %>%
      mutate(group_id = cumsum(c(TRUE, abs(diff(id)) != 1))) %>%
      group_by(start, end, group_id) %>%
      reframe(
        y_start = min(id),
        y_end = max(id)
      ) %>%
      mutate("query_seq" = query_seq) %>%
      select(start, end, y_start, y_end, query_seq)
    
    locs_temp
  }
  
  # Use mclapply to process queries in parallel
  out_coords_list <- mclapply(query_df$query_seq, process_query, mc.cores = numberOfCores)
  
  remove_jumps <- function(df){

  query_seq_temp <- unique(df$query_seq)
   df %>%
      arrange(y_start) %>%
      mutate(distance = y_end - y_start) %>%
      filter(distance <40) %>%
      summarize(start = min(start), end = max(end), y_start = min(y_start), y_end = max(y_end)) %>%
     mutate(query_seq = query_seq_temp)
  }

  for(i in seq_along(out_coords_list)){
    if(nrow(out_coords_list[[i]]) > 1){
      out_coords_list[[i]] <-  remove_jumps(df = out_coords_list[[i]])
    } else{
      next
    }
  }
  
  # Combine results into a single tibble
  out_coords <- bind_rows(out_coords_list)
  out_coords <- left_join(out_coords, query_df, by = "query_seq")
  
  # Check for matches
  if (!nrow(out_coords)) {
    message("No matches found at all.")
    return(invisible(NULL))
  }
  
  # Determine colors for plotting
  seq_used <- unique(out_coords$query_seq)
  num_used <- length(seq_used)
  
  colors_list <- if (num_used > 8) {
    colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_used)
  } else {
    RColorBrewer::brewer.pal(max(3, num_used), "Dark2")[1:num_used]
  }
  
  # Assign colors to sequences
  color_mapping <- setNames(colors_list, seq_used)
  out_coords$color <- color_mapping[out_coords$query_seq]
  out_coords$start <- out_coords$start * max_length
  out_coords$end <- out_coords$end * max_length
  
  # Prepare label coordinates
  label_coords_seq <- out_coords %>%
    group_by(query_seq, query_name) %>%
    summarise(
      x = mean(start + end) / 2,
      y = max(y_end) - ((y_end - y_start) * 0.25),
      .groups = 'drop'
    )
  
  label_coords_id <- out_coords %>%
    group_by(query_seq, query_name) %>%
    summarise(
      x = mean(start + end) / 2,
      y = max(y_end) - ((y_end - y_start) * 0.10),
      .groups = 'drop'
    )
  
  # Create the plot
  p <- ggplot(data = out_coords, aes(x = (start + end) / 2, y = (y_start + y_end) / 2)) +
    geom_tile(aes(width = end - start, height = y_end - y_start, fill = query_seq), color = "white") +
    scale_fill_manual(values = color_mapping) +
    geom_text(aes(label = (y_end - y_start) + 1), size = 2, color = "black") +
    geom_text(data = label_coords_seq, aes(x = x, y = y, label = query_seq), size = 1, vjust = 0) +
    geom_text(data = label_coords_id, aes(x = x, y = y, label = query_name), size = 2, vjust = 0) +
    labs(x = "Oligopaint length", y = "Oligopaint number in the library") +
    theme_minimal() +
    scale_x_continuous(limits = c(0, max_length))+
    theme(legend.position = "none")
  
  # Return plot invisibly
  invisible(list(plot = p))
}


# start <- Sys.time()
# p <- find_and_plot_matches(df, query_df =query_df )$'plot'
# ggplotly(p)
# Sys.time() - start
