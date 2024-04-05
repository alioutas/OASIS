library(tidyverse)
library(RColorBrewer)
library(ggplot2)

# import data

# df <- read_csv('/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/HMS/genome_project/WGI2_lib/output/WGI2.0/WGI2.0_appended_ops_steps1_13_ORDER.txt', col_names = 'ops')
df<- read_csv('/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/HMS/genome_project/WGI2_lib/output/WGI2.0/WGI2.0_appended_ops_steps1_13_GLess_IDT11_ORDER.txt', col_names = 'ops')

df$id <- 1:nrow(df)

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

query_seq <- c('GATCGGGTCCCACAACCACG', #43
               'GTGCCAGGCAACCCGTACAG', #44
                'GAACGGCGTCACGCTGAGAG', #45
               'CGACCGGACACACCTCCTCC', #46
               'CTATCAGGGCAACCCGCAGG') #47


find_and_plot_matches <- function(df, query_seq, colors = NULL) {
  # Check if colors are provided, else use ColorBrewer
  if(is.null(colors)) {
    colors <- RColorBrewer::brewer.pal(min(length(query_seq), 12), "Set3")
  }
  
  # Ensure 'id' column exists in 'df'
  if(!"id" %in% names(df)) {
    df$id <- seq_len(nrow(df))
  }
  
  df <- df %>%
    mutate(length = nchar(ops))
  
  # Initialize list to store matches
  matches <- list()
  
  for(i in seq_along(query_seq)) {
    seq <- query_seq[i]
    
    # Find exact matches for each sequence
    matches_found <- df %>%
      mutate(match_found = stringr::str_detect(ops, stringr::fixed(seq))) %>%
      filter(match_found) %>%
      mutate(match_start = stringr::str_locate(ops, stringr::fixed(seq))[,1],
             match_end = match_start + nchar(seq) - 1,
             sequence_id = i) %>%
      select(id, match_start, match_end, sequence_id)
    
    # Store the results
    matches[[i]] <- matches_found
  }
  
  # Combine all matches into a single data frame
  df_matches <- bind_rows(matches)
  
  # Calculate count of each sequence's appearance
  sequence_counts <- df_matches %>%
    group_by(sequence_id) %>%
    summarise(count = n()) %>%
    mutate(sequence = query_seq[sequence_id],
           label = paste(sequence, ": ", count))
  
  # Join back to df_matches for labeling
  df_matches <- df_matches %>%
    left_join(sequence_counts, by = "sequence_id") %>%
    mutate(color = setNames(colors, seq_along(query_seq))[as.character(sequence_id)])
  
  # Generate the plot
  plot <- ggplot() +
    geom_bar(data = df, aes(x = as.numeric(id), y = length), stat = "identity", fill = "#EAEAEA") +
    geom_rect(data = df_matches, aes(xmin = as.numeric(id) + 0.5, 
                                     xmax = as.numeric(id) - 0.5,
                                     ymin = match_start, 
                                     ymax = match_end, 
                                     fill = color, 
                                     group = id),
              stat = "identity") +
    scale_fill_identity() +
    theme_bw() +
    labs(x = "Sequence ID", y = "Oligopaint length") +
    coord_flip() +
    geom_text(data = df_matches %>% distinct(sequence_id, .keep_all = TRUE),
              aes(x = as.numeric(id), y = match_start, label = label),
              hjust = 0, vjust = 0.5, size = 2.5, color = "black")
  
  # return plot and matches
  return(list(plot = plot, matches = df_matches))
}

find_and_plot_matches(df, query_seq)['plot']
