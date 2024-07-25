library(tidyverse)
library(RColorBrewer)
library(ggplot2)

# import data

# df <- read_csv('/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/HMS/genome_project/WGI2_lib/output/WGI2.0/WGI2.0_appended_ops_steps1_13_ORDER.txt', col_names = 'ops')
df<- read_csv('/Users/alioutas/Library/CloudStorage/GoogleDrive-alioutas@gmail.com/My\ Drive/2.Areas/HMS/genome_project/WGI2_lib/output/WGI2.0/WGI2.0_appended_ops_steps1_13_GLess_IDT11_ORDER.txt', col_names = 'ops')
names(df) <- c('ops')
# df$id <- 1:nrow(df)

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


query_name <- c('This_is_43This_is_43This_is_43',
                'This_is_44This_is_44This_is_44',
                'This_is_45This_is_45This_is_45',
                'This_is_46This_is_46This_is_46',
                'This_is_47This_is_47This_is_47')

query_df <- tibble(query_seq, query_name)


find_and_plot_matches <- function(df, query_df) {
  # Check if colors are provided, else use ColorBrewer
  colors_list <- RColorBrewer::brewer.pal(length(query_df$query_seq), "Dark2")
  
  # Ensure 'id' column exists in 'df'
  if(!"id" %in% names(df)) {
    df$id <- seq_len(nrow(df))
  }
  
  # Check if 'ops' column exists in 'df'
  if(!"ops" %in% names(df)) {
    stop("The dataframe does not have a 'ops' column.")
  }
  
  df <- df %>%
    mutate(length = nchar(ops))
  
  max_length <- max(df$length)
  
  # Initialize list to store matches
  matches <- list()
  
  for(i in seq_along(query_df$query_seq)) {
    seq <- query_df$query_seq[i]
    # Find exact matches for each sequence
    matches_found <- df %>%
      mutate(match_found = stringr::str_detect(ops, stringr::fixed(seq))) %>%
      filter(match_found) %>%
      mutate(match_start = stringr::str_locate(ops, stringr::fixed(seq))[,1],
             match_end = match_start + nchar(seq) - 1,
             sequence_id = i,
             match_start_percent = round((match_start / max_length * 100)/10)*10,
             match_end_percent = round((match_end / max_length * 100)/10)*10 
      )%>%
      select(id, match_start_percent, match_end_percent, sequence_id)
    
    # Store the results
    matches[[i]] <- matches_found
  }
  
  # Combine all matches into a single data frame
  df_matches <- bind_rows(matches)
  
  # Calculate count of each sequence's appearance
  sequence_counts <- df_matches %>%
    group_by(sequence_id) %>%
    summarise(count = n()) %>%
    mutate(
      sequence = query_df$query_seq[sequence_id],
      name = if_else(
        str_length(query_df$query_name[sequence_id]) > 20,
        str_c(
          str_sub(query_df$query_name[sequence_id], 1, 7), "...", str_sub(query_df$query_name[sequence_id], -3, -1)
        ),
        query_df$query_name[sequence_id]
      ),
      label = paste(
        name, "\n",
        str_c(
          str_sub(sequence, 1, 5), "...", str_sub(sequence, -3, -1)
        ), "\n n: ", count
      )
    )
  
  # Join back to df_matches for labeling
  df_matches <- df_matches %>%
    left_join(sequence_counts, by = "sequence_id") %>%
    mutate(color = setNames(colors_list, seq_along(query_df$query_seq))[as.character(sequence_id)])
  
  # Generate the plot
  plot <- ggplot() +
    geom_bar(data = df, aes(x = as.numeric(id), y = 100), stat = "identity", fill = "#EAEAEA") +
    geom_rect(data = df_matches, aes(xmin = as.numeric(id) + 0.5, 
                                     xmax = as.numeric(id) - 0.5,
                                     ymin = match_start_percent, 
                                     ymax = match_end_percent, 
                                     fill = color, 
                                     group = id),
              stat = "identity") +
    scale_fill_identity() +
    theme_bw() +
    labs(x = "Oligopaint number in library", y = "Oligopaint length") +
    coord_flip() +
    scale_x_reverse() +
    geom_text(data = df_matches %>% distinct(sequence_id, .keep_all = TRUE),
              aes(x = as.numeric(id), y = match_start_percent, label = label),
              hjust = 0, vjust = +1.1, size = 2.8, color = "black") +
    ylim(c(0, 120))+
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_blank())+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # return plot and matches
  return(list(plot = plot, matches = df_matches))
}

find_and_plot_matches(df, query_df)['plot']
