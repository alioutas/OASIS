library(tidyverse)
library(RColorBrewer)
library(ggplot2)

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
# 
# query_seq <- c('GATCGGGTCCCACAACCACG', #43
#                'GTGCCAGGCAACCCGTACAG', #44
#                 'GAACGGCGTCACGCTGAGAG', #45
#                'CGACCGGACACACCTCCTCC', #46
#                'CTATCAGGGCAACCCGCAGG') #47
# 
# 
# query_name <- c('This_is_43This_is_43This_is_43',
#                 'This_is_44This_is_44This_is_44',
#                 'This_is_45This_is_45This_is_45',
#                 'This_is_46This_is_46This_is_46',
#                 'This_is_47This_is_47This_is_47')
# 
# query_df <- tibble(query_seq, query_name)
# 
# 




find_and_plot_matches <- function(df, query_df) {
  if (!"id" %in% names(df)) {
    df$id <- seq_len(nrow(df))
  }
  
  if (!"appended_oligopaint" %in% names(df)) {
    stop("The dataframe does not have an 'appended_oligopaint' column.")
  }
  
  df <- df %>% mutate(length = nchar(appended_oligopaint))
  max_length <- max(df$length, na.rm = TRUE)
  
  matches <- list()
  
  for (i in seq_along(query_df$query_seq)) {
    seq <- query_df$query_seq[i]
    matches_found <- df %>%
      mutate(match_found = str_detect(appended_oligopaint, fixed(seq))) %>%
      filter(match_found) %>%
      mutate(
        match_start = str_locate(appended_oligopaint, fixed(seq))[, 1],
        match_end = match_start + nchar(seq) - 1,
        sequence_id = i,
        match_start_percent = ((match_start / max_length * 100) / 10) * 10,
        match_end_percent = ((match_end / max_length * 100) / 10) * 10
      ) %>%
      select(id, match_start_percent, match_end_percent, sequence_id)
      
    matches_found$match_start_percent <- min(matches_found$match_start_percent)
    matches_found$match_end_percent <- min(matches_found$match_end_percent)
    
    matches[[i]] <- matches_found
  }
  
  df_matches <- bind_rows(matches)
  
  sequence_counts <- df_matches %>%
    group_by(sequence_id) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(
      sequence = query_df$query_seq[sequence_id],
      name = if_else(
        str_length(query_df$query_name[sequence_id]) > 20,
        str_c(
          str_sub(query_df$query_name[sequence_id], 1, 7), "...", str_sub(query_df$query_name[sequence_id], -3, -1),
          paste0("(", count, ")")
        ),
        paste(query_df$query_name[sequence_id] ,
        paste0("(", count, ")"))
      ),
      label = paste(name, "\n", str_c(str_sub(sequence, 1, 5), "...", str_sub(sequence, -3, -1)))
    )
  
  df_matches <- df_matches %>%
    left_join(sequence_counts, by = "sequence_id")
  
  # Generate a color list for each sequence_id, ensuring there's enough colors
  num_unique_ids <- length(unique(df_matches$sequence_id))
  if (num_unique_ids > 8) {
    colors_list <- colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(num_unique_ids)
  } else {
    colors_list <- RColorBrewer::brewer.pal(num_unique_ids, "Accent")
  }
  
  color_mapping <- setNames(colors_list, unique(df_matches$sequence_id))
  
  plot <- ggplot() +
    geom_bar(data = df, aes(x = as.numeric(id), y = 100), stat = "identity", fill = "#EAEAEA") +
    geom_rect(data = df_matches, aes(xmin = as.numeric(id) + 0.5, xmax = as.numeric(id) - 0.5,
                                     ymin = match_start_percent, ymax = match_end_percent, fill = factor(sequence_id)),
              stat = "identity") +
    scale_fill_manual(values = color_mapping) +
    theme_bw() +
    labs(x = "Oligopaint number in library", y = "Oligopaint length") +
    coord_flip() +
    scale_x_reverse() +
    geom_text(data = df_matches %>% distinct(sequence_id, .keep_all = TRUE),
              aes(x = as.numeric(id), y = match_start_percent, label = label),
              hjust = 0, vjust = +1.1, size = 2.8, color = "black") +
    ylim(c(0, 120))+
    theme_minimal()+
    theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  return(list(plot = plot, matches = df_matches))
}

# find_and_plot_matches(df, query_df)$'plot'
