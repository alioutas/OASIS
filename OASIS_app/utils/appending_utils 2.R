# Create Pairs of oligoSTORM sequences   -------------------------------------------------------------------
## and select from toes and streets files the seqeuences.
create_pairs <- function(data = comb_ops(), ms_input = input$append_streets_uni_ms, bs_input = input$append_streets_uni_bs, ms_id = "id_uni_ms", bs_id = "id_uni_bs", .streets = streets(), .toes = toes(), .available_os_barcodes = available_os_barcodes(), .matched_streets = matched_streets()) { #, .previous_df = NULL
  require(tidyverse)
  ############## TO DO
  # - See if you can find a way to be consistent in the barcode appended for names, maybe only do the os pairs for universals and not for the rest
  # 
  
  # if(!is.null(.previous_df) ){
  #   .available_os_barcodes <- setdiff(.available_os_barcodes, unique(c(.previous_df$ms_num, .previous_df$bs_num)))
  # }
  
  # BOTH MS and BS need to be appended
  if (!is.null(ms_input) && !is.null(bs_input)) {
    if ((ms_input == "toe_seq_im" | ms_input == "seq_im") && (bs_input == "toe_seq_im" | bs_input == "seq_im")) {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]]) #%>%
      # rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE)) %>%
      # rename_with(~gsub({{ bs_id }}, "bs", .x, fixed = TRUE))
      df$ms_num <- NA
      df$bs_num <- NA
      
      # df contains two columns ms and bs each one containing the respective unique ids of the regions of interest
      # NAs are added to the following columns, these NAs will be replaced by matched os pairs
      
      
      for (i in 1:nrow(df)) {
        if (i == 1) {
          df$ms_num[i] <- .available_os_barcodes[1]
          all_num <-
            .matched_streets$main[.matched_streets$key == .available_os_barcodes[1]]
          all_num <- intersect(all_num, .available_os_barcodes)
          .available_os_barcodes <- .available_os_barcodes[-1]
          df$bs_num[i] <- all_num[1]
          .available_os_barcodes <- .available_os_barcodes[-1]
        } else{
          if (df$ms[i] != df$ms[i - 1] && df$bs[i] != df$bs[i - 1]) {
            df$ms_num[i] <- .available_os_barcodes[1]
            all_num <-
              .matched_streets$main[.matched_streets$key == .available_os_barcodes[1]]
            all_num <- intersect(all_num, .available_os_barcodes)
            .available_os_barcodes <- .available_os_barcodes[-1]
            df$bs_num[i] <- all_num[1]
            .available_os_barcodes <-
              .available_os_barcodes[-which(.available_os_barcodes == all_num[1])]
          } else if (df$ms[i] == df$ms[i - 1] &&
                     df$bs[i] != df$bs[i - 1]) {
            df$ms_num[i] <- df$ms_num[i - 1]
            all_num <-
              .matched_streets$main[.matched_streets$key == df$ms_num[i - 1]]
            all_num <- intersect(all_num, .available_os_barcodes)
            df$bs_num[i] <- all_num[1]
            .available_os_barcodes <-
              .available_os_barcodes[-which(.available_os_barcodes == all_num[1])]
          } else if (df$ms[i] != df$ms[i - 1] &&
                     df$bs[i] == df$bs[i - 1]) {
            df$bs_num[i] <- df$bs_num[i - 1]
            all_num <-
              .matched_streets$main[.matched_streets$key == df$bs_num[i - 1]]
            all_num <- intersect(all_num, .available_os_barcodes)
            df$ms_num[i] <- all_num[1]
            .available_os_barcodes <-
              .available_os_barcodes[-which(.available_os_barcodes == all_num[1])]
          }
        }
      }
      df$ms_street <- .streets[df$ms_num, ]$streets
      df$ms_toe <- .toes[df$ms_num, ]$toes
      df$bs_street <- .streets[df$bs_num, ]$streets
      df$bs_toe <- .toes[df$bs_num, ]$toes
      
    } else if ((ms_input == "toe_seq_im" | ms_input == "seq_im") && bs_input == "ofq") {
      df_os <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df_os$ms_num <- .available_os_barcodes[1:nrow(df_os)]
      df_os$ms_street <- .streets[df_os$ms_num, ]$streets
      df_os$ms_toe <- .toes[df_os$ms_num, ]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-df_os$ms_num]
      
      df_ofq <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      df_ofq$bs_ofq_key <- 1:nrow(df_ofq)
      df_ofq$bs_ofq_seq_primer <- .available_os_barcodes[1]
      .available_os_barcodes <-
        .available_os_barcodes[-df_ofq$bs_ofq_seq_primer]
      
      df_pairs <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      
      df <- df_pairs %>%
        left_join(df_os, by = "ms") %>%
        left_join(df_ofq, by = "bs")
      
    } else if (ms_input == "ofq" && (bs_input == "toe_seq_im" | bs_input == "seq_im")) {
      df_os <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df_os$bs_num <- .available_os_barcodes[1:nrow(df_os)]
      df_os$bs_street <- .streets[df_os$bs_num, ]$streets
      df_os$bs_toe <- .toes[df_os$bs_num, ]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-df_os$bs_num]
      
      df_ofq <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      df_ofq$ms_ofq_key <- 1:nrow(df_ofq)
      df_ofq$ms_ofq_seq_primer <- .available_os_barcodes[1]
      .available_os_barcodes <-
        .available_os_barcodes[-df_ofq$ms_ofq_seq_primer]
      
      df_pairs <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      
      df <- df_pairs %>%
        left_join(df_ofq, by = "ms") %>%
        left_join(df_os, by = "bs")
      
    }
    if (ms_input == "ofq" &&  bs_input == "ofq") {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      df$ms_ofq_key <- 1:nrow(df)
      df$ms_ofq_seq_primer <- .available_os_barcodes[1]
      df$bs_ofq_key <- nrow(df):(nrow(df) * 2 - 1)
      df$bs_ofq_seq_primer <- .available_os_barcodes[2]
      .available_os_barcodes <- .available_os_barcodes[-c(1, 2)]
    }
    
    if(str_detect(bs_id, "uni")){
      names(df) <- gsub("ms", "uni_ms",names(df), fixed = TRUE)
      names(df) <- gsub("bs", "uni_bs",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "uni_ms",.x, fixed = TRUE)) %>% 
      #   rename_with(~gsub("bs", "uni_bs",.x, fixed = TRUE))
    } else if(str_detect(bs_id, "1")){
      names(df) <- gsub("ms", "ms1",names(df), fixed = TRUE)
      names(df) <- gsub("bs", "bs1",names(df), fixed = TRUE)
      # df <- df %>% 
      #   rename_with(~gsub("ms", "ms1",.x, fixed = TRUE)) %>% 
      #   rename_with(~gsub("bs", "bs1",.x, fixed = TRUE))
    } else if(str_detect(bs_id, "2")){
      names(df) <- gsub("ms", "ms2",names(df), fixed = TRUE)
      names(df) <- gsub("bs", "bs2",names(df), fixed = TRUE)
      # df <- df %>% 
      #   rename_with(~gsub("ms", "ms2",.x, fixed = TRUE)) %>% 
      #   rename_with(~gsub("bs", "bs2",.x, fixed = TRUE))
    }
    
    # ONLY MS needs to be appended
  } else if (!is.null(ms_input) && is.null(bs_input)) {
    if (ms_input == "toe_seq_im" | ms_input == "seq_im") {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]]) #%>% #.[[rlang::as_name(enquo(ms_id))]]
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df$ms_num <- .available_os_barcodes[1:nrow(df)]
      df$ms_street <- .streets[df$ms_num,]$streets
      df$ms_toe <- .toes[df$ms_num,]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-df$ms_num]
    } else if (ms_input == "ofq") {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      df$ms_ofq_key <- 1:nrow(df)
      df$ms_ofq_seq_primer <- .available_os_barcodes[1]
      .available_os_barcodes <-
        .available_os_barcodes[-df$ms_ofq_seq_primer]
    }

    if(str_detect(ms_id, "uni")){
      #names(df) <- paste0("uni_", names(df))
      names(df) <- gsub("ms", "uni_ms",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "uni_ms",.x, fixed = TRUE))
    } else if(str_detect(ms_id, "1")){
      #names(df) <- paste0(names(df), "1")
      names(df) <- gsub("ms", "ms1",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "ms1",.x, fixed = TRUE))
    } else if(str_detect(ms_id, "2")){
      #names(df) <- paste0(names(df), "2")
      names(df) <- gsub("ms", "ms2",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "ms2",.x, fixed = TRUE))
    }

    # ONLY BS needs to be appended
  } else if (is.null(ms_input) && !is.null(bs_input)) {
    if (bs_input == "toe_seq_im" | bs_input == "seq_im") {
      df <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df$bs_num <- .available_os_barcodes[1:nrow(df)]
      df$bs_street <- .streets[df$bs_num,]$streets
      df$bs_toe <- .toes[df$bs_num,]$toes
      .available_os_barcodes <- .available_os_barcodes[-df$bs_num]
    } else if (bs_input == "ofq") {
      df <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      df$bs_ofq_key <- 1:nrow(df)
      df$bs_ofq_seq_primer <- .available_os_barcodes[1]
      .available_os_barcodes <-
        .available_os_barcodes[-df$bs_ofq_seq_primer]
    }

    if(str_detect(bs_id, "uni")){
      names(df) <- gsub("bs", "uni_bs",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("bs", "uni_bs",.x, fixed = TRUE))
    } else if(str_detect(bs_id, "1")){
      names(df) <- gsub("bs", "bs1",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("bs", "bs1",.x, fixed = TRUE))
    } else if(str_detect(bs_id, "2")){
      names(df) <- gsub("bs", "bs2",names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("bs", "bs2",.x, fixed = TRUE))
    }

  }

  
  created_pairs_list <- list("available_barcodes" = .available_os_barcodes, "df" = df)
  return(created_pairs_list)
}
  
  

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


# Generate OligoFISSEQ barcodes   -------------------------------------------------------------------

generate_oligoFISSEQ_barcodes <- function(cycles = 3, sequences = c("GGTCT","TGGTC","AGTCA","CGCTC"), different_last = 2){
  require(tidyverse)
  if(between(cycles,3,5)) {
    pattern_allowed <- "0"
    
  } else if(between(cycles,6,8)){
    pattern_allowed <- "00"
    
  } else if(between(cycles,9,10)){
    pattern_allowed <- "000"
  } else if (cycles > 10){
    break()
  }
  
  l<-list(replicate(1, sequences))
  ll<- rep(l,cycles)
  lll<- rev(expand.grid(ll))
  names(lll) <- c(1:cycles)
  
  m<-list(replicate(1, seq_along(sequences)))
  mm<- rep(m,cycles)
  mmm<- rev(expand.grid(mm))
  names(mmm) <- c(1:cycles)
  
  mmm_lead <- mmm %>%
    rowwise() %>%
    lead()
  
  score <- as_tibble(abs(mmm_lead - mmm) %>%
                       select(-cycles)) %>%
    mutate_all(as.character) 
  
  colnames_score <- colnames(score)
  score$all <- apply( score[ , colnames_score ] , 1 , paste0 , collapse = "" )
  
  
  score <- score %>% 
    mutate(freq_pattern = str_count(.$all,pattern_allowed))
  score$zeros <- rowSums(score == 0)
  barcodes_split <- lll[score$freq_pattern < different_last,]
  #barcodes_split <- mmm[score$freq_pattern < (different_last-1),]
  #names(barcodes)
  barcodes <- unite(barcodes_split, sequence, 1:cycles, sep="")
  barcodes$code <- row.names(barcodes)
  barcodes
}


# append_ms_OS_bs_OS <- function(ms = ms, bs = bs, avoid_until = avoid_until, available_barcodes = available_barcodes, matched_streets = matched_streets, mode = c("seq_im", "toe_seq_im")){
#   
#   # Set available seq barcodes
#   if (mode == "seq_im"){
#     total_streets <- c(1:nrow(streets)) 
#   } else if (mode == "toe_seq_im") {
#     total_streets <- c(1:nrow(toes)) 
#   }
#   
#   # bind main streets to back streets
#   MS_BS <- cbind(ms, bs)
#   # group MS and BS 1
#   MS_BS_grouped <- MS_BS %>% 
#     group_by(chr, start,end,contains(id_ms),contains(id_bs)) %>% 
#     group_nest()
#   
# 
# 
# }
# 
# append_ms_OS_bs_OF
# 
# append_ms_OF_bs_OS





bedTools.2in<-function(functionstring="bedIntersect",bed1,bed2,opt.string="")
{
  #create temp files
  a.file=tempfile()
  b.file=tempfile()
  out   =tempfile()
  options(scipen =99) # not to use scientific notation when writing out
  
  #write bed formatted dataframes to tempfile
  write.table(bed1,file=a.file,quote=F,sep="\t",col.names=F,row.names=F)
  write.table(bed2,file=b.file,quote=F,sep="\t",col.names=F,row.names=F)
  
  # create the command string and call the command using system()
  command=paste(functionstring,"-a",a.file,"-b",b.file,opt.string,">",out,sep=" ")
  cat(command,"\n")
  try(system(command))
  
  res=read.table(out,header=F)
  unlink(a.file);unlink(b.file);unlink(out)
  return(res)
}


make_summary <- function(df) {
  datatable(
    df %>% 
      group_by(chr,across(contains("id"))) %>% 
      summarise(n = n(), size_kb = round((max(end) - min(start))/1000), density_kb = round(n/size_kb, digits =1)) %>%
      print(n = Inf),
    extensions = c('Scroller','FixedColumns'), options = list(
      deferRender = FALSE,
      scrollY = 200,
      scroller = TRUE,
      dom = 't',
      scrollX = TRUE,
      fixedColumns = TRUE,
      scrollX = TRUE))
}


make_table <- function(df) {
  datatable(
    df,
    extensions = c('Scroller','FixedColumns'), options = list(
      deferRender = FALSE,
      scrollY = 200,
      scroller = TRUE,
      dom = 't',
      scrollX = TRUE,
      fixedColumns = TRUE,
      scrollX = TRUE))
}



bedtools_merge_test <- function(x){
  # This function identifies overlapping coordinated in user uploaded bed files
  # The input is a bed file and the output is TRUE (same) or FALSE (different) row numbers 
  # between x and merged x (bedtools) 
  #require(RBedtools)
  nrow(x) == nrow(to_data_frame(RBedtools(tool = 'merge',
                                          i= from_data_frame(x))))
}



intersect_coordinates <- function(y, x, correct_coor = T){
  require(tidyverse)
  require(RBedtools)
  
  # if(correct_coor) {
  #   x <- x %>%
  #     group_by(X1) %>%
  #     mutate(X2 = case_when(
  #       lag(X3) >= X2   ~ (lag(X3))+1L,
  #       TRUE ~ X2)) %>%
  #     ungroup()
  # }
  
  # TO FIX make correct ordering of chromosomes
  # x <- x %>% 
  #   mutate(order = as.numeric(str_replace(x %>% pull(contains("chr")), "chr", ""))) %>% 
  #   arrange(order) %>% 
  #   select(-order)
  
  x %>% from_data_frame %>% 
    RBedtools('intersect',options = '-wa -wb', a=., b=from_data_frame(y)) %>% to_data_frame
  
  #RBedtools(tool = 'intersect',options = '-wa -wb', a=from_data_frame(x), b=from_data_frame(y)) %>% to_data_frame
  
} 




# ref_genome <- fread("~/Google Drive/HMS/general_lab/Vutara_related/scripts/appending/appending/appending/genome_coordinates/hg38.txt")
# chr_ROI_ms <- fread("~/Google Drive/HMS/HCR/final/oligopaint_probes/chr06_ER_oligopaints_MS_4641_sorted_wo_appending_final_unique.bed", col.names = c("chr_ms", "start_ms", "end_ms", "id_ms", "chr", "start", "end", "sequence", "Tm"))
# chr_ROI_bs <- fread("~/Google Drive/HMS/HCR/final/oligopaint_probes/chr06_ER_oligopaints_BS_4641_sorted_wo_appending_final_unique.bed", col.names = c("chr_bs", "start_bs", "end_bs", "id_bs", "chr", "start", "end", "sequence", "Tm"))
# chr_ROI_c <- inner_join(chr_ROI_ms, chr_ROI_bs)
# 
# chr_ROI_ms_sum <- chr_ROI_c %>%
#   group_by(chr) %>%
#   group_by_at(vars(starts_with("id")), .add = T) %>%
#   summarise(start = min(start), end = max(end))
# 
# 
# chr_ROI_ms_nest <- chr_ROI_ms %>%
#   group_by(chr) %>%
#   group_by_at(vars(starts_with("id")), .add = T) %>%
#   group_nest() 
# 
# 
# chr_ROI_ms_all <- tibble(inner_join(chr_ROI_ms_sum, chr_ROI_ms_nest)) %>%
#   mutate(n_probes=map_dbl(.$data, nrow), density = round(n_probes/(end-start)*1000, digits = 1), size_kb = round((end-start)/1000))%>% 
#   select(-data, chr, start, end, starts_with("id")) %>% mutate(chr = as_factor(chr))
# 
# 
# ref_genome <- tibble(ref_genome) %>% mutate(chr = as_factor(chr))
# levels_chr <-  factor(x = ref_genome$chr, levels = rev(ref_genome$chr))
# 
# chr_ROI_ms_all$chr <- factor(x = chr_ROI_ms_all$chr, 
#                              levels = levels_chr)
# 
# ref_genome$chr <- factor(x = ref_genome$chr, 
#                          levels = levels_chr)
# 
# plot_chrom(ref_genome, chr_ROI)
# 
# plot_chrom <- function(ref_genome, chr_ROI){
#   require(ggchicklet)
#   
#   ref_genome <- tibble(ref_genome) %>% mutate(chr = as_factor(chr))
#   
  
#   
#   formatter1000000 <- function(x){ 
#     x/1000000 
#   }
# 
#   p <- ggplot(data=ref_genome) +
#     #geom_bar(aes(x = as.numeric(fct_rev(chr)), y = end),stat="identity", fill = "#EAEAEA") +
#     geom_chicklet(aes(x = as.numeric(fct_rev(chr)), y = end),  fill = "#EAEAEA", radius = grid::unit(6, "mm")) +
#     coord_flip() +
#     theme(axis.title.x = element_text(color="black", size=14, face="bold"),
#           axis.title.y = element_text(color="black", size=14, face="bold"),
#           axis.text.x = element_text(colour = "black", size=15),
#           axis.text.y = element_text(colour = "black", size=15),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(), 
#           panel.background = element_blank()) +
#     scale_x_discrete(name = "Chromosomes", limits = levels(fct_rev(ref_genome$chr))) +
#     scale_y_continuous(name= "Genomic distance Mb", limits = c(min(ref_genome$start)-1,max(ref_genome$end)+1), labels = formatter1000000)
#   
#   
#   
#   
#   
#   p<- p + geom_rect(data=chr_ROI_ms_all, 
#                     aes(xmin=as.numeric(fct_rev(chr)) + 0.2, 
#                         xmax=as.numeric(fct_rev(chr)) - 0.2, 
#                         ymin=start, 
#                         ymax=end, 
#                         # color = density,
#                         # fill = as_factor(contains("id"))
#                     ), 
#                     # size=1,
#                     inherit.aes = FALSE) +
#     #scale_color_gradientn(colours = c("#63ACBE", "#EE442F"))+
#     #scale_fill_gradientn(colours = c("#63ACBE", "#EE442F"), guide = F)+
#     labs(color="Density per Kb", fill = NULL)
#   
#   
#   p <- p + geom_rect(data = ref_genome,
#                      aes(xmin=as.numeric(fct_rev(chr))+0.45, 
#                          xmax=as.numeric(fct_rev(chr))-0.45, 
#                          ymin=cent_start, 
#                          ymax=cent_end),fill = I("white"),
#                      inherit.aes = FALSE)
#   
#   p + geom_text(data = chr_ROI_ms_all,
#                 aes(x = as.numeric(fct_rev(chr)),
#                     y = start ,
#                     label = factor(id)),
#                 size = 6,
#                 nudge_x      = 0.3,
#                 check_overlap = T)
# }





