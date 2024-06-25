choices_append <- list(
  "Sequential OligoSTORM" = "toe_seq_im",
  "OligoSTORM" = "seq_im",
  "OligoFISSEQ" = "ofq"#,
  #"lambdaFISH" = "lambdaFISH"
)
# default lambda sequences
default_lambda <- data.frame(
  bitName = c("lambda1", 
              #"lambda2", 
              "lambda3", 
              #"lambda4", 
              "lambda5", 
              "lambda6"),
  key = c("1", "2", "3", "4"),
  Sequence = c("CACCGACGTCGCATAGAACGGAAGAGCGTGTG",
               #"AGAACGATCCAGCGAGATCAAGTGGAGCTGCG",
               "CGAGCCAGGTCATCCTAGCCCATACGGCAATG",
               #"GCATTCACCCTTGCACGATACCGAGCCACACC",
               "AGCGCAGGAGGTCCACGACGTGCAAGGGTGT",
               "CACACGCTCTCCGTCTTGGCCGTGGTCGATCA")
)

# default ofq sequences
default_ofq <- data.frame(
  bitName = c("OFQ1", 
              "OFQ2",
              "OFQ3",
              "OFQ4"),
  Sequence = c("GGTCT", 
               "TGGTC", 
               "AGTCA", 
               "CGCTC"))


# default secondary sequences
default_sec <- data.frame(
  bitName = c("sec1", "sec2", "sec3", "sec4", "sec5", "sec6"),
  Sequence = c("CACCGACGTCGCATAGAACGGAAGAGCGTGTG",
               "CGCAGCTCCACTTGATCTCGCTGGATCGTTCT",
               "CGAGCCAGGTCATCCTAGCCCATACGGCAATG",
               "GGTGTGGCTCGGTATCGTGCAAGGGTGAATGC",
               "TAGCGCAGGAGGTCCACGACGTGCAAGGGTGT",
               "CACACGCTCTCCGTCTTGGCCGTGGTCGATCA"))

# default activator secondary sequences
default_actsec <- data.frame(
  bitName = c("sec405"),
  Sequence = c("GGTCTTACAGCGGCGCAATG"))


# oligopaints table
default_oligopaints <- data.frame(
  Name = c("hg38 newBalance", 
           "hg19 newBalance", 
           "chm13 newBalance", 
           "mm39 newBalance",
           "mm10 newBalance",
           "mm9 newBalance",
           "dm6 newBalance",
           "ce11 newBalance",
           "danRer11 newBalance",
           "TAIR10 newBalance",
           "sacCer3 newBalance",
           "rn6 newBalance",
           "galGal5 newBalance",
           "galGal6 newBalance",
           "rheMac10 newBalance",
           "xenTro10 newBalance",
           "Nfu_20140520 newBalance",
           "AaegL5.0 newBalance",
           "susScr11 newBalance",
           "ASM694v2 newBalance",
           "sScyCan1.1 newBalance",
           "OligoMiner hg38 balance",
           "OligoMiner hg19 balance",
           "2012 oligopaints hg19",
           "iFISH4U 40-mer hg19"),
  link = c("http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/hg38_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/hg19_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/chm13_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/mm39_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/mm10_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/mm9_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/dm6_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/ce11_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/danRer11_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/TAIR10_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/sacCer3_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/rn6_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/galGal5_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/galGal6_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/rheMac10_all_newBalance.zip1",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/xenTro10_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/Nfu_20140520_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/AaegL5.0_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/susScr11_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/ASM694v2_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/sScyCan1.1_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/hg38b_all_newBalance.zip1",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/hg19b_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/hg19_2012_all_newBalance.zip",
    "http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/iFISH4U_all_newBalance.zip")
  # links from Antonios Lioutas Dropbox
  # link = c(#"https://www.dropbox.com/scl/fi/0b2zyqjpsb0p9rjvj8yzc/hg38_all_newBalance.zip?rlkey=cn7vr23qvzqsicd2fqonqns05&dl=1",
  #          'http://hmsrsc-wulab-data.s3.amazonaws.com/oligopaints/hg38_all_newBalance.zip',
  #          "https://www.dropbox.com/scl/fi/s1xozcdmahgr21cuc7rc9/hg19_all_newBalance.zip?rlkey=e9systtyq0s3njdrm9icadm4p&dl=1",
  #          "https://www.dropbox.com/scl/fi/grqcj57i3rezpj0j1xw4n/chm13_all_newBalance.zip?rlkey=k9iwbgnd8h8hkukbcgqqtml5m&dl=1",
  #          "https://www.dropbox.com/scl/fi/jndqqj3jrnjtplmbiop73/mm39_all_newBalance.zip?rlkey=fe1fh6kh1cp4yvjetd6evhwly&dl=1",
  #          "https://www.dropbox.com/scl/fi/stcg03kj9esgw5h6w3eve/mm10_all_newBalance.zip?rlkey=2r8lj18b80o7w7mq43bf0wtvn&dl=1",
  #          "https://www.dropbox.com/scl/fi/k3rpaumj8ajvlcb7jbvis/mm9_all_newBalance.zip?rlkey=1scdxee5k82ff4a3bovjewr8l&dl=1",
  #          "https://www.dropbox.com/scl/fi/swxbdzxrufivke4jickov/dm6_all_newBalance.zip?rlkey=9bzz2vwoy88cax1uh4evrxubv&dl=1",
  #          "https://www.dropbox.com/scl/fi/ygyndahitvef71mgcw3ct/ce11_all_newBalance.zip?rlkey=i3o1zybfrs3olwt44ggr2iuzu&dl=1",
  #          "https://www.dropbox.com/scl/fi/39l9i8sv3b4q5rgoia5ui/danRer11_all_newBalance.zip?rlkey=54oqf23rzfvsd5vgqgs8dnah7&dl=1",
  #          "https://www.dropbox.com/scl/fi/tvc508cy2bexmmliaa4hy/TAIR10_all_newBalance.zip?rlkey=4udybokzbvz8j285ywf8r3cfm&dl=1",
  #          "https://www.dropbox.com/scl/fi/e4pcksr2ss4qhhe1dr3fy/sacCer3_all_newBalance.zip?rlkey=opyv39166f27f7y0taq90nvh0&dl=1",
  #          "https://www.dropbox.com/scl/fi/np8801n6a05a9r816tor5/rn6_all_newBalance.zip?rlkey=i3tlc4puarshmunsccw50ys7s&dl=1",
  #          "https://www.dropbox.com/scl/fi/mf2sktcd9bh46zc0odltr/galGal5_all_newBalance.zip?rlkey=iv4pkv6yijlu055vvwwkl282h&dl=1",
  #          "https://www.dropbox.com/scl/fi/9xzivtglqi26uh05mmot3/galGal6_all_newBalance.zip?rlkey=tys965xj2yp8cd3vv2hqvb3fp&dl=1",
  #          "https://www.dropbox.com/scl/fi/ns4lnfd6l8rc9srfs2dpa/rheMac10_all_newBalance.zip?rlkey=1loz57thphnf5w3t5xbkbtl5t&dl=1",
  #          "https://www.dropbox.com/scl/fi/5trswh7wzts0d7lk939hu/xenTro10_all_newBalance.zip?rlkey=arvfdmo98qgzp2cydpcuu1wtm&dl=1",
  #          "https://www.dropbox.com/scl/fi/xxsgo4w79tcanzv0d13eu/Nfu_20140520_all_newBalance.zip?rlkey=4xfls4sm2vv6u7jgtqzh87810&dl=1",
  #          "https://www.dropbox.com/scl/fi/9sgu0wl1weo3r6ji2im5k/AaegL5.0_all_newBalance.zip?rlkey=pd2vbp9kug30qyoj6gqocd7sr&dl=1",
  #          "https://www.dropbox.com/scl/fi/0vxj10vvad6rv3aduojdx/susScr11_all_newBalance.zip?rlkey=19wktl3uu2pern1ae8kl1zmt4&dl=1",
  #          "https://www.dropbox.com/scl/fi/at3bbj9n08zt0krtjih72/ASM694v2_all_newBalance.zip?rlkey=xh5tl0tdj6g4tq887h8jn56vb&dl=1",
  #          "https://www.dropbox.com/scl/fi/9ycqfam4b5sdvzmgc8i4i/sScyCan1.1_all_newBalance.zip?rlkey=czb8qwvhs3rw1el3n5v6bga00&dl=1",
  #          "https://www.dropbox.com/scl/fi/2teqfar9d1f9q60mnuq1k/hg38b_all_newBalance.zip?rlkey=eu20zqga9nxjbbivy2ivmw2hd&dl=1",
  #          "https://www.dropbox.com/scl/fi/uo4mkwc1ejat452fym64g/hg19b_all_newBalance.zip?rlkey=ya169ray41n8pbtrukzlskesz&dl=1",
  #          "https://www.dropbox.com/scl/fi/u2yzjxrullb1tmiks0hz4/hg19_2012_all_newBalance.zip?rlkey=7202f7uyjgmilkanz4rvouwbs&dl=1",
  #          "https://www.dropbox.com/scl/fi/4iravbdz8p0v0659tup0k/iFISH4U_all_newBalance.zip?rlkey=il7wc1ibrf4fkv6ugvpcl39hq&dl=1")
  # links from PaintSHOP
  # link = c("https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/hg38_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/hg19_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/chm13_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/mm39_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/mm10_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/mm9_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/dm6_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/ce11_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/danRer11_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/TAIR10_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/sacCer3_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/rn6_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/galGal5_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/galGal6_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/rheMac10_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/xenTro10_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/Nfu_20140520_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/AaegL5.0_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/susScr11_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/ASM694v2_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/sScyCan1.1_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/hg38b_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/hg19b_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/hg19_2012_all_newBalance.zip",
  #          "https://paintshop-bucket.s3.amazonaws.com/v1.2/resources/all/iFISH4U_all_newBalance.zip"           
  #          )
)

# T7 sequence

T7 <- "TAATACGACTCACTATAGGG"


# Create Pairs of oligoSTORM sequences   -------------------------------------------------------------------
## and select from toes and streets files the seqeuences.
create_pairs <- function(data = comb_ops(),
                         ms_input = input$append_streets_uni_ms,
                         bs_input = input$append_streets_uni_bs,
                         ms_id = "id_uni_ms",
                         bs_id = "id_uni_bs",
                         .streets = streets(),
                         .toes = toes(),
                         .available_os_barcodes = available_os_barcodes(),
                         .available_multiplex_barcodes = multiplex_list(),
                         .matched_streets = matched_streets(),
                         input_ofq_bits = input_sec_ofq_reactive(), 
                         input_lambda_bits = input_sec_lambda_reactive, 
                         bs_rc = FALSE) {
  #, .previous_df = NULL
  require(tidyverse)

  #transform id to character from list
  .available_multiplex_barcodes$id <- as.character(.available_multiplex_barcodes$id)
  
  #####################################
  # BOTH MS and BS needs to be appended
  #####################################
  if (!is.null(ms_input) && !is.null(bs_input)) {
    if ((ms_input == "toe_seq_im" | ms_input == "seq_im") &&
        (bs_input == "toe_seq_im" | bs_input == "seq_im")) {
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
        } else {
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
      df$ms_street <- .streets[df$ms_num,]$streets
      df$ms_toe <- .toes[df$ms_num,]$toes
      df$bs_street <- ifelse(bs_rc, rc(.streets[df$bs_num,]$streets), .streets[df$bs_num,]$streets)
      df$bs_toe <- ifelse(bs_rc, rc(.toes[df$bs_num,]$toes),.toes[df$bs_num,]$toes)
      
    } else if ((ms_input == "toe_seq_im" | ms_input == "seq_im") &&
               (bs_input == "ofq" | bs_input == "lambdaFISH")) {
      df_os <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df_os$ms_num <- .available_os_barcodes[1:nrow(df_os)]
      df_os$ms_street <- .streets[df_os$ms_num,]$streets
      df_os$ms_toe <- .toes[df_os$ms_num,]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df_os$ms_num)]
      
      
      
      
      if (bs_input == "ofq") {
        df_multiplex <- data %>%
          ungroup() %>%
          distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
        # df$bs_ofq_key <- .available_multiplex_barcodes$barcode[1:nrow(df)]
        df_multiplex <-
          left_join(df_multiplex,
                    .available_multiplex_barcodes,
                    by = c("bs" = "id")) %>%
          rename(bs_ofq_key = barcode)
        
        df_multiplex$bs_ofq_seq_primer_num <-
          .available_os_barcodes[1]
        df_multiplex$bs_ofq_seq_primer <-
          .streets[df_multiplex$bs_ofq_seq_primer_num, ]$streets
        .available_os_barcodes <-
          .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$bs_ofq_seq_primer_num)]
        
        
        
        # add OFQ barcode sequence
        df_multiplex$bs_ofq_barcode <- as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex$bs_ofq_key), ""))) %>% 
          mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_ofq_bits$Sequence[[1]],
                                                   . == 2 ~ input_ofq_bits$Sequence[[2]],
                                                   . == 3 ~ input_ofq_bits$Sequence[[3]],
                                                   . == 4 ~ input_ofq_bits$Sequence[[4]],
                                                   .default = ""))) %>%
          # bind all columns in one
          unite("ofq_barcode_seq", starts_with("V"), sep = "") %>%
          pull(ofq_barcode_seq)
        
        
      } else if (bs_input == "lambdaFISH") {
        df_multiplex <- data %>%
          ungroup() %>%
          distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
        
        df_multiplex$bs_num <- .available_os_barcodes[1:nrow(df_multiplex)]
        df_multiplex$bs_street <- .streets[df_multiplex$bs_num,]$streets
        df_multiplex$bs_toe <- .toes[df_multiplex$bs_num,]$toes
        
        .available_os_barcodes <-
          .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$bs_num)]
        
        
        
        df_multiplex <-
          left_join(df_multiplex,
                    .available_multiplex_barcodes,
                    by = c("bs" = "id")) %>%
          rename(bs_lambdaFISH_key = barcode)
        # Add same FWD amplification primer to lambdaFISH
        df_multiplex$bs_FWD_primer_num_lambdaFISH <- .available_os_barcodes[1]
        df_multiplex$bs_FWD_primer_lambdaFISH <- .streets[df_multiplex$bs_FWD_primer_num_lambdaFISH, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$bs_FWD_primer_num_lambdaFISH)]
        
        # Determine the number of REV primers based on the first element of bs_lambdaFISH_key
        num_rev_primers <- nchar(df_multiplex$bs_lambdaFISH_key[1])
        
        # Update .available_os_barcodes to get the REV primers
        bs_lambdaFISH_REV_primer_nums <- .available_os_barcodes[1:num_rev_primers]
        
        # Create new columns for each REV primer in df_multiplex
        for (i in seq_len(num_rev_primers)) {
          # Get the REV primer number and primer
          bs_lambdaFISH_REV_primer_num <- bs_lambdaFISH_REV_primer_nums[i]
          bs_lambdaFISH_REV_primer <- .streets[bs_lambdaFISH_REV_primer_num, ]$streets
          
          # Add the new columns to df_multiplex
          df_multiplex <- df_multiplex %>%
            mutate(!!paste0("bs_REV_primer_num_lambdaFISH", i) := bs_lambdaFISH_REV_primer_num,
                   !!paste0("bs_REV_primer_lambdaFISH", i) := bs_lambdaFISH_REV_primer)
        }
        # Update .available_os_barcodes
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% bs_lambdaFISH_REV_primer_nums)]
        
        # Add another column for the toe reverse primer
        df_multiplex$bs_REV_primer_num_lambdaFISHtoe <- .available_os_barcodes[1]
        df_multiplex$bs_REV_primer_lambdaFISHtoe <- .streets[df_multiplex$bs_REV_primer_num_lambdaFISHtoe, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$bs_REV_primer_num_lambdaFISHtoe)]
        
        # Create new columns for each lambda seq in df
        df_multiplex <- df_multiplex %>% 
          bind_cols(., as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex$bs_lambdaFISH_key), ""))) %>%
                      mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_lambda_bits$Sequence[[1]],
                                                               . == 2 ~ input_lambda_bits$Sequence[[2]],
                                                               . == 3 ~ input_lambda_bits$Sequence[[3]],
                                                               . == 4 ~ input_lambda_bits$Sequence[[4]],
                                                               .default = "")))
          ) %>%
          #rename columns containing V to lambda_seq
          rename_with(~ str_replace(., "V", "bs_sec_lambdaFISH"), starts_with("V"))
        
      }

      df_pairs <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      
      df <- df_pairs %>%
        left_join(df_os, by = "ms") %>%
        left_join(df_multiplex, by = "bs")
      
    } else if ((ms_input == "ofq" | ms_input == "lambdaFISH") &&
               (bs_input == "toe_seq_im" | bs_input == "seq_im")) {
      df_os <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df_os$bs_num <- .available_os_barcodes[1:nrow(df_os)]
      
      df_os$bs_street <- ifelse(bs_rc, rc(.streets[df_os$bs_num,]$streets), .streets[df_os$bs_num,]$streets)
      df_os$bs_toe <- ifelse(bs_rc, rc(.toes[df_os$bs_num, ]$toes),.toes[df_os$bs_num, ]$toes)

      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df_os$bs_num)]
      
      
      if (ms_input == "ofq") {
        df_multiplex <- data %>%
          ungroup() %>%
          distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
        # df$ms_ofq_key <- .available_multiplex_barcodes$barcode[1:nrow(df)]
        df_multiplex <-
          left_join(df_multiplex,
                    .available_multiplex_barcodes,
                    by = c("ms" = "id")) %>%
          rename(ms_ofq_key = barcode)
        
        df_multiplex$ms_ofq_seq_primer_num <-
          .available_os_barcodes[1]
        df_multiplex$ms_ofq_seq_primer <-
          .streets[df_multiplex$ms_ofq_seq_primer_num, ]$streets
        .available_os_barcodes <-
          .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$ms_ofq_seq_primer_num)]
        
        
        
        # add OFQ barcode sequence
        df_multiplex$ms_ofq_barcode <- as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex$ms_ofq_key), ""))) %>% 
          mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_ofq_bits$Sequence[[1]],
                                                      . == 2 ~ input_ofq_bits$Sequence[[2]],
                                                      . == 3 ~ input_ofq_bits$Sequence[[3]],
                                                      . == 4 ~ input_ofq_bits$Sequence[[4]],
                                                      .default = ""))) %>%
          # bind all columns in one
          unite("ofq_barcode_seq", starts_with("V"), sep = "") %>%
          pull(ofq_barcode_seq)
        
        
      }else if (ms_input == "lambdaFISH") {
        df_multiplex <- data %>%
          ungroup() %>%
          distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
        
        df_multiplex$ms_num <- .available_os_barcodes[1:nrow(df_multiplex)]
        df_multiplex$ms_street <- .streets[df_multiplex$ms_num,]$streets
        df_multiplex$ms_toe <- .toes[df_multiplex$ms_num,]$toes
        
        .available_os_barcodes <-
          .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$ms_num)]
        
        
        
        df_multiplex <-
          left_join(df_multiplex,
                    .available_multiplex_barcodes,
                    by = c("ms" = "id")) %>%
          rename(bs_lambdaFISH_key = barcode)
        # Add same FWD amplification primer to lambdaFISH
        df_multiplex$ms_FWD_primer_num_lambdaFISH <- .available_os_barcodes[1]
        df_multiplex$ms_FWD_primer_lambdaFISH <- .streets[df_multiplex$ms_FWD_primer_num_lambdaFISH, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$ms_FWD_primer_num_lambdaFISH)]
        
        # Determine the number of REV primers based on the first element of ms_lambdaFISH_key
        num_rev_primers <- nchar(df_multiplex$ms_lambdaFISH_key[1])
        
        # Update .available_os_barcodes to get the REV primers
        ms_lambdaFISH_REV_primer_nums <- .available_os_barcodes[1:num_rev_primers]
        
        # Create new columns for each REV primer in df_multiplex
        for (i in seq_len(num_rev_primers)) {
          # Get the REV primer number and primer
          ms_lambdaFISH_REV_primer_num <- ms_lambdaFISH_REV_primer_nums[i]
          ms_lambdaFISH_REV_primer <- .streets[ms_lambdaFISH_REV_primer_num, ]$streets
          
          # Add the new columns to df_multiplex
          df_multiplex <- df_multiplex %>%
            mutate(!!paste0("ms_REV_primer_num_lambdaFISH", i) := ms_lambdaFISH_REV_primer_num,
                   !!paste0("ms_REV_primer_lambdaFISH", i) := ms_lambdaFISH_REV_primer)
        }
        # Update .available_os_barcodes
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% ms_lambdaFISH_REV_primer_nums)]
        
        # Add another column for the toe reverse primer
        df_multiplex$ms_REV_primer_num_lambdaFISHtoe <- .available_os_barcodes[1]
        df_multiplex$ms_REV_primer_lambdaFISHtoe <- .streets[df_multiplex$ms_REV_primer_num_lambdaFISHtoe, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex$ms_REV_primer_num_lambdaFISHtoe)]
        
        # Create new columns for each lambda seq in df
        df_multiplex <- df_multiplex %>% 
          bind_cols(., as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex$ms_lambdaFISH_key), ""))) %>%
                      mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_lambda_bits$Sequence[[1]],
                                                               . == 2 ~ input_lambda_bits$Sequence[[2]],
                                                               . == 3 ~ input_lambda_bits$Sequence[[3]],
                                                               . == 4 ~ input_lambda_bits$Sequence[[4]],
                                                               .default = "")))
          ) %>%
          #rename columns containing V to lambda_seq
          rename_with(~ str_replace(., "V", "ms_sec_lambdaFISH"), starts_with("V"))
        
      }
      
      df_pairs <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      
      df <- df_pairs %>%
        left_join(df_multiplex, by = "ms") %>%
        left_join(df_os, by = "bs")
      
    } 
    
    if ((ms_input == "ofq" | ms_input == "lambdaFISH") &&
        (bs_input == "ofq" | bs_input == "lambdaFISH")) {
      
      df_multiplex_ms <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      
      df_multiplex_bs <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      
      if (ms_input == "ofq") {
        df_multiplex_ms <-
          left_join(df_multiplex_ms,
                    .available_multiplex_barcodes,
                    by = c("ms" = "id")) %>%
          rename(ms_ofq_key = barcode)
        
        # add OFQ barcode 
        df_multiplex_ms$ms_ofq_seq_primer_num <- .available_os_barcodes[1]
        df_multiplex_ms$ms_ofq_seq_primer <-.streets[df_multiplex_ms$ms_ofq_seq_primer_num, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_ms$ms_ofq_seq_primer_num)]
        
        # add OFQ barcode sequence
        df_multiplex_ms$ms_ofq_barcode <- as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex_ms$ms_ofq_key), ""))) %>% 
          mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_ofq_bits$Sequence[[1]],
                                                   . == 2 ~ input_ofq_bits$Sequence[[2]],
                                                   . == 3 ~ input_ofq_bits$Sequence[[3]],
                                                   . == 4 ~ input_ofq_bits$Sequence[[4]],
                                                   .default = ""))) %>%
          # bind all columns in one
          unite("ofq_barcode_seq", starts_with("V"), sep = "") %>%
          pull(ofq_barcode_seq)
        
      } else if (ms_input == "lambdaFISH") {
        df_multiplex_ms <-
          left_join(df_multiplex_ms,
                    .available_multiplex_barcodes,
                    by = c("ms" = "id")) %>%
          rename(ms_lambdaFISH_key = barcode)
        
        df_multiplex_ms$ms_num <- .available_os_barcodes[1:nrow(df_multiplex_ms)]
        df_multiplex_ms$ms_street <- .streets[df_multiplex_ms$ms_num,]$streets
        df_multiplex_ms$ms_toe <- .toes[df_multiplex_ms$ms_num,]$toes
        .available_os_barcodes <-
          .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_ms$ms_num)]
        
        # Add same FWD amplification primer to lambdaFISH
        df_multiplex_ms$ms_FWD_primer_num_lambdaFISH <- .available_os_barcodes[1]
        df_multiplex_ms$ms_FWD_primer_lambdaFISH <- .streets[df_multiplex_ms$ms_FWD_primer_num_lambdaFISH, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_ms$ms_FWD_primer_num_lambdaFISH)]
        
        # Determine the number of REV primers based on the first element of ms_lambdaFISH_key
        num_rev_primers <- nchar(df_multiplex_ms$ms_lambdaFISH_key[1])
        
        # Update .available_os_barcodes to get the REV primers
        ms_lambdaFISH_REV_primer_nums <- .available_os_barcodes[1:num_rev_primers]
        
        # Create new columns for each REV primer in df_multiplex_ms
        for (i in seq_len(num_rev_primers)) {
          # Get the REV primer number and primer
          ms_lambdaFISH_REV_primer_num <- ms_lambdaFISH_REV_primer_nums[i]
          ms_lambdaFISH_REV_primer <- .streets[ms_lambdaFISH_REV_primer_num, ]$streets
          
          # Add the new columns to df_multiplex_ms
          df_multiplex_ms <- df_multiplex_ms %>%
            mutate(!!paste0("ms_REV_primer_num_lambdaFISH", i) := ms_lambdaFISH_REV_primer_num,
                   !!paste0("ms_REV_primer_lambdaFISH", i) := ms_lambdaFISH_REV_primer)
        }
        # Update .available_os_barcodes
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% ms_lambdaFISH_REV_primer_nums)]
        
        # Add another column for the toe reverse primer
        df_multiplex_ms$ms_REV_primer_num_lambdaFISHtoe <- .available_os_barcodes[1]
        df_multiplex_ms$ms_REV_primer_lambdaFISHtoe <- .streets[df_multiplex_ms$ms_REV_primer_num_lambdaFISHtoe, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_ms$ms_REV_primer_num_lambdaFISHtoe)]
        
        # Create new columns for each lambda seq in df
        df_multiplex_ms <- df_multiplex_ms %>% 
          bind_cols(., as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex_ms$ms_lambdaFISH_key), ""))) %>%
                      mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_lambda_bits$Sequence[[1]],
                                                               . == 2 ~ input_lambda_bits$Sequence[[2]],
                                                               . == 3 ~ input_lambda_bits$Sequence[[3]],
                                                               . == 4 ~ input_lambda_bits$Sequence[[4]],
                                                               .default = "")))
          ) %>%
          #rename columns containing V to lambda_seq
          rename_with(~ str_replace(., "V", "ms_sec_lambdaFISH"), starts_with("V"))

        
      }
      
      
      if (bs_input == "ofq") {
        df_multiplex_bs <-
          left_join(df_multiplex_bs,
                    .available_multiplex_barcodes,
                    by = c("bs" = "id")) %>%
          rename(bs_ofq_key = barcode)
        df_multiplex_bs$bs_ofq_seq_primer_num <- .available_os_barcodes[1]
        df_multiplex_bs$bs_ofq_seq_primer <-.streets[df_multiplex_bs$bs_ofq_seq_primer_num, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_bs$bs_ofq_seq_primer_num)]
        
        # add OFQ barcode 
        df_multiplex_bs$bs_ofq_seq_primer_num <- .available_os_barcodes[1]
        df_multiplex_bs$bs_ofq_seq_primer <-.streets[df_multiplex_bs$bs_ofq_seq_primer_num, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_bs$bs_ofq_seq_primer_num)]
        
        # add OFQ barcode sequence
        df_multiplex_bs$bs_ofq_barcode <- as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex_bs$bs_ofq_key), ""))) %>% 
          mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_ofq_bits$Sequence[[1]],
                                                   . == 2 ~ input_ofq_bits$Sequence[[2]],
                                                   . == 3 ~ input_ofq_bits$Sequence[[3]],
                                                   . == 4 ~ input_ofq_bits$Sequence[[4]],
                                                   .default = ""))) %>%
          # bind all columns in one
          unite("ofq_barcode_seq", starts_with("V"), sep = "") %>%
          pull(ofq_barcode_seq)
        
      } else if (bs_input == "lambdaFISH") {
        df_multiplex_bs <-
          left_join(df_multiplex_bs,
                    .available_multiplex_barcodes,
                    by = c("bs" = "id")) %>%
          rename(bs_lambdaFISH_key = barcode)
        
        df_multiplex_bs$bs_num <- .available_os_barcodes[1:nrow(df_multiplex_bs)]
        df_multiplex_bs$bs_street <- .streets[df_multiplex_bs$bs_num,]$streets
        df_multiplex_bs$bs_toe <- .toes[df_multiplex_bs$bs_num,]$toes
        .available_os_barcodes <-
          .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_bs$bs_num)]
        
        # Add same FWD amplification primer to lambdaFISH
        df_multiplex_bs$bs_FWD_primer_num_lambdaFISH <- .available_os_barcodes[1]
        df_multiplex_bs$bs_FWD_primer_lambdaFISH <- .streets[df_multiplex_bs$bs_FWD_primer_num_lambdaFISH, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_bs$bs_FWD_primer_num_lambdaFISH)]
        
        # Determine the number of REV primers based on the first element of bs_lambdaFISH_key
        num_rev_primers <- nchar(df_multiplex_bs$bs_lambdaFISH_key[1])
        
        # Update .available_os_barcodes to get the REV primers
        bs_lambdaFISH_REV_primer_nums <- .available_os_barcodes[1:num_rev_primers]
        
        # Create new columns for each REV primer in df_multiplex_bs
        for (i in seq_len(num_rev_primers)) {
          # Get the REV primer number and primer
          bs_lambdaFISH_REV_primer_num <- bs_lambdaFISH_REV_primer_nums[i]
          bs_lambdaFISH_REV_primer <- .streets[bs_lambdaFISH_REV_primer_num, ]$streets
          
          # Add the new columns to df_multiplex_bs
          df_multiplex_bs <- df_multiplex_bs %>%
            mutate(!!paste0("bs_REV_primer_num_lambdaFISH", i) := bs_lambdaFISH_REV_primer_num,
                   !!paste0("bs_REV_primer_lambdaFISH", i) := bs_lambdaFISH_REV_primer)
        }
        # Update .available_os_barcodes
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% bs_lambdaFISH_REV_primer_nums)]
        
        # Add another column for the toe reverse primer
        df_multiplex_bs$bs_REV_primer_num_lambdaFISHtoe <- .available_os_barcodes[1]
        df_multiplex_bs$bs_REV_primer_lambdaFISHtoe <- .streets[df_multiplex_bs$bs_REV_primer_num_lambdaFISHtoe, ]$streets
        .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df_multiplex_bs$bs_REV_primer_num_lambdaFISHtoe)]
        
        # Create new columns for each lambda seq in df
        df_multiplex_bs <- df_multiplex_bs %>% 
          bind_cols(., as.data.frame(do.call(rbind, strsplit(as.character(df_multiplex_bs$bs_lambdaFISH_key), ""))) %>%
                      mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_lambda_bits$Sequence[[1]],
                                                               . == 2 ~ input_lambda_bits$Sequence[[2]],
                                                               . == 3 ~ input_lambda_bits$Sequence[[3]],
                                                               . == 4 ~ input_lambda_bits$Sequence[[4]],
                                                               .default = "")))
          ) %>%
          #rename columns containing V to lambda_seq
          rename_with(~ str_replace(., "V", "bs_sec_lambdaFISH"), starts_with("V"))
        
        
        
      }
      
      

      df_pairs <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      
      df <- df_pairs %>%
        left_join(df_multiplex_ms, by = "ms") %>%
        left_join(df_multiplex_bs, by = "bs")
      # df <- data %>%
      #   ungroup() %>%
      #   distinct(ms = .[[rlang::as_name(enquo(ms_id))]], bs = .[[rlang::as_name(enquo(bs_id))]])
      # df$ms_ofq_key <- 1:nrow(df)
      # df$ms_ofq_seq_primer <- .available_os_barcodes[1]
      # df$bs_ofq_key <- nrow(df):(nrow(df) * 2 - 1)
      # df$bs_ofq_seq_primer <- .available_os_barcodes[2]
      # .available_os_barcodes <- .available_os_barcodes[-c(1, 2)]
    }
    
    if (str_detect(bs_id, "uni")) {
      names(df) <- gsub("ms", "uni_ms", names(df), fixed = TRUE)
      names(df) <- gsub("bs", "uni_bs", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "uni_ms",.x, fixed = TRUE)) %>%
      #   rename_with(~gsub("bs", "uni_bs",.x, fixed = TRUE))
    } else if (str_detect(bs_id, "1")) {
      names(df) <- gsub("ms", "ms1", names(df), fixed = TRUE)
      names(df) <- gsub("bs", "bs1", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "ms1",.x, fixed = TRUE)) %>%
      #   rename_with(~gsub("bs", "bs1",.x, fixed = TRUE))
    } else if (str_detect(bs_id, "2")) {
      names(df) <- gsub("ms", "ms2", names(df), fixed = TRUE)
      names(df) <- gsub("bs", "bs2", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "ms2",.x, fixed = TRUE)) %>%
      #   rename_with(~gsub("bs", "bs2",.x, fixed = TRUE))
    }
    
    ###############################
    # ONLY MS needs to be appended
    ###############################
  } else if (!is.null(ms_input) && is.null(bs_input)) {
    if (ms_input == "toe_seq_im" | ms_input == "seq_im") {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]]) #%>% #.[[rlang::as_name(enquo(ms_id))]]
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df$ms_num <- .available_os_barcodes[1:nrow(df)]
      df$ms_street <- .streets[df$ms_num,]$streets
      df$ms_toe <- .toes[df$ms_num, ]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df$ms_num)]
    } else if (ms_input == "ofq") {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      # df$bs_ofq_key <- .available_multiplex_barcodes$barcode[1:nrow(df)]
      df <-
        left_join(df, .available_multiplex_barcodes, by = c("ms" = "id")) %>%
        rename(ms_ofq_key = barcode)
      df$ms_ofq_seq_primer_num <- .available_os_barcodes[1]
      df$ms_ofq_seq_primer <-
        .streets[df$ms_ofq_seq_primer_num, ]$streets
      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df$ms_ofq_seq_primer_num)]
      
      # add OFQ barcode sequence
      df$ms_ofq_barcode <- as.data.frame(do.call(rbind, strsplit(as.character(df$ms_ofq_key), ""))) %>% 
        mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_ofq_bits$Sequence[[1]],
                                                 . == 2 ~ input_ofq_bits$Sequence[[2]],
                                                 . == 3 ~ input_ofq_bits$Sequence[[3]],
                                                 . == 4 ~ input_ofq_bits$Sequence[[4]],
                                                 .default = ""))) %>%
        # bind all columns in one
        unite("ofq_barcode_seq", starts_with("V"), sep = "") %>%
        pull(ofq_barcode_seq)

      #   bind_cols(as.data.frame(do.call(rbind, strsplit(as.character(.$barcode), ""))) %>%
      #               # change numbers to sequences
      #               mutate(across(starts_with("V"), ~ case_when(. == 1 ~ sequences[[1]],
      #                                                        . == 2 ~ sequences[[2]],
      #                                                        . == 3 ~ sequences[[3]],
      #                                                        . == 4 ~ sequences[[4]])))) %>%
      #   # rename to lamda
      #   rename_with(~ gsub("V", "lambda_", .x))
      
    } else if (ms_input == "lambdaFISH") {
      df <- data %>%
        ungroup() %>%
        distinct(ms = .[[rlang::as_name(enquo(ms_id))]])
      df <-
        left_join(df, .available_multiplex_barcodes, by = c("ms" = "id")) %>%
        rename(ms_lambdaFISH_key = barcode)
      
      # Add the target street sequence
      df$ms_num <- .available_os_barcodes[1:nrow(df)]
      df$ms_street <- .streets[df$ms_num,]$streets
      # Add the target toe sequence
      df$ms_toe <- .toes[df$ms_num,]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df$ms_num)]
      # Add another column for the toe sequence
      
      
      # Add same FWD amplification primer to lambdaFISH
      df$ms_FWD_primer_num_lambdaFISH <- .available_os_barcodes[1]
      df$ms_FWD_primer_lambdaFISH <- .streets[df$ms_FWD_primer_num_lambdaFISH, ]$streets
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df$ms_FWD_primer_num_lambdaFISH)]
      
      # Determine the number of REV primers based on the first element of ms_lambdaFISH_key
      num_rev_primers <- nchar(df$ms_lambdaFISH_key[1])
      
      # Update .available_os_barcodes to get the REV primers
      ms_lambdaFISH_REV_primer_nums <- .available_os_barcodes[1:num_rev_primers]
      
      # Create new columns for each REV primer in df
      for (i in seq_len(num_rev_primers)) {
        # Get the REV primer number and primer
        ms_lambdaFISH_REV_primer_num <- ms_lambdaFISH_REV_primer_nums[i]
        ms_lambdaFISH_REV_primer <- .streets[ms_lambdaFISH_REV_primer_num, ]$streets
        
        # Add the new columns to df
        df <- df %>%
          mutate(!!paste0("ms_REV_primer_num_lambdaFISH", i) := ms_lambdaFISH_REV_primer_num,
                 !!paste0("ms_REV_primer_lambdaFISH", i) := ms_lambdaFISH_REV_primer)
      }
      # Update .available_os_barcodes
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% ms_lambdaFISH_REV_primer_nums)]
      
      # Add another column for the toe reverse primer
      df$ms_REV_primer_num_lambdaFISHtoe <- .available_os_barcodes[1]
      df$ms_REV_primer_lambdaFISHtoe <- .streets[df$ms_REV_primer_num_lambdaFISHtoe, ]$streets
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df$ms_REV_primer_num_lambdaFISHtoe)]
      

      # Create new columns for each lambda seq in df
      df <- df %>% 
        bind_cols(., as.data.frame(do.call(rbind, strsplit(as.character(df$ms_lambdaFISH_key), ""))) %>%
                    mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_lambda_bits$Sequence[[1]],
                                                             . == 2 ~ input_lambda_bits$Sequence[[2]],
                                                             . == 3 ~ input_lambda_bits$Sequence[[3]],
                                                             . == 4 ~ input_lambda_bits$Sequence[[4]],
                                                             .default = "")))
        ) %>%
        #rename columns containing V to lambda_seq
        rename_with(~ str_replace(., "V", "ms_sec_lambdaFISH"), starts_with("V"))
      

    }
    
    if (str_detect(ms_id, "uni")) {
      #names(df) <- paste0("uni_", names(df))
      names(df) <- gsub("ms", "uni_ms", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "uni_ms",.x, fixed = TRUE))
    } else if (str_detect(ms_id, "1")) {
      #names(df) <- paste0(names(df), "1")
      names(df) <- gsub("ms", "ms1", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "ms1",.x, fixed = TRUE))
    } else if (str_detect(ms_id, "2")) {
      #names(df) <- paste0(names(df), "2")
      names(df) <- gsub("ms", "ms2", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("ms", "ms2",.x, fixed = TRUE))
    }
    
    ###############################
    # ONLY BS needs to be appended
    ###############################
  } else if (is.null(ms_input) && !is.null(bs_input)) {
    if (bs_input == "toe_seq_im" | bs_input == "seq_im") {
      df <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      #rename_with(~gsub({{ ms_id }}, "ms", .x, fixed = TRUE))
      df$bs_num <- .available_os_barcodes[1:nrow(df)]
      df$bs_street <- ifelse(bs_rc, rc(.streets[df$bs_num,]$streets), .streets[df$bs_num,]$streets)
      df$bs_toe <- ifelse(bs_rc, rc(.toes[df$bs_num, ]$toes),.toes[df$bs_num, ]$toes)
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in%df$bs_num)]
      
    } else if (bs_input == "ofq") {
      df <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      # df$bs_ofq_key <- .available_multiplex_barcodes$barcode[1:nrow(df)]
      df <-
        left_join(df, .available_multiplex_barcodes, by = c("bs" = "id")) %>%
        rename(bs_ofq_key = barcode)
      df$bs_ofq_seq_primer_num <- .available_os_barcodes[1]
      df$bs_ofq_seq_primer <-
        .streets[df$bs_ofq_seq_primer_num, ]$streets
      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df$bs_ofq_seq_primer_num)]
      
      
      # add OFQ barcode sequence
      df$bs_ofq_barcode <- as.data.frame(do.call(rbind, strsplit(as.character(df$bs_ofq_key), ""))) %>% 
        mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_ofq_bits$Sequence[[1]],
                                                 . == 2 ~ input_ofq_bits$Sequence[[2]],
                                                 . == 3 ~ input_ofq_bits$Sequence[[3]],
                                                 . == 4 ~ input_ofq_bits$Sequence[[4]],
                                                 .default = ""))) %>%
        # bind all columns in one
        unite("ofq_barcode_seq", starts_with("V"), sep = "") %>%
        pull(ofq_barcode_seq)
      
    } else if (bs_input == "lambdaFISH") {
      df <- data %>%
        ungroup() %>%
        distinct(bs = .[[rlang::as_name(enquo(bs_id))]])
      df <-
        left_join(df, .available_multiplex_barcodes, by = c("bs" = "id")) %>%
        rename(bs_lambdaFISH_key = barcode)
      
      # Add the target street sequence
      df$bs_num <- .available_os_barcodes[1:nrow(df)]
      df$bs_street <- .streets[df$bs_num,]$streets
      # Add the target toe sequence
      df$bs_toe <- .toes[df$bs_num,]$toes
      .available_os_barcodes <-
        .available_os_barcodes[-which(.available_os_barcodes %in% df$bs_num)]
      # Add another column for the toe sequence
      
      
      # Add same FWD amplification primer to lambdaFISH
      df$bs_FWD_primer_num_lambdaFISH <- .available_os_barcodes[1]
      df$bs_FWD_primer_lambdaFISH <- .streets[df$bs_FWD_primer_num_lambdaFISH, ]$streets
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df$bs_FWD_primer_num_lambdaFISH)]
      
      # Determine the number of REV primers based on the first element of bs_lambdaFISH_key
      num_rev_primers <- nchar(df$bs_lambdaFISH_key[1])
      
      # Update .available_os_barcodes to get the REV primers
      bs_lambdaFISH_REV_primer_nums <- .available_os_barcodes[1:num_rev_primers]
      
      # Create new columns for each REV primer in df
      for (i in seq_len(num_rev_primers)) {
        # Get the REV primer number and primer
        bs_lambdaFISH_REV_primer_num <- bs_lambdaFISH_REV_primer_nums[i]
        bs_lambdaFISH_REV_primer <- .streets[bs_lambdaFISH_REV_primer_num, ]$streets
        
        # Add the new columns to df
        df <- df %>%
          mutate(!!paste0("bs_REV_primer_num_lambdaFISH", i) := bs_lambdaFISH_REV_primer_num,
                 !!paste0("bs_REV_primer_lambdaFISH", i) := bs_lambdaFISH_REV_primer)
      }
      # Update .available_os_barcodes
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% bs_lambdaFISH_REV_primer_nums)]
      
      # Add another column for the toe reverse primer
      df$bs_REV_primer_num_lambdaFISHtoe <- .available_os_barcodes[1]
      df$bs_REV_primer_lambdaFISHtoe <- .streets[df$bs_REV_primer_num_lambdaFISHtoe, ]$streets
      .available_os_barcodes <- .available_os_barcodes[-which(.available_os_barcodes %in% df$bs_REV_primer_num_lambdaFISHtoe)]
      
      
      # Create new columns for each lambda seq in df
      df <- df %>% 
        bind_cols(., as.data.frame(do.call(rbind, strsplit(as.character(df$bs_lambdaFISH_key), ""))) %>%
                    mutate(across(starts_with("V"), ~ case_when(. == 1 ~ input_lambda_bits$Sequence[[1]],
                                                             . == 2 ~ input_lambda_bits$Sequence[[2]],
                                                             . == 3 ~ input_lambda_bits$Sequence[[3]],
                                                             . == 4 ~ input_lambda_bits$Sequence[[4]],
                                                             .default = "")))
        ) %>%
        #rename columns containing V to lambda_seq
        rename_with(~ str_replace(., "V", "bs_sec_lambdaFISH"), starts_with("V"))
      
      
    }
    
    if (str_detect(bs_id, "uni")) {
      names(df) <- gsub("bs", "uni_bs", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("bs", "uni_bs",.x, fixed = TRUE))
    } else if (str_detect(bs_id, "1")) {
      names(df) <- gsub("bs", "bs1", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("bs", "bs1",.x, fixed = TRUE))
    } else if (str_detect(bs_id, "2")) {
      names(df) <- gsub("bs", "bs2", names(df), fixed = TRUE)
      # df <- df %>%
      #   rename_with(~gsub("bs", "bs2",.x, fixed = TRUE))
    }
    
  }
  
  
  created_pairs_list <-
    list("available_barcodes" = .available_os_barcodes, "df" = df)
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
  # barcodes_split <- lll[score$freq_pattern < different_last,]
  barcodes_split <- mmm[score$freq_pattern < (different_last-1),]
  #names(barcodes)
  barcodes <- unite(barcodes_split, sequence, 1:cycles, sep="")
  barcodes$code <- row.names(barcodes)
  barcodes
}

# Ensure that the stringdist package is installed
if (!requireNamespace("stringdist", quietly = TRUE)) {
  install.packages("stringdist")
}


library(stringdist)

generate_multiplex_barcodes <- function(cycles = 3, sequences = c("A", "G", "C", "T"), min_hamming_distance = floor(cycles / 2)) {
  
  # Map sequences to a number
  sequence_map <- setNames(seq_along(sequences), sequences)
  
  # Generate all permutations using numbers
  all_permutations <- expand.grid(rep(list(sequence_map), cycles))
  all_permutations <- apply(all_permutations, 1, function(x) paste0(x, collapse = ""))
  
  # Adjust the repeat limit for small cycles
  max_repeat_limit <- ifelse(cycles <= 4, 1, ceiling(0.2 * cycles))
  
  # Function to check for too many repeats
  has_too_many_repeats <- function(seq) {
    seq_numbers <- as.integer(strsplit(seq, "")[[1]])
    any(rle(seq_numbers)$lengths > max_repeat_limit)
  }
  
  # Filter out sequences with too many repeats
  filtered_permutations <- Filter(function(seq) !has_too_many_repeats(seq), all_permutations)
  
  # Function to calculate Hamming distance
  is_valid_hamming_distance <- function(seq, sequences, min_distance) {
    # Count how many sequences meet the Hamming distance criterion
    valid_count <- sum(sapply(sequences, function(other_seq) {
      stringdist::stringdist(seq, other_seq, method = "hamming") >= min_distance
    }))
    # Return TRUE if a sufficient number of sequences meet the criterion
    return(valid_count >= length(sequences) * 0.5)  # Example: at least 50% of sequences meet the criterion
  }
  
  # Filter permutations based on Hamming distance
  valid_sequences <- Filter(function(seq) is_valid_hamming_distance(seq, filtered_permutations, min_hamming_distance), filtered_permutations)
  
  return(valid_sequences)
}


#########################################################
# precompute the multiplex barcodes for a number of cycles and save them to accelerate the appending process for OFQ and lambdaFISH
#########################################################
# for (cycles in 7:10) {
#   mpx_data <- generate_multiplex_barcodes(cycles = cycles, sequences = c("GGTCT","TGGTC","AGTCA","CGCTC"), min_hamming_distance = ceiling(cycles/2))
#   df <- tibble(mpx = mpx_data)
#   # file path
#   file_path <- paste0("/Users/alioutas/Google Drive/My Drive/GitHub/OASIS/OASIS_app/mpx/mpx_", cycles, ".csv")
#   # Write csv
#   write.csv(df, file_path, row.names = FALSE)
# }





# START TO DELETE
# Example usage
# sequences <- c("AGTC", "GAGT", "CTGC", "TAHAH")
# result <- generate_multiplex_barcodes(cycles = 6, sequences, min_hamming_distance = 3)
# sort(result)
# length(result)

# END TO DELETE




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
      scrollX = TRUE)
    )
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



intersect_coordinates <- function(y, x, correct_coor = T, .options = '-wa -wb'){
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
    RBedtools('intersect',options = .options, a=., b=RBedtools(tool = 'sort', i=from_data_frame(y))) %>% to_data_frame #from_data_frame(y)
  
  #RBedtools(tool = 'intersect',options = '-wa -wb', a=from_data_frame(x), b=from_data_frame(y)) %>% to_data_frame
  
} 


balance_density <- function(data, id_column, n_probes) {
  all_balanced_filtered <- data %>% 
    drop_na() %>% 
    arrange(chr, start) %>% 
    group_by(chr) %>% 
    group_modify(~ { 
      .x %>% 
        mutate(distance = start - lag(end)) 
    }) %>% 
    filter(between(distance, 0, 1000000)) %>% 
    ungroup() %>% 
    group_by(chr, !!sym(id_column)) %>% 
    add_count(name = "total") %>%
    group_split() %>% 
    map(function(x) {
      if (nrow(x) <= n_probes) {
        res <- rbind(x, sample_n(size = n_probes - nrow(x), replace = T, x))
      } else { 
        res <- sample_n(size = n_probes, weight = distance, replace = F, x)
      }
      return(res)
    }) %>%
    bind_rows() %>% 
    arrange(chr, start) %>% 
    group_by(chr) %>% 
    group_modify(~ { 
      .x %>% 
        mutate(distance = start - lag(end)) 
    }) %>% 
    ungroup()
  
  return(all_balanced_filtered)
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





