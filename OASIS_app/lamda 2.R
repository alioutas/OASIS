# load libraries
require(tidyverse)
require(RcppAlgos)

#################### loading OS barcodes
# Import matched_streets 3K last OS barcodes
matched_streets <- read_csv("/Volumes/GoogleDrive/My Drive/HMS/general_lab/oligopaint_mining_appending/matches_toes_hg38mm10dm6wuhCor1loxafr3ce11_100120.csv")

# use 21-2021
toes <- read_csv("/Volumes/GoogleDrive/My Drive/HMS/OligoLego/Streets/3K/Toes_hg38mm10dm6wuhCor1loxafr3ce11_100120.txt", col_names = F) %>%
  `colnames<-`(c("toes"))
streets <- read_csv("/Volumes/GoogleDrive/My Drive/HMS/OligoLego/Streets/3K/Streets_hg38mm10dm6wuhCor1loxafr3ce11_100120.txt", col_names = F) %>% mutate(street_number = as.numeric(row.names(.))) %>%
  `colnames<-`(c("street", "street_number"))


# load 2K GWI
lib <- read_tsv("/Volumes/GoogleDrive/My Drive/HMS/collaborations/Huy_Shyamtanu/genome_wide_2k_OF/final_2K_all_barcodes.txt")


# create barcodes sequences
sequences <- c(sec1 = "CACCGACGTCGCATAGAACGGAAGAGCGTGTG",
               sec3 = "CGAGCCAGGTCATCCTAGCCCATACGGCAATG",
               sec5 = "AGCGCAGGAGGTCCACGACGTGCAAGGGTGT",
               sec6 = "CACACGCTCTCCGTCTTGGCCGTGGTCGATCA")
rounds <- 6
set.seed(1)
OF_key <- (tibble(as_tibble(permuteGeneral(seq_along(sequences), rounds,freqs = rep(round(.2*rounds), 4)))) %>% unite("all", 1:rounds, remove = T,sep = ""))$all
length(OF_key)
# filter them by hamming distance
hamming.distance <- function(x, y, pairwise = TRUE) {
  nx <- length(x)
  ny <- length(y)
  rawx <- intToBits(x)
  rawy <- intToBits(y)
  if (nx == 1 && ny == 1) return(sum(as.logical(xor(intToBits(x),intToBits(y)))))
  if (nx < ny) {
    ## pivoting
    tmp <- rawx; rawx <- rawy; rawy <- tmp
    tmp <- nx; nx <- ny; ny <- tmp
  }
  if (nx %% ny) stop("unconformable length!") else {
    bits <- length(intToBits(0)) ## 32-bit or 64 bit?
    result <- unname(tapply(as.logical(xor(rawx,rawy)), rep(1:ny, each = bits), sum))
  }
  if (pairwise) result else sum(result)
}


hamming_table <- outer(OF_key, OF_key, hamming.distance)

D <- dist(cbind(runif(4), runif(4)), diag=TRUE, upper=TRUE) # generate dummy data
m <- as.matrix(hamming_table) # coerce dist object to a matrix
dimnames(m) <- dimnames(m) <- list(1:nrow(hamming_table), 1:nrow(hamming_table)) 
xy <- t(combn(colnames(m), 2))
OF_key_filtered <- OF_key[as.numeric((data.frame(xy, dist=m[xy]) %>% 
                                        filter(dist > round(rounds*2)) %>% 
                                        select(1:2) %>% 
                                        distinct(X1))$X1)]





# LAMDA FISH creation of sequential imaging barcodes
# create imaging library ampF + bridge_toe + sec + ampR
lamda <- lib %>%
  # select and split number column
  bind_cols(as.data.frame(do.call(rbind, strsplit(as.character(.$bs1_number), ""))) %>%
              # change numbers to sequences
              mutate(across(contains("V"), ~ case_when(. == 1 ~ sequences[[1]],
                                                       . == 2 ~ sequences[[2]],
                                                       . == 3 ~ sequences[[3]],
                                                       . == 4 ~ sequences[[4]])))) %>%
  # rename to lamda
  rename_with(~ gsub("V", "lamda_", .x)) %>% 
  # create imaging library ampF + bridge_toe + sec + ampR
  mutate(across(contains("lamda"), ~ str_c(streets$street[max(lib$bs2_number)+1],
                                           "tt",
                                           rc(toes$toes[lib$bs2_number]), 
                                           "tt",
                                           .x,
                                           "tt",
                                           # select for i-th complementary available barcode of the forward primer
                                           streets$street[(matched_streets %>% filter(main == max(lib$bs2_number)+1, key > max(lib$bs2_number)+1) %>% arrange(key))$key[as.numeric(str_sub(cur_column(), start = -1L))]]))) %>% 
  select(bs1_number, contains("lamda"))
write_tsv(lamda, "/Volumes/GoogleDrive/My Drive/HMS/collaborations/Huy_Shyamtanu/genome_wide_2k_OF/lamda/GenomeWide_2K_lamda_key.csv", col_names = T)


# LAMDA FISH amplification primers
#lamda amp primers table
T7 <- "TAATACGACTCACTATAGGG" # add 5' rc(BS)
lamda_primers <- lamda %>% 
  select(contains("lamda")) %>% 
  pivot_longer(cols = everything(), names_to = "sequence") %>% 
  mutate(forward = str_sub(.$value, start = 1, end = 20),
         reverse = rc(str_sub(.$value, start = -20)),
         T7_reverse = str_c(T7, reverse)) %>% 
  distinct(sequence, forward, reverse, T7_reverse)


# create toe library ampF + toe + ampR
# find which is the max primer number used for lamda bridges
max_p <- max(streets[streets$street %in% unique(c(lamda_primers$forward, rc(lamda_primers$reverse))),]$street_number)+1
lamda_toe <- lib %>%
  # create imaging library ampF + bridge_toe + sec + ampR
  mutate(lamda_toe = str_c(streets$street[max(lib$bs2_number)+1],
                           "tt",
                           toes$toes[lib$bs2_number],
                           "tt",
                           streets$street[(matched_streets %>% filter(main == max_p, key > max_p) %>% arrange(key))$key[1]])) %>% 
  select(lamda_toe)
write_tsv(lamda_toe, "/Volumes/GoogleDrive/My Drive/HMS/collaborations/Huy_Shyamtanu/genome_wide_2k_OF/lamda/GenomeWide_2K_lamda_toe_order.txt", col_names = F)
lamda_toe_primers <- lamda_toe %>% 
  mutate(forward = str_sub(.$lamda_toe, start = 1, end = 20),
         reverse = rc(str_sub(.$lamda_toe, start = -20)),
         T7_reverse = str_c(T7, reverse)) %>% 
        distinct(forward, reverse, T7_reverse) %>% 
        bind_cols(sequence = "lamda_toe") %>% 
        select(sequence, everything())

write_tsv(bind_rows(lamda_primers, lamda_toe_primers), "/Volumes/GoogleDrive/My Drive/HMS/collaborations/Huy_Shyamtanu/genome_wide_2k_OF/lamda/GenomeWide_2K_lamda_primers.csv", col_names = T)


lamda_order <- lamda %>% 
  select(contains("lamda")) %>% 
  pivot_longer(cols = everything(), names_to = "sequence") %>% 
  arrange(sequence) %>% 
  select(value)
write_tsv(lamda_order, "/Volumes/GoogleDrive/My Drive/HMS/collaborations/Huy_Shyamtanu/genome_wide_2k_OF/lamda/GenomeWide_2K_lamda_order.txt", col_names = F)



