
start_t <- Sys.time()

for(i in 13:15){
  
  rounds <- i
  n_barcodes <- 4^rounds
  n_repeats <- if(rounds > 3) round(rounds*0.4) else 0
  
  pool <- 1:4
  
  
  barcodes <- t(replicate(n_barcodes, sample(pool, rounds, replace = T)))
  barcodes <- apply( barcodes[ , 1:ncol(barcodes) ] , 1 , paste0 , collapse = "" )
  barcodes <- unique(barcodes)
  
  repeats <- c(strrep("1", n_repeats), 
               strrep("2", n_repeats),
               strrep("3", n_repeats),
               strrep("4", n_repeats))
  
  barcodes_filtered <- if(n_repeats !=0) barcodes[str_detect(barcodes, paste0(repeats, collapse = "|"), negate = T)] else barcodes
  
  write_csv(tibble(bc = barcodes_filtered), paste0("~/Google Drive/My Drive/HMS/general_lab/Vutara_related/scripts/appending/appending/appending/LF/LF", i, "_rounds"))
  print(paste0("Saved ", i, " lambda round file"))
}




Sys.time() - start_t
length(barcodes)
length(barcodes_filtered)
