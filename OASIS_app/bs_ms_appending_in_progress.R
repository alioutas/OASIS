outcome_tibble <- tibble(
  ms = c("blue", "orange", "orange", "orange", "yellow", "black"),
  bs = c("elephant", "tiger", "leon", "leopard", "hawk", "hawk"),
  ms_num = c(1,3,3,3,4,6),
  bs_num = c(2,25, 26, 27, 5,5)
)
library(tidyverse)

compatible_numbers <- tibble(
  key = c(rep(1, 8), rep(2, 8), rep(3, 8), rep(4, 8), rep(5, 8), rep(6, 8), rep(7, 8)),
  main = c(seq(2, 9), seq(13, 20), seq(25,32), seq(3, 18, by =2), c(4, 6:12), seq(7, 14), seq(5, 26, by = 3))
)

available_numbers <- seq_len(max(compatible_numbers))[seq_len(max(compatible_numbers)) %in% c(compatible_numbers$key, compatible_numbers$main)]



id_tibble <- tibble(
  ms = c("blue", "orange", "orange", "orange", "yellow", "black"),
  bs = c("elephant", "tiger", "leon", "leopard", "hawk", "hawk")
)


id_tibble$ms_num <- NA
id_tibble$bs_num <- NA


for(i in 1:nrow(id_tibble)){
  if (i == 1){
    #assign the first available number
    id_tibble$ms_num[i] <- available_numbers[1]
    all_num <- compatible_numbers$main[compatible_numbers$key == available_numbers[1]]
    #Keep only the ones which are available
    all_num <- intersect(all_num, available_numbers)
    #Remove the color_num value
    available_numbers <- available_numbers[-1]
    #assign the first available compatible number
    id_tibble$bs_num[i] <- all_num[1]
    #Remove the animal_num value
    available_numbers <- available_numbers[-1]
  } else{
    if(id_tibble$ms[i] != id_tibble$ms[i-1] && id_tibble$bs[i] != id_tibble$bs[i-1]){
      #assign the first available number
      id_tibble$ms_num[i] <- available_numbers[1]
      all_num <- compatible_numbers$main[compatible_numbers$key == available_numbers[1]]
      #Keep only the ones which are available
      all_num <- intersect(all_num, available_numbers)
      #Remove the color_num value
      available_numbers <- available_numbers[-1]
      #assign the first available compatible number
      id_tibble$bs_num[i] <- all_num[1]
      #Remove the animal_num value
      available_numbers <- available_numbers[-which(available_numbers == all_num[1])]
    } else if(id_tibble$ms[i] == id_tibble$ms[i-1] && id_tibble$bs[i] != id_tibble$bs[i-1]){
      #assign the previous number
      id_tibble$ms_num[i] <- id_tibble$ms_num[i-1]
      all_num <- compatible_numbers$main[compatible_numbers$key == id_tibble$ms_num[i-1]]
      #Keep only the ones which are available
      all_num <- intersect(all_num, available_numbers)
      #assign the first available compatible number
      id_tibble$bs_num[i] <- all_num[1]
      #Remove the animal_num value
      available_numbers <- available_numbers[-which(available_numbers == all_num[1])]
    } else if(id_tibble$ms[i] != id_tibble$ms[i-1] && id_tibble$bs[i] == id_tibble$bs[i-1]){
      #assign the previous number
      id_tibble$bs_num[i] <- id_tibble$bs_num[i-1]
      all_num <- compatible_numbers$main[compatible_numbers$key == id_tibble$bs_num[i-1]]
      #Keep only the ones which are available
      all_num <- intersect(all_num, available_numbers)
      #assign the first available compatible number
      id_tibble$ms_num[i] <- all_num[1]
      #Remove the animal_num value
      available_numbers <- available_numbers[-which(available_numbers == all_num[1])]
    }
  }
}
