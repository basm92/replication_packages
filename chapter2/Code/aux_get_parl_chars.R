get_parl_chars <- function(dateseq = 20){
  
  #Argument tells you which "day" to pick after election, default = 50
  #Load the election dates
  
  parls <- read_csv("./Data/electiondates.csv")
  # Make an empty list to store the results
  results <- vector("list", length(parls$parl))
  
  for(i in 1:length(parls$parl)){
    
    # Create a date sequence
    datebegin <- parls$date[i]
    dateend <- ifelse(i == length(parls$parl), 
                      as.character("1922-07-05"),
                      as.character(parls$date[i+1]))
    
    dateend <- dateend %>%
      ymd()
    
    sequence <- seq(datebegin, dateend, by = dateseq)
    
    #For each time in this sequence, get the politician count
    for(j in 1:length(sequence)){
      #Data input: lh_parliaments
      results[[i]][[j]] <- lh_parliaments %>%
        filter(parliament == parls$parl[i]) %>%
        filter(begin_periode < sequence[j], einde_periode > sequence[j]) %>%
        group_by(class) %>%
        summarize(count = n(), date = sequence[j])
      
    }
    
  }
  
  results %>%
    lapply(reduce, rbind) %>%
    reduce(rbind)
  
}
