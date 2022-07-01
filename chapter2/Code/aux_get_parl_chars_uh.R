get_parl_chars_uh <- function(dateseq = 20){
    
    #Argument tells you which "day" to pick after election, default = 50
    #Load the election dates
    
    parls <- uh_parliaments %>%
        select(parliament) %>%
        unique() %>%
        pull()
        
    
    #parls <- paste0(parls, "-01-01")
    # Make an empty list to store the results
    results <- vector("list", length(parls))
    
    for(i in 1:length(parls)){
        
        # Create a date sequence
        datebegin <- str_split(parls[i], "-") %>%
            magrittr::extract2(1) %>%
            magrittr::extract(1) %>%
            paste0("-01-01")
        
        datebegin <- ymd(datebegin)
        
        dateend <- str_split(parls[i], "-") %>%
            magrittr::extract2(1) %>%
            magrittr::extract(2) %>%
            paste0("-01-01")
        
        dateend <- ifelse(i == length(parls), 
                          as.character("1922-01-01"),
                          dateend)
        
        dateend <- dateend %>%
            ymd()
        
        sequence <- seq(datebegin, dateend, by = dateseq)
        
        #For each time in this sequence, get the politician count
        for(j in 1:length(sequence)){
            #Data input: uh_parliaments
            results[[i]][[j]] <- uh_parliaments %>%
                filter(parliament == parls[i]) %>%
                filter(begin_periode < sequence[j], einde_periode > sequence[j]) %>%
                group_by(class) %>%
                summarize(count = n(), date = sequence[j])
            
        }
        
    }
    
    results %>%
        lapply(reduce, rbind) %>%
        reduce(rbind)
    
}
