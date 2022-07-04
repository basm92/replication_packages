here <- here::here()

number <- utils::select.list(title = "Which chapter do you want to replicate?",
                   choices=c("Chapter 2", "Chapter 3", "Chapter 4"))

if(number == "Chapter 2") {
  
  setwd(paste0(here, '/chapter2'))
  paste("Working Directory:", getwd())
  
} else if(number == "Chapter 3"){
  
  setwd(paste0(here, '/chapter3'))
  paste("Working Directory:", getwd())
} else if(number == "Chapter 4"){
  
  setwd(paste0(here, '/chapter4'))
  paste("Working Directory:", getwd())
  
} else {
  
  "Invalid Number"
  
}

