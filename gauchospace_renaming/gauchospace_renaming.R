library(tidyverse)

for (i in list.files('gauchospace_renaming/exported')) {
  
  file <- list.files(paste0('gauchospace_renaming/exported/',i))
  
  ext <- str_split(file, '[.]', simplify=TRUE)[2]
  
  name <- str_split(i, '_', simplify=TRUE)[1] # change to "2" to return perm instead of name
  
  file.rename(from = paste0('gauchospace_renaming/exported/',i,'/',file),
              to = paste0('gauchospace_renaming/renamed/',name,'.',ext))
}
