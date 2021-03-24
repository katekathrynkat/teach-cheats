### TAKING ATTENDANCE FROM ZOOM REPORTS ###

# Requires Zoom participant lists
# Download from online Zoom account: Reports --> Usage reports --> Participants --> Export (check "Export with meeting data" and "Show unique users")
# Move to "zoom_attendance/participant_lists" folder

# Requires class roster downloaded from eGrades
# Move to "zoom_attendance" folder

# Load packages
library(janitor)
library(lubridate)
library(tidyverse)

# Load class roster
roster <- read_csv('zoom_attendance/roster.csv') %>% 
  clean_names() %>% 
  mutate(length = nchar(student_last),
         temp = 1) %>% 
  group_by(student_last) %>% 
  mutate(duplicates = sum(temp)) %>% 
  ungroup() %>% 
  mutate(check = case_when(
    length < 4 | duplicates > 1 ~ 'YES',
    TRUE ~ 'NO'
  )) %>% 
  select(perm_number, student_last, student_first_middle, email, check)

# Create empty data frame for taking roll
roll <- tibble('name'=numeric(),
               'date'=Date(),
               'roll'=character())

for (list_i in list.files('zoom_attendance/participant_lists')) {

  # Load meeting info from header
  meeting <- read_csv(paste0('zoom_attendance/participant_lists/', list_i), n_max=1) %>% 
    clean_names() %>% 
    mutate(date = date(dmy_hms(start_time)))
  date <- as.character(meeting[[1,9]])

  # Load participant info
  participants <- read_csv(paste0('zoom_attendance/participant_lists/', list_i), skip=2) %>% 
    clean_names()
  
  # Check attendance
  for (student_i in roster$perm_number) {
    
    # Strings for student's last name and email
    first <- filter(roster, perm_number==student_i)[[3]]
    last <- filter(roster, perm_number==student_i)[[2]]
    email <- filter(roster, perm_number==student_i)[[4]]
    
    # Filter for Zoom participants that match last name and email
    temp <- participants
    temp$roll <- if_else(grepl(email, participants$user_email, ignore.case=TRUE) |
                           (grepl(last, participants$name_original_name, ignore.case=TRUE) &
                              grepl(first, participants$name_original_name, ignore.case=TRUE)),
                         'PRESENT', NULL)
    temp2 <- temp %>% 
      filter(roll=='PRESENT')
    
    # Extract Zoom name
    zoomname <- paste(temp2$name_original_name, collapse = ', ')
    
    # Add student's attendance to roll
    new_roll <- tibble(perm_number = student_i, date = date, roll = zoomname)
    roll <- rbind(roll, new_roll) %>% 
      filter(roll != '')
    
  }
}

# Reformat data frame
attendance <- roll %>% 
  pivot_wider(names_from = date, values_from = roll) %>% 
  full_join(roster) %>% 
  mutate_all(~replace_na(., 'ABSENT'))

write_csv(attendance, 'attendance.csv')
