### PDF WRANGLING ###

# Uses Gradescope student evaluation PDFs (from Gradescope --> Review Grades --> Export Evaluations)
# PDFs should be copied into a folder named "pdf_originals" in the same working directory or project as this script
# This code saves the first TWO pages of each eval as a new PDF according to student name

# Load packages
library(pdftools)
library(tidyverse)

# For each original PDF, save the first TWO pages and rename using the student's name

for (pdf_i in list.files('pdf_wrangling/pdfs_original')) {
  
  # Only save first TWO pages of each PDF
  pdf_subset(paste0('pdf_wrangling/pdfs_original/', pdf_i), pages = c(1:2), output = 'split.pdf')
  
  # Extract student name from PDF and arrange as last, first
  name <- str_split(read_lines(pdf_text('split.pdf'))[2],' ', simplify = TRUE)
  label <- paste0(name[2],'_',name[1])
  
  # Rename files by name
  file.rename('split.pdf', paste0('pdf_wrangling/pdfs_new/',label,'.pdf'))
}


