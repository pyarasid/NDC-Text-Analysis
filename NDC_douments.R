library(pdftools)
library(dplyr)
library(tm)
library(readr)
library(stringr)

#reading pdf and converting the text one line per row
txt_india <- pdf_text("INDIA INDC TO UNFCCC.pdf") %>% 
  read_lines()

str(txt_india)

#converting the character vector into a tibble
txt_indiadf <- tibble(line=1:1399, text=txt_india)

#adding NAs to the blank rows
txt_indiadf[txt_indiadf==""] <- NA

#remoing rows with NAs
txt_indiadf <- txt_indiadf %>% 
  na.omit() 

