library(pdftools)
library(dplyr)
library(tm)
library(readr)
library(stringr)
library(tidytext)
library(ggplot2)
library(cowplot)
library(tidyr)
library(scales)

#reading pdf and converting the text one line per row (India's NDC)
txt_india <- pdf_text("INDIA INDC TO UNFCCC.pdf") %>% 
  read_lines()

str(txt_india)

#converting the character vector into a tibble
txt_indiadf <- tibble(line=1:1399, text=txt_india)

#adding NAs to the blank rows
txt_indiadf[txt_indiadf==""] <- NA

#removing rows with NAs
txt_indiadf <- txt_indiadf %>% 
  na.omit() 

#removing rows with Sanskrit language
txt_indiadf <- txt_indiadf[-c(3:6),] 

#removing rows with page number
txt_indiadf <- txt_indiadf %>% 
  filter(!grepl("Page", text)) 

#adding a column with index number
txt_indiadf$linenumber <- 1:nrow(txt_indiadf)

#moving the index number column to the start
txt_indiadf <- txt_indiadf %>% 
  select(-line) %>% 
  relocate(linenumber, .before = text) 

#converting every letter to lower case
txt_indiadf$text <- str_to_lower(txt_indiadf$text)

#addin a column
txt_indiadf$country <- "india_ndc"

#tokenization
txt_token <- txt_indiadf %>% 
  unnest_tokens(word, text) 

#removing the stop words
txt_token <- txt_token %>% 
  anti_join(stop_words) 

#removing the rows with just numbers
txt_token <- txt_token %>% 
  filter(!str_detect(word, "\\d")) 

txt_token <- txt_token %>% 
  select(-linenumber) 

#counting the word frequencies
txt_token %>% 
  count(word, sort=T) %>% 
  View()

#most common words in NDC
p1 <- txt_token %>% 
  count(word, sort=TRUE) %>% 
  filter(n>=20) %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(n, word))+
  geom_col()+
  labs(y=NULL)
p1

#reading the US NDC------------
txt_US <- pdf_text("United States NDC April 21 2021 Final.pdf") %>% 
  read_lines()

str(txt_US)

#converting the character vector into a tibble
txt_USdf <- tibble(line=1:778, text=txt_US)

#adding NAs to the blank rows
txt_USdf[txt_USdf==""] <- NA

#removing rows with NAs
txt_USdf <- txt_USdf %>% 
  na.omit() 

txt_USdf <- txt_USdf[-8,]


#adding a column with index number
txt_USdf$linenumber <- 1:nrow(txt_USdf)

#moving the index number column to the start
txt_USdf <- txt_USdf%>% 
  select(-line) %>% 
  relocate(linenumber, .before = text) 

#converting every letter to lower case
txt_USdf$text <- str_to_lower(txt_USdf$text)

#addin a column
txt_USdf$country <- "US_ndc"

#tokenization
txt_tokenUS <- txt_USdf %>% 
  unnest_tokens(word, text) 

#removing the stop words
txt_tokenUS <- txt_tokenUS %>% 
  anti_join(stop_words) 

#removing the rows with just numbers
txt_tokenUS <- txt_tokenUS %>% 
  filter(!str_detect(word, "\\d")) 

txt_tokenUS <- txt_tokenUS %>% 
  select(-linenumber) 

#counting the word frequencies
txt_tokenUS %>% 
  count(word, sort=T) %>% 
  View()

#most common words in NDC
p2 <- txt_tokenUS %>% 
  count(word, sort=TRUE) %>% 
  filter(n>=12) %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(n, word))+
  geom_col()+
  labs(y=NULL)
p2

#arranging India and US plot together
plot_grid(p1, p2, labels = c("India", "US"),
          label_size = 12,
          label_x = 0, label_y = 0,
          hjust = -0.5, vjust = -0.5)

#binding the India and Us dataframe
frequency <- bind_rows(txt_token, txt_tokenUS) %>% View()
  count(country, word) %>% View()
  group_by(country) %>% 
  mutate(proportion=n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = country, values_from = proportion) %>% 
  pivot_longer(`india_ndc`,
               names_to = "country", values_to = "proportion") 


#frequency plot
ggplot(frequency, aes(x = proportion, y = `US_ndc`,
                      color=abs(`US_ndc`-proportion)))+
  geom_abline(color="gray40", lty=2)+
  geom_jitter(alpha=0.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5)+
  scale_x_log10(labels=percent_format())+
  scale_y_log10(labels=percent_format())+
  scale_color_gradient(limits=c(0,0.001),
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~country)+
  theme_light()+
  theme(legend.position = "none")+
  labs(y="US_ndc", x=NULL)

#correlation between words in different texts
cor.test(data=frequency[frequency$country=="india_ndc",], 
         ~ proportion + `US_ndc`)