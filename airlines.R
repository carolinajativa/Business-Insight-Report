library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(widyr)
library(tidyr)
library(stringr)
library(scales)
library(twitteR)
library(tm)
library(ggplot2)
library(igraph)
library(ggraph)
library(reshape2)
library(wordcloud)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'fXZKq3cBuzf0SxNF3HtBhS1QP'
consumer_secret <- 'WGU570efXI1mqEewnO11ayK1VoiAcUUIXEOWGGtHq5FHid5xOi'
access_token <- '1217533548321619968-WS9uPTcaPEmusN7DUi3tlXqo9UREfG'
access_secret <- 'gtpnH4Ea9fC0DC8A4GgZ0BlrbqgHKb1dPQBOa43X4O9Fl'

# select 2 : No

###################################
#pull 3 Twitter datasets, #########

Ebay <- twitteR::searchTwitter('#ebay', n = 1000, since = '2015-01-01',retryOnRateLimit = 1e3)
ebay = twitteR::twListToDF(Ebay)

Delta<- twitteR::searchTwitter('#delta airlines #covid', n = 1000, since = '2015-01-01',retryOnRateLimit = 1e3)
delta = twitteR::twListToDF(Delta)

United<- twitteR::searchTwitter('#united airlines #covid', n = 1000, since = '2015-01-01',retryOnRateLimit = 1e3)
united= twitteR::twListToDF(United)

# create my own stop word library
# print(stop_words) 
# stop_words is dataframe with two variable, word and lexicon
cust_stop <- data_frame(word = c("http", "https", "rt", "t.co", "amp" ,"h", "a", "q", "b", "c", "n", "w", "o", "f", "g", "i", "m", "d", "u", "th", "aber", "it", "t", "al", "el"), lexicon = rep("cust", each =25))

# tokenize, rmv stop words, count
tidy_amer <- american %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) #%>%
#  count(word, sort = T)

#delta df
tidy_delta <- delta %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) #%>%
#  count(word, sort = T)

#united df
tidy_united <- united %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) #%>%
#  count(word, sort = T)

####We want to combine all the datasets and do frequencies 
# correlation is the best framework to compare
frequency <- bind_rows(mutate(tidy_amer, author = "american"),
                               mutate(tidy_delta, author = "delta"),
                               mutate(tidy_united, author = "united")
) %>% #closing bind rows
  
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion,`delta`, `united`)

print(frequency)

#correlograms
ggplot(frequency, aes(x=proportion, y=`american`, 
                              color = abs(`american`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "american", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency_twitter[frequency_twitter$author == "postmates",],
         ~proportion + `instacart`)


cor.test(data=frequency_twitter[frequency_twitter$author == "amazon fresh",],
         ~proportion + `instacart`)


