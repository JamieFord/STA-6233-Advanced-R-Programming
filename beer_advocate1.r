library(tidyverse)
library(stringr)
library(ralger)  
library(rvest)
library(RColorBrewer)
library(flextable)
library(tm)
library(SnowballC)
library(wordcloud2)
library(tidytext)

# --------------------------------------------------------------------------------------
# San Antonio Main Page
url_city <- 'https://www.beeradvocate.com/place/city/74/'

# get url links to brewers
brewers_abs_urls <- weblink_scrap(link = url_city)  #using ralger
brewers_abs_urls <- brewers_abs_urls[(str_sub(brewers_abs_urls,1,14)=='/beer/profile/')]
brewers_abs_urls <- brewers_abs_urls[(str_sub(brewers_abs_urls,-1,-1)=='/')]
brewers_abs_urls <- brewers_abs_urls[!is.na(brewers_abs_urls)]
brewers_abs_urls <- unique(brewers_abs_urls) %>% 
  url_absolute(url_city)

brewers_all <- data.frame(brewers_abs_urls)

# --------------------------------------------------------------------------------------
# Brewer Pages

# get website
#url_brewer <- 'https://www.beeradvocate.com/beer/profile/30882/'  #orig test

for (i in 1:nrow(brewers_all)) {
  url_brewer <- brewers_all[i,1]  
  html_brewer <- read_html(url_brewer)
  
  #get lists
  brewer <- html_nodes(html_brewer, 'h1')
  brewer_txt <- html_text(brewer, trim = TRUE)
  beers <- html_nodes(html_brewer, 'a b')
  if (length(beers) > 0 ) {
    beers_txt <- html_text(beers, trim = TRUE)
    styles <-html_nodes(html_brewer, '.hr_bottom_light:nth-child(2) a')
    styles_txt <- html_text(styles, trim = TRUE)
    ratings <- html_nodes(html_brewer, '.hr_bottom_light:nth-child(4) b')
    ratings_txt <- as.numeric(html_text(ratings, trim = TRUE))
    avg <- html_nodes(html_brewer, '.hr_bottom_light:nth-child(5) b')
    avg_txt <- as.numeric(html_text(avg, trim = TRUE))
    
    # get url links to beers
    beers_abs_urls <- weblink_scrap(link = url_brewer) #using ralger
    beers_abs_urls2 <- beers_abs_urls[(str_sub(beers_abs_urls,1,14)=='/beer/profile/')]
    beers_abs_urls3 <- beers_abs_urls2[(str_sub(beers_abs_urls2,-1,-1)=='/')]
    beers_abs_urls4 <- beers_abs_urls3[!is.na(beers_abs_urls3)]
    beers_abs_urls <- beers_abs_urls4 %>% 
      url_absolute(url_brewer)
    
    #combine lists into dataframe
    if (i==1) {
      beers_all <- data.frame(beers_txt,styles_txt,ratings_txt,avg_txt,beers_abs_urls)
      beers_all$brewer <- brewer_txt
    } else {
      beers_temp <- data.frame(beers_txt,styles_txt,ratings_txt,avg_txt,beers_abs_urls)
      beers_temp$brewer <- brewer_txt   
      beers_all <- rbind(beers_all, beers_temp)
    }
  }
}


# --------------------------------------------------------------------------------------
# Individual Beer Pages

# Read page (for beers with ratings)
beers_all_ratings <- filter(beers_all, ratings_txt >= 1)
for (i in 1:nrow(beers_all_ratings)) {
  tryCatch( {                                             # if error, keep working
    url_ind_beer <- beers_all_ratings[i,'beers_abs_urls']  
    html_ind_beer <- read_html(url_ind_beer)
    review <- html_nodes(html_ind_beer, xpath=paste(selectr::css_to_xpath("#rating_fullview_content_2"), "/text()")) 

    # create dataframe
    if (i==1) {
      review_txt <- data.frame(html_text(review, trim = TRUE)) %>% 
        rename(ind_beer_comment=1) %>%
        filter(ind_beer_comment != 'rDev')
      review_txt$ind_beer_name <- beers_all_ratings[i,'beers_txt']  
      review_txt$ind_beer_style <- beers_all_ratings[i,'styles_txt']  
      review_txt$url_ind_beer <- beers_all_ratings[i,'beers_abs_urls']  
    } else {
      review_txt_temp <- data.frame(html_text(review, trim = TRUE)) %>% 
        rename(ind_beer_comment=1) %>%
        filter(ind_beer_comment != 'rDev')
      review_txt_temp$ind_beer_name <- beers_all_ratings[i,'beers_txt']  
      review_txt_temp$ind_beer_style <- beers_all_ratings[i,'styles_txt']  
      review_txt_temp$url_ind_beer <- beers_all_ratings[i,'beers_abs_urls']    
      review_txt <- rbind(review_txt, review_txt_temp)
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


# --------------------------------------------------------------------------------------

# Style Groupings
url_style_list <- 'https://www.beeradvocate.com/beer/styles/'
html_style_list <- read_html(url_style_list)
style_list_hdr <- html_nodes(html_style_list, 'b')
style_list_hdr_txt <- html_text(style_list_hdr, trim = TRUE)
style_list <- html_nodes(html_style_list, '.stylebreak li')
style_list_txt <- html_text(style_list, trim = TRUE) %>% 
  str_remove("[*]") %>% 
  str_trim()  

# Assign Styles to Categories
bocks <- cbind(style_list_txt[1:5], style_list_hdr_txt[1])
brownales <- cbind(style_list_txt[6:10], style_list_hdr_txt[2])
darkales <- cbind(style_list_txt[11:14], style_list_hdr_txt[3])
darklagers <- cbind(style_list_txt[15:21], style_list_hdr_txt[4])
hybrids <- cbind(style_list_txt[22:25], style_list_hdr_txt[5])
ipas <- cbind(style_list_txt[26:32], style_list_hdr_txt[6])
paleales <- cbind(style_list_txt[33:45], style_list_hdr_txt[7])
palelagers <- cbind(style_list_txt[46:58], style_list_hdr_txt[8])
porters <- cbind(style_list_txt[59:64], style_list_hdr_txt[9])
specialty <- cbind(style_list_txt[65:76], style_list_hdr_txt[10])
stouts <- cbind(style_list_txt[77:84], style_list_hdr_txt[11])
strongales <- cbind(style_list_txt[85:96], style_list_hdr_txt[12])
wheats <- cbind(style_list_txt[97:102], style_list_hdr_txt[13])
sours <- cbind(style_list_txt[103:113], style_list_hdr_txt[14])
styles_combined <- data.frame(rbind(bocks,brownales,darkales,darklagers,hybrids,ipas,paleales,
                         palelagers,porters,specialty,stouts,strongales,wheats,sours)) %>%
  rename(styles_txt=1,style_hdr=2)    


#Group / Summarize Styles
beer_style <- beers_all %>%
  group_by(styles_txt) %>%
  summarize(style_count = n(), style_mean = round(mean(avg_txt, na.rm = TRUE),2)) %>%
  left_join(styles_combined) %>%
  mutate(rank_count = min_rank(desc(style_count))) %>%
  mutate(rank_rating = min_rank(desc(style_mean)))
  
# Graphing Individual Styles
mycolors = c(brewer.pal(name="Set1", n = 9), brewer.pal(name="Set3", n = 5))

style_plot_avg <- ggplot(subset(beer_style, style_mean > 0.00), aes(x=reorder(styles_txt, style_mean), y=style_mean, fill=style_hdr)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.y = element_text(size=5,color='black'), axis.title.y = element_text(size=9),
        axis.text.x = element_text(size=7), axis.title.x = element_text(size=9)) +  
  theme(legend.position="right",legend.title = element_text(size=8, face="bold"),
        legend.text = element_text(size=7)) + 
  scale_fill_manual(name="Style\nCategory", values = mycolors) + 
  labs(title="San Antonio Brews: Style Average Ratings", x="Style", y = "Rating")

count_max <- max(beer_style$style_count)
style_plot_count <- ggplot(data=beer_style, aes(x=reorder(styles_txt, style_count), y=style_count, fill=style_hdr)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, count_max)) +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.y = element_text(size=5), axis.title.y = element_text(size=9),
        axis.text.x = element_text(size=7), axis.title.x = element_text(size=9)) +  
  theme(legend.position="right",legend.title = element_text(size=8, face="bold"),
        legend.text = element_text(size=7)) + 
  scale_fill_manual(name="Style\nCategory", values = mycolors) + 
  labs(title="San Antonio Brews: Style Counts", x="Style", y = "Count")

#Top 10 lists
top10_count <- slice_max(beer_style,style_count,n=10)
top10_rating <- slice_max(beer_style,style_mean,n=10)

# Group by Style Category
beer_category <- beer_style %>%
  group_by(style_hdr) %>%
  summarize(style_count = sum(style_count), style_mean = round(mean(style_mean, na.rm = TRUE),2)) %>%
  mutate(rank_count = min_rank(desc(style_count))) %>%
  mutate(rank_rating = min_rank(desc(style_mean)))

# Graphing Style Category
categ_plot_avg <- ggplot(data=beer_category, aes(x=reorder(style_hdr, style_mean), y=style_mean, fill=style_hdr)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  coord_flip() + 
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.y = element_text(size=7,color='black'), axis.title.y = element_text(size=9),
        axis.text.x = element_text(size=7), axis.title.x = element_text(size=9)) +  
  scale_fill_manual(name="Style\nCategory", values = mycolors) + 
  geom_text(aes(label=style_mean), hjust=-1, vjust=0.2, size=2.5, fontface = "bold") +
  labs(title="San Antonio Brews: Style Category Average Ratings", x="Style Category\n", y = "Rating")

count_max2 <- max(beer_category$style_count) + 10
categ_plot_count <- ggplot(data=beer_category, aes(x=reorder(style_hdr, style_count), y=style_count, fill=style_hdr)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, count_max2)) +
  coord_flip() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.y = element_text(size=7), axis.title.y = element_text(size=9),
        axis.text.x = element_text(size=7), axis.title.x = element_text(size=9)) +  
  theme(legend.position = "none") +
  scale_fill_manual(name="Style\nCategory", values = mycolors) +  
  geom_text(aes(label=style_count), hjust=-1, vjust=0.2, size=2.5, fontface = "bold") +
  labs(title="San Antonio Brews: Style Category Counts", x="Style Category\n", y = "Count")


# Tables
style_table <- beer_style %>% 
  rename(Style=1, Count=2, Count_Rank=5, Avg_Rating=3, Rating_Rank=6,Style_Category=4) %>%
  relocate(Count_Rank, .after = Count) %>%
  relocate(Rating_Rank, .after = Avg_Rating) %>%
  flextable() %>% 
  autofit() %>% 
  theme_vader()
#save_as_html("SA Beer Styles" = style_table, path = "C:/My Files/UTSA/Intro_to_R/style_table.html")

name_table <- beers_all %>%
  rename(Beer_Name=1, Style=2, Count_Ratings=3, Avg_Rating=4, Brewer=6) %>%
  arrange(Beer_Name)
name_table$Avg_Rating[name_table$Avg_Rating==0.00] <- NA
name_table <-  name_table %>%
  flextable(col_keys = c("Beer_Name", "Style", "Count", "Avg_Rating", "Brewer" )) %>% 
  colformat_num(na_str = "N/A") %>% 
  autofit() %>% 
  theme_booktabs()
#save_as_html("SA Beer Names" = name_table, path = "C:/My Files/UTSA/Intro_to_R/name_table.html")

brewers_list <- beers_all %>%
  group_by(brewer) %>%
  summarize(beer_count = n()) 
brewer_table <- brewers_list %>%
  rename(Brewer=1, Count=2) %>%
  flextable() %>% 
  autofit() %>% 
  theme_tron_legacy()
#print(brewer_table, preview = "pptx")

# --------------------------------------------------------------------------------------
# Text Analysis

# Beer Names
name_analysis = Corpus(VectorSource(beers_all$beers_txt)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords('english'))) %>%
  tm_map(removeWords, c('ale')) %>%
  tm_map(removeWords, c('beer')) %>%
  tm_map(removeWords, c('lager')) %>%
  tm_map(removeWords, c('pilsner')) %>%
  tm_map(removeWords, c('porter')) %>%
  tm_map(removeWords, c('stout')) %>%
  tm_map(removeWords, c('ipa')) %>%
  tm_map(removeWords, c('wheat')) %>%
  tm_map(removeWords, c('wit')) %>%
  tm_map(removeWords, style_list_hdr_txt) %>%
  tm_map(stemDocument)
dtm <- TermDocumentMatrix(name_analysis)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)
d_freq <- filter(d,freq >= 3)
wc_name <- wordcloud2(d_freq, size=.6, shape="triangle",rotateRatio = 0.5)

# Reviews
review_txt <- review_txt %>%
  left_join(styles_combined, by = c("ind_beer_style" = "styles_txt"))

#Pale Ales
review_txt_pale <- review_txt %>% 
  filter(style_hdr=='Pale Ales')

review_analysis_pale = Corpus(VectorSource(review_txt_pale$ind_beer_comment)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords('english'))) %>%
  tm_map(removeWords, c('ale')) %>%
  tm_map(removeWords, c('beer')) %>%
  tm_map(removeWords, c('stout')) %>%
  tm_map(removeWords, c('ipa')) %>%
  tm_map(removeWords, c('wit')) %>%
  tm_map(removeWords, style_list_hdr_txt) %>%
  tm_map(stemDocument)
dtm_pale <- TermDocumentMatrix(review_analysis_pale)
m_pale <- as.matrix(dtm_pale)
v_pale <- sort(rowSums(m_pale),decreasing=TRUE)
d_pale <- data.frame(word = names(v_pale),freq=v_pale)
head(d_pale, 150)
d_freq_pale <- d_pale[1:95,]
wc_pale <- wordcloud2(d_freq_pale, size=.6, shape="triangle",rotateRatio = 0.5)

freq_2word_pale <- review_txt_pale %>%
  unnest_tokens(word, ind_beer_comment, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1=='NA') %>%
  count(word1, word2, sort = TRUE) %>%
  unite(word, word1, word2, sep = " ")

#Stouts
review_txt_stout <- review_txt %>% 
  filter(style_hdr=='Stouts')

review_analysis_stout = Corpus(VectorSource(review_txt_stout$ind_beer_comment)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, c(stopwords('english'))) %>%
  tm_map(removeWords, c('ale')) %>%
  tm_map(removeWords, c('beer')) %>%
  tm_map(removeWords, c('stout')) %>%
  tm_map(removeWords, c('ipa')) %>%
  tm_map(removeWords, c('wit')) %>%
  tm_map(removeWords, style_list_hdr_txt) %>%
  tm_map(stemDocument)
dtm_stout <- TermDocumentMatrix(review_analysis_stout)
m_stout <- as.matrix(dtm_stout)
v_stout <- sort(rowSums(m_stout),decreasing=TRUE)
d_stout <- data.frame(word = names(v_stout),freq=v_stout)
head(d_stout, 150)
d_freq_stout <- d_stout[1:101,]
wc_stout <- wordcloud2(d_freq_stout, size=.6, shape="triangle",rotateRatio = 0.5)

freq_2word_stout <- review_txt_stout %>%
  unnest_tokens(word, ind_beer_comment, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1=='NA') %>%
  count(word1, word2, sort = TRUE) %>%
  unite(word, word1, word2, sep = " ")

# Table compare 2-Word Ngrams
top7_count_pale <- slice_max(freq_2word_pale,n,n=10) %>% 
  filter(word!="pale ale") %>%
  filter(word!="san antonio") %>%
  filter(word!="beer pours") %>%
  filter(word!="pint glass")
top7_count_stout <- slice_max(freq_2word_stout,n,n=7)

top7_pale_table <- top7_count_pale %>%
  flextable() %>% 
  autofit() %>% 
  theme_tron_legacy()
#print(top7_pale_table, preview = "pptx")

top7_stout_table <- top7_count_stout %>%
  flextable() %>% 
  autofit() %>% 
  theme_tron_legacy()
#print(top7_stout_table, preview = "pptx")