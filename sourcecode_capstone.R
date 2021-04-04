#CIND820 Project - Daniel Nessia

# 1. Data Acquisition & Basic Cleaning 

# A) Installed following packages:
  # install.packages("rtweet")
  # install.packages("dplyr")

  # For cleaning tweets (preprocessing)
    # install.packages("tidytext")
    # install.packages("stringr")

  # For EDA
    # install.packages("wordcloud")
    # install.packages("RColorBrewer")
  
  # additional stopwords list: https://github.com/stopwords-iso/stopwords-en/blob/master/stopwords-en.txt

  # quick sentiment values to aid with labelling target feature


# B) Use to rtweet pass twitter API authentication and scrap data

#     Storing & Passing Twitter Authentication Credentials

consumerkey <- "6sFUfo3mhD3MRuqXdBaDzywrg"
consumersecret <- "l4ClwcNDeqhk8UofNIwj5T375kS7uCKTzZXjlJRCWPVYZMj5HY"
accesstoken <- "1358963064905887744-YqegN3Jaky99Am9R7RP04qfcJDYiou"
accesssecret <- "CLBIZErp8W6vp3784wNMGcsRGqsnbTq2s3ckSDnidbdki"

library(rtweet)
Token <- create_token(
  app = "dannn9000ds",
  consumer_key = consumerkey,
  consumer_secret = consumersecret,
  access_token = accesstoken,
  access_secret = accesssecret)
get_token()

#    Scrap twitter and store to table data


tweet_df<- search_tweets2(c("#covid or #covid19 or covid or covid19", "lockdown OR #lockdown"), include_rts=FALSE, retryonratelimit = TRUE)
write.csv(tweet_df, "covidlockdowntweets.csv")

library(data.table)
fwrite(tweet_df, file ="covidlockdowntweets.csv")



# C) Basic Cleaning before pre-processing

      # reduce rows by cleaning/narrowing down on the following:
          # filter lang: eng
          Tweet_df_clean1<- tweet_df[tweet_df$lang=="en",]
          
          # remove an extra query group that is not needed
          covidhash_toremove<- grep("covid", Tweet_df_clean1$query)   
          Tweet_df_clean1<- Tweet_df_clean1[-covidhash_toremove,]
          
          # filter source: Twitter for iPhone, Twitter for Android, Twitter Web App, Twitter for iPad, Twitter for Mac
          Tweet_df_clean2<- Tweet_df_clean1[Tweet_df_clean1$source=="Twitter for Android" | Tweet_df_clean1$source=="Twitter for iPhone" | 
                                     Tweet_df_clean1$source=="Twitter Web App" | Tweet_df_clean1$source=="Twitter for iPad" | 
                                     Tweet_df_clean1$source=="Twitter for Mac",]
          

      # reduce attributes:   
          Tweet_df_clean3<- Tweet_df_clean2[, c("user_id", "created_at", "screen_name", "text", "hashtags", "source", "favorite_count", 
                                                "retweet_count", "followers_count", "friends_count", "verified", "query")]

      # Create avg sentiment attribute - this will be used to aid with labelling of target feature, as well as for EDA. 
      # A separate target feature will be manually reviewed & labelled. 
          library(sentimentr)E
          Tweet_df_sentiment <- Tweet_df_clean3
          Tweet_df_sentiment <- sentiment_by(Tweet_df_clean3$text)
          
          tweet_sentiment_analysis<- sentiment_by(Tweet_df_clean3$text)
          Tweet_df_sentiment$ave_sentiment<- test11$ave_sentiment   #only taking the avg sentiment col
          write.csv(Tweet_df_sentiment)   
      
      # Reload new dataset with (manually assigned) sentiment labels
          
          Tweet_df_clean4<- read.csv("tweets_cleaned_wsentiment.csv")
          names(Tweet_df_clean4)[names(Tweet_df_clean4)=="ï..rowid"] <- "rowid"
          
          

#=========================================================================================================
          
          
# 3. Data Preprocessing 
          
          library(tidytext)
          library(stringr)
          library(wordcloud)
          library(dplyr)
          
          tweets_df_preproc<- Tweet_df_clean4  #(backup for preprocessing)
          
          
      # remove tweets that contains a url links, twitter @user,  
          tweets_df_preproc$text<- gsub("http\\S+", "", tweets_df_preproc$text) #links with http
          tweets_df_preproc$text<- gsub("(^[^w]*)www\\.[^\\.]*\\.[[:alpha:]]{2,3}(.*$)", "\\1\\2", tweets_df_preproc$text) #links with no http
          tweets_df_preproc$text<- gsub("(\\S+).html", "", tweets_df_preproc$text) #remove any words that end with .html
          tweets_df_preproc$text<- gsub("@\\w+", "", tweets_df_preproc$text) #@user
          
          #store hashtags then delete
          j<- tweets_df_preproc$text
          tweets_df_preproc$hashtags <- regmatches(j, gregexpr("#\\w+", j, perl=T))
          tweets_df_preproc$text<- gsub("#\\w+", "", tweets_df_preproc$text) # delete #hashtags from tweets 
          
          #store emojis then delete
          j<- tweets_df_preproc$text
          tweets_df_preproc$emoji <- regmatches(j, gregexpr("\\<(.*?)\\>", j, perl=T))
          tweets_df_preproc$text<- gsub("\\<U\\+(.*?)\\>", "", tweets_df_preproc$text) # delete #emoji values
          tweets_df_preproc$text<- gsub("<|>", " ", tweets_df_preproc$text) # delete angle brackets and replace with space
          rm(j)
          
      # Remove numbers, punctuations, linebreaks, and extra spaces    
          tweets_df_preproc$text<- gsub("[0-9]", " ", tweets_df_preproc$text) #remove numbers = not needed for NLP
          tweets_df_preproc$text<- gsub("[[:punct:]]", " ", tweets_df_preproc$text) #remove punctuations as final step (intended)
          tweets_df_preproc$text<- gsub("[&â???¦TðY¥]", "", tweets_df_preproc$text) #remove special characters
          
          tweets_df_preproc$text<- sapply(tweets_df_preproc$text, function(x) { gsub("[\r\n]", " ", x) }) #remove line breaks
          tweets_df_preproc$text<- gsub("^ *|(?<= ) | *$", "", tweets_df_preproc$text, perl = TRUE) # remove extra spaces
          
      # Convert all letters to lowercase
          tweets_df_preproc$text<- tolower(tweets_df_preproc$text)
          
      # Concatenate into one list of emojis and hashtags into a string
          tweets_df_preproc$hashtags <- sapply(tweets_df_preproc$hashtags, paste, collapse=" ")
          tweets_df_preproc$hashtags <- tolower(tweets_df_preproc$hashtags)
          tweets_df_preproc$emoji <- sapply(tweets_df_preproc$emoji, paste, collapse=" ")
           

  #---TOKENIZATION and CLEANING (for EDA)------
                              
      # Create new dataset for tokenization
          
          tweets_corpus<- tweets_df_preproc %>% unnest_tokens(word, text, drop=FALSE)
          
      # Remove stopwords    
          
          #1. default tinytext
          tweets_corpus2<- tweets_corpus %>% anti_join(get_stopwords())  
          
          #2. taken from an external list (also refer to github link)
          stopwords_doc1<- read.csv("list1_stopwords.csv", header=F)
          stopwords_doc1<- stopwords_doc1[-1,]
          tweets_corpus2<- tweets_corpus2[!tweets_corpus2$word %in% stopwords_doc1, ]
          
          #3. manually reviewed stopwords and non-words with n count > 2
          stopwords_doc2<- read.csv("list2_manualreview.csv", header=F) 
          stopwords_doc2<- stopwords_doc2[-1,]
          stopwords_doc2<- c("â", stopwords_doc2)
          tweets_corpus2<- tweets_corpus2[!tweets_corpus2$word %in% stopwords_doc2, ]
          
          #create TD-IDF dataset for each Tweet
          tweets_corpus_TFIDF<- tweets_corpus2  %>%  count(rowid, word, sort = TRUE)
          tweets_corpus_TFIDF<- tweets_corpus_TFIDF %>% 
            bind_tf_idf(word, rowid, n) %>% arrange(desc(tf_idf))
          
          #create TD-IDF dataset for each user
          tweets_corpus_TFIDF2<- tweets_corpus2  %>%  count(screen_name, word, sort = TRUE)
          tweets_corpus_TFIDF2<- tweets_corpus_TFIDF2 %>% 
            bind_tf_idf(word, screen_name, n) %>% arrange(desc(tf_idf))
            
          
                 
          
 
#=========================================================================================================               

# 4. Exploratory Data Analysis 
      
          library(wordcloud)
          library(RColorBrewer)
          
          tweet_corpus3_pos1<- subset(tweets_corpus2, ave_sentiment_est > 0)
          tweet_corpus3_pos2<- subset(tweets_corpus2, sentiment_label=='POS')
          tweet_corpus4_neg1<- subset(tweets_corpus2, ave_sentiment_est < 0)
          tweet_corpus4_neg2<- subset(tweets_corpus2, sentiment_label=='NEG')
          
      # Generate Word Cloud          
          
          # Overall
          wordcloud(tweets_corpus2$word,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
                    random.color= TRUE, random.order = FALSE, max.words = 150)
          
          # Positive sentiment using sentimentr 
          wordcloud(tweet_corpus3_pos1$word,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
                    random.color= TRUE, random.order = FALSE, max.words = 150)
          
          # Positive sentiment using manually reviewed/assigned labels 
          wordcloud(tweet_corpus3_pos2$word,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
                    random.color= TRUE, random.order = FALSE, max.words = 150)
          
          # Negative sentiment using sentimentr 
          wordcloud(tweet_corpus4_neg1$word,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
                    random.color= TRUE, random.order = FALSE, max.words = 150)
          
          # Negative sentiment using manually reviewed/assigned labels 
          wordcloud(tweet_corpus4_neg2$word,min.freq = 3, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
                    random.color= TRUE, random.order = FALSE, max.words = 150)
          
         
              
#=========================================================================================================               
        
# 5. Cleaning to setup data & functions for modelling
          
      library(tm)
      library(superm1)
      library(randomForest)
      library(caret)    
          
          
      #n_: word frequency is greater than or equal to #
      #This function will be used to tweak model
      wf <- function(n_) {
        return(sum(wordfrequency_tdf[wordfrequency_tdf$freq>=n_,]$n))
      }   
          
          
          
      #go back to the untokenize data and remove stopwords/non-words
          
          #combine all stopwords, non-words, and lockdown
          stopwords_combined<- c(stopwords_doc1, stopwords_doc2, "â", "lockdown")
          
          #filter manually labelled data before removing stopwords
          tweet_df_wlabel <- tweets_df_preproc[tweets_df_preproc$sentiment_label=="POS" | tweets_df_preproc$sentiment_label=="NEG",]
          rownames(tweet_df_wlabel) <- 1:nrow(tweet_df_wlabel) #clean rowid
          
          #use tm package to remove stopwords using Corpus
          tweet_corpus_tm<- Corpus(VectorSource(tweet_df_wlabel$text))
          tweet_corpus_tm<- tm_map(tweet_corpus_tm,removeWords, stopwords_combined)
          tweet_corpus_tm<- dataframe<-data.frame(text=unlist(sapply(tweet_corpus_tm, `[`)), stringsAsFactors=F) #convert back to data frame
          
          #merge cleaned_text as column with tweet_df_wlabel
          names(tweet_corpus_tm)[names(tweet_corpus_tm) == "text"] <- "text_clean"
          #tweet_corpus_tm$text_clean<- gsub("[&â???¦TðY¥]", "", tweet_corpus_tm$text_clean)
          tweet_df_merge<- merge(tweet_df_wlabel, tweet_corpus_tm, by=0)
          tweet_df_merge$Row.names<- NULL
          tweet_df_merge$rowid<- 1:nrow(tweet_df_merge) #this will be used later to re-index rows
          

      #Word frequency of entire data set (all tweets). This will be needed in order to play/tweak the model around.
          
          #Use Corpus (TM package) to create term document matrix. 
          wordfrequency <- Corpus(VectorSource(tweet_df_merge$text_clean))
          wordfrequency_tdf <- TermDocumentMatrix(wordfrequency)
          wordfrequency_tdf <- as.matrix(wordfrequency_tdf)
          wordfrequency_tdf <- sort(rowSums(wordfrequency_tdf),decreasing=TRUE)
          wordfrequency_tdf <- data.frame(word = names(wordfrequency_tdf),freq=wordfrequency_tdf)
          wordfrequency_tdf <- wordfrequency_tdf %>% count(freq)
          
       
          
          
#=========================================================================================================   

library(superm1)
library(ranger)
library(caret)
library(randomForest)
library(dplyr)
          
          
# 6. Setup the train/test splits. 
          
      #80/20 prep train and test set (i=1)
          
          #positive label sample: 20% test, 80% is split between training and validation (another 80/20) 
          pos_df <- tweet_df_merge[tweet_df_merge$sentiment_label=="POS",]
          pos_test_df <- sample_frac(pos_df, .2)
          pos_train_df <- sample_frac(setdiff(pos_df, pos_test_df), .8)
          pos_val_df <- setdiff(setdiff(pos_df, pos_test_df), pos_train_df)
          
          #negative label sample: 20% test, 80% is split between training and validation (another 80/20)
          neg_df <- tweet_df_merge[tweet_df_merge$sentiment_label=="NEG",]
          neg_test_df <- sample_frac(neg_df, .2)
          neg_train_df <- sample_frac(setdiff(neg_df, neg_test_df), .8)
          neg_val_df <- setdiff(setdiff(neg_df, neg_test_df), neg_train_df)

          #added train/test/val label in column name called set
          train_df <- union(pos_train_df, neg_train_df)
          train_df$set <- 'train'
          test_df <- union(pos_test_df, neg_test_df)
          test_df$set <- 'test'
          val_df <- union(pos_val_df, neg_val_df)
          val_df$set <- 'val'
          
          #form train, test, and val set, and retain previous row index and its sorting
          tweet_df_8020 <- rbind(train_df, test_df, val_df)
          tweet_df_8020 <- data.frame(tweet_df_8020[,-1], row.names = tweet_df_8020[,1]) #retain previous row index (before sampling)
          tweet_df_8020 <- tweet_df_8020[order(as.numeric(rownames(tweet_df_8020))),,]  #sort by row index
          
          #provide summary of training, test and val sets
          tweet_df_8020 %>% count(set, sentiment_label)
      
      #80/20 prep train and test set (i=2)
          
          #positive label sample: 20% test, 80% is split between training and validation (another 80/20) 
          pos_df2 <- tweet_df_merge[tweet_df_merge$sentiment_label=="POS",]
          pos_test_df2 <- sample_frac(pos_df2, .2)
          pos_train_df2 <- sample_frac(setdiff(pos_df2, pos_test_df2), .8)
          pos_val_df2 <- setdiff(setdiff(pos_df2, pos_test_df2), pos_train_df2)
          
          #negative label sample: 20% test, 80% is split between training and validation (another 80/20)
          neg_df2 <- tweet_df_merge[tweet_df_merge$sentiment_label=="NEG",]
          neg_test_df2 <- sample_frac(neg_df2, .2)
          neg_train_df2 <- sample_frac(setdiff(neg_df2, neg_test_df2), .8)
          neg_val_df2 <- setdiff(setdiff(neg_df2, neg_test_df2), neg_train_df2)
          
          #added train/test/val label in column name called set
          train_df2 <- union(pos_train_df2, neg_train_df2)
          train_df2$set <- 'train'
          test_df2 <- union(pos_test_df2, neg_test_df2)
          test_df2$set <- 'test'
          val_df2 <- union(pos_val_df2, neg_val_df2)
          val_df2$set <- 'val'
          
          #form train, test, and val set, and retain previous row index and its sorting
          tweet_df2_8020 <- rbind(train_df2, test_df2, val_df2)
          tweet_df2_8020 <- data.frame(tweet_df2_8020[,-1], row.names = tweet_df2_8020[,1]) #retain previous row index (before sampling)
          tweet_df2_8020 <- tweet_df2_8020[order(as.numeric(rownames(tweet_df2_8020))),,]  #sort by row index
          
          #provide summary of training, test and val sets
          tweet_df2_8020 %>% count(set, sentiment_label) 
          

      #80/20 prep train and test set (i=3)
          
          #positive label sample: 20% test, 80% is split between training and validation (another 80/20) 
          pos_df3 <- tweet_df_merge[tweet_df_merge$sentiment_label=="POS",]
          pos_test_df3 <- sample_frac(pos_df3, .2)
          pos_train_df3 <- sample_frac(setdiff(pos_df3, pos_test_df3), .8)
          pos_val_df3 <- setdiff(setdiff(pos_df3, pos_test_df3), pos_train_df3)
          
          #negative label sample: 20% test, 80% is split between training and validation (another 80/20)
          neg_df3 <- tweet_df_merge[tweet_df_merge$sentiment_label=="NEG",]
          neg_test_df3 <- sample_frac(neg_df3, .2)
          neg_train_df3 <- sample_frac(setdiff(neg_df3, neg_test_df3), .8)
          neg_val_df3 <- setdiff(setdiff(neg_df3, neg_test_df3), neg_train_df3)
          
          #added train/test/val label in column name called set
          train_df3 <- union(pos_train_df3, neg_train_df3)
          train_df3$set <- 'train'
          test_df3 <- union(pos_test_df3, neg_test_df3)
          test_df3$set <- 'test'
          val_df3 <- union(pos_val_df3, neg_val_df3)
          val_df3$set <- 'val'
          
          #form train, test, and val set, and retain previous row index and its sorting
          tweet_df3_8020 <- rbind(train_df3, test_df3, val_df3)
          tweet_df3_8020 <- data.frame(tweet_df3_8020[,-1], row.names = tweet_df3_8020[,1]) #retain previous row index (before sampling)
          tweet_df3_8020 <- tweet_df3_8020[order(as.numeric(rownames(tweet_df3_8020))),,]  #sort by row index
          
          #provide summary of training, test and val sets
          tweet_df3_8020 %>% count(set, sentiment_label)                          
          
#=========================================================================================================           
          
# 7. Create term document matrix of tweets/words (will be repeated to tweak/compare model performance using maximum word frequency n)
#    Also setup train/test set for each tweak.
          
   # ======= i=1 =====================        
      #N=3   
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix_8020_n3 <- TfIdfVectorizer$new(max_features = wf(3), remove_stopwords=FALSE)
          TFIDF_Matrix_8020_n3  <- TFIDF_Matrix_8020_n3$fit_transform(tweet_df_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df_8020_n3 <- data.frame(cbind(TFIDF_Matrix_8020_n3 , target=tweet_df_8020$sentiment_label, tt_set=tweet_df_8020$set))
          TFIDF_df_8020_n3$target <- as.factor(TFIDF_df_8020_n3$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df_train_8020_n3<- TFIDF_df_8020_n3[TFIDF_df_8020_n3$tt_set=='train',]
          TFIDF_df_train_8020_n3$tt_set <- NULL
          TFIDF_df_test_8020_n3<- TFIDF_df_8020_n3[TFIDF_df_8020_n3$tt_set=='test',]  
          TFIDF_df_test_8020_n3$tt_set <- NULL
          TFIDF_df_val_8020_n3<- TFIDF_df_8020_n3[TFIDF_df_8020_n3$tt_set=='val',]  
          TFIDF_df_val_8020_n3$tt_set <- NULL

      #N=4, i=1    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix_8020_n4 <- TfIdfVectorizer$new(max_features = wf(4), remove_stopwords=FALSE)
          TFIDF_Matrix_8020_n4  <- TFIDF_Matrix_8020_n4$fit_transform(tweet_df_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df_8020_n4 <- data.frame(cbind(TFIDF_Matrix_8020_n4 , target=tweet_df_8020$sentiment_label, tt_set=tweet_df_8020$set))
          TFIDF_df_8020_n4$target <- as.factor(TFIDF_df_8020_n4$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df_train_8020_n4<- TFIDF_df_8020_n4[TFIDF_df_8020_n4$tt_set=='train',]
          TFIDF_df_train_8020_n4$tt_set <- NULL
          TFIDF_df_test_8020_n4<- TFIDF_df_8020_n4[TFIDF_df_8020_n4$tt_set=='test',]  
          TFIDF_df_test_8020_n4$tt_set <- NULL
          TFIDF_df_val_8020_n4<- TFIDF_df_8020_n4[TFIDF_df_8020_n4$tt_set=='val',]  
          TFIDF_df_val_8020_n4$tt_set <- NULL

      #N=5, i=1    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix_8020_n5 <- TfIdfVectorizer$new(max_features = wf(5), remove_stopwords=FALSE)
          TFIDF_Matrix_8020_n5  <- TFIDF_Matrix_8020_n5$fit_transform(tweet_df_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df_8020_n5 <- data.frame(cbind(TFIDF_Matrix_8020_n5 , target=tweet_df_8020$sentiment_label, tt_set=tweet_df_8020$set))
          TFIDF_df_8020_n5$target <- as.factor(TFIDF_df_8020_n5$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df_train_8020_n5<- TFIDF_df_8020_n5[TFIDF_df_8020_n5$tt_set=='train',]
          TFIDF_df_train_8020_n5$tt_set <- NULL
          TFIDF_df_test_8020_n5<- TFIDF_df_8020_n5[TFIDF_df_8020_n5$tt_set=='test',]  
          TFIDF_df_test_8020_n5$tt_set <- NULL
          TFIDF_df_val_8020_n5<- TFIDF_df_8020_n5[TFIDF_df_8020_n5$tt_set=='val',]  
          TFIDF_df_val_8020_n5$tt_set <- NULL
          

   # ======= i=2 =====================            
      #N=3, i=2    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix2_8020_n3 <- TfIdfVectorizer$new(max_features = wf(3), remove_stopwords=FALSE)
          TFIDF_Matrix2_8020_n3  <- TFIDF_Matrix2_8020_n3$fit_transform(tweet_df2_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df2_8020_n3 <- data.frame(cbind(TFIDF_Matrix2_8020_n3 , target=tweet_df2_8020$sentiment_label, tt_set=tweet_df2_8020$set))
          TFIDF_df2_8020_n3$target <- as.factor(TFIDF_df2_8020_n3$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df2_train_8020_n3<- TFIDF_df2_8020_n3[TFIDF_df2_8020_n3$tt_set=='train',]
          TFIDF_df2_train_8020_n3$tt_set <- NULL
          TFIDF_df2_test_8020_n3<- TFIDF_df2_8020_n3[TFIDF_df2_8020_n3$tt_set=='test',]  
          TFIDF_df2_test_8020_n3$tt_set <- NULL
          TFIDF_df2_val_8020_n3<- TFIDF_df2_8020_n3[TFIDF_df2_8020_n3$tt_set=='val',]  
          TFIDF_df2_val_8020_n3$tt_set <- NULL
          
      #N=4, i=2    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix2_8020_n4 <- TfIdfVectorizer$new(max_features = wf(4), remove_stopwords=FALSE)
          TFIDF_Matrix2_8020_n4  <- TFIDF_Matrix2_8020_n4$fit_transform(tweet_df2_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df2_8020_n4 <- data.frame(cbind(TFIDF_Matrix2_8020_n4 , target=tweet_df2_8020$sentiment_label, tt_set=tweet_df2_8020$set))
          TFIDF_df2_8020_n4$target <- as.factor(TFIDF_df2_8020_n4$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df2_train_8020_n4<- TFIDF_df2_8020_n4[TFIDF_df2_8020_n4$tt_set=='train',]
          TFIDF_df2_train_8020_n4$tt_set <- NULL
          TFIDF_df2_test_8020_n4<- TFIDF_df2_8020_n4[TFIDF_df2_8020_n4$tt_set=='test',]  
          TFIDF_df2_test_8020_n4$tt_set <- NULL
          TFIDF_df2_val_8020_n4<- TFIDF_df2_8020_n4[TFIDF_df2_8020_n4$tt_set=='val',]  
          TFIDF_df2_val_8020_n4$tt_set <- NULL
          
      #N=5, i=2    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix2_8020_n5 <- TfIdfVectorizer$new(max_features = wf(5), remove_stopwords=FALSE)
          TFIDF_Matrix2_8020_n5  <- TFIDF_Matrix2_8020_n5$fit_transform(tweet_df2_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df2_8020_n5 <- data.frame(cbind(TFIDF_Matrix2_8020_n5 , target=tweet_df2_8020$sentiment_label, tt_set=tweet_df2_8020$set))
          TFIDF_df2_8020_n5$target <- as.factor(TFIDF_df2_8020_n5$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df2_train_8020_n5<- TFIDF_df2_8020_n5[TFIDF_df2_8020_n5$tt_set=='train',]
          TFIDF_df2_train_8020_n5$tt_set <- NULL
          TFIDF_df2_test_8020_n5<- TFIDF_df2_8020_n5[TFIDF_df2_8020_n5$tt_set=='test',]  
          TFIDF_df2_test_8020_n5$tt_set <- NULL 
          TFIDF_df2_val_8020_n5<- TFIDF_df2_8020_n5[TFIDF_df2_8020_n5$tt_set=='val',]  
          TFIDF_df2_val_8020_n5$tt_set <- NULL 
        
  # ======= i=3 =====================            
      #N=3, i=3    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix3_8020_n3 <- TfIdfVectorizer$new(max_features = wf(3), remove_stopwords=FALSE)
          TFIDF_Matrix3_8020_n3  <- TFIDF_Matrix3_8020_n3$fit_transform(tweet_df3_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df3_8020_n3 <- data.frame(cbind(TFIDF_Matrix3_8020_n3 , target=tweet_df3_8020$sentiment_label, tt_set=tweet_df3_8020$set))
          TFIDF_df3_8020_n3$target <- as.factor(TFIDF_df3_8020_n3$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df3_train_8020_n3<- TFIDF_df3_8020_n3[TFIDF_df3_8020_n3$tt_set=='train',]
          TFIDF_df3_train_8020_n3$tt_set <- NULL
          TFIDF_df3_test_8020_n3<- TFIDF_df3_8020_n3[TFIDF_df3_8020_n3$tt_set=='test',]  
          TFIDF_df3_test_8020_n3$tt_set <- NULL
          TFIDF_df3_val_8020_n3<- TFIDF_df3_8020_n3[TFIDF_df3_8020_n3$tt_set=='val',]  
          TFIDF_df3_val_8020_n3$tt_set <- NULL
          
      #N=4, i=3    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix3_8020_n4 <- TfIdfVectorizer$new(max_features = wf(4), remove_stopwords=FALSE)
          TFIDF_Matrix3_8020_n4  <- TFIDF_Matrix3_8020_n4$fit_transform(tweet_df3_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df3_8020_n4 <- data.frame(cbind(TFIDF_Matrix3_8020_n4 , target=tweet_df3_8020$sentiment_label, tt_set=tweet_df3_8020$set))
          TFIDF_df3_8020_n4$target <- as.factor(TFIDF_df3_8020_n4$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df3_train_8020_n4<- TFIDF_df3_8020_n4[TFIDF_df3_8020_n4$tt_set=='train',]
          TFIDF_df3_train_8020_n4$tt_set <- NULL
          TFIDF_df3_test_8020_n4<- TFIDF_df3_8020_n4[TFIDF_df3_8020_n4$tt_set=='test',]  
          TFIDF_df3_test_8020_n4$tt_set <- NULL
          TFIDF_df3_val_8020_n4<- TFIDF_df3_8020_n4[TFIDF_df3_8020_n4$tt_set=='val',]  
          TFIDF_df3_val_8020_n4$tt_set <- NULL
          
      #N=5, i=3    
          #CREATE TFIDF MATRIX using TFIdfVectorizer from superm1 package
          TFIDF_Matrix3_8020_n5 <- TfIdfVectorizer$new(max_features = wf(5), remove_stopwords=FALSE)
          TFIDF_Matrix3_8020_n5  <- TFIDF_Matrix3_8020_n5$fit_transform(tweet_df3_8020$text_clean)          
          
          #Convert to data frame and re-add target feature (sentiment label) and training/test indicator
          TFIDF_df3_8020_n5 <- data.frame(cbind(TFIDF_Matrix3_8020_n5 , target=tweet_df3_8020$sentiment_label, tt_set=tweet_df3_8020$set))
          TFIDF_df3_8020_n5$target <- as.factor(TFIDF_df3_8020_n5$target) #needed to change target feature to factor for random forest to work
          
          #Separate training and test sets; remove training/set indicator column
          TFIDF_df3_train_8020_n5<- TFIDF_df3_8020_n5[TFIDF_df3_8020_n5$tt_set=='train',]
          TFIDF_df3_train_8020_n5$tt_set <- NULL
          TFIDF_df3_test_8020_n5<- TFIDF_df3_8020_n5[TFIDF_df3_8020_n5$tt_set=='test',]  
          TFIDF_df3_test_8020_n5$tt_set <- NULL
          TFIDF_df3_val_8020_n5<- TFIDF_df3_8020_n5[TFIDF_df3_8020_n5$tt_set=='val',]  
          TFIDF_df3_val_8020_n5$tt_set <- NULL
                                
         
#=========================================================================================================             
          
# 8. Train random forest
      
      mtry_N3<- sqrt(wf(3)-1)
      mtry_N4<- sqrt(wf(4)-1)
      mtry_N5<- sqrt(wf(5)-1)
              
      #Random Forest for 80/20, i=1, N=3, results
          set.seed(71)
          
          #train default random forest
          rf_8020_n3 <- randomForest(target~.,data=TFIDF_df_train_8020_n3, ntree=500, mtry=floor(mtry_N3))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 60% of the rounded down default mtry, which is 31. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n3 = seq(31-floor(31*0.6), 31+floor(31*0.6), by=2)
          mtry_table_iter1_n3 = data.frame()
          mtry_list_iter1_n3 = list()
          tempnum=1
          for (i in mtry_iter1_n3) {
              rf1 <- randomForest(target~.,data=TFIDF_df_val_8020_n3, ntree=500, mtry=i)
              mtry_table_iter1_n3[tempnum,1] <- tempnum
              mtry_table_iter1_n3[tempnum,2] <- i
              mtry_table_iter1_n3[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
              mtry_list_iter1_n3[[tempnum]] <- rf1
              tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=10 (same as default)
          rf_8020_n3_mtry_best <- randomForest(target~.,data=TFIDF_df_train_8020_n3, ntree=500, mtry=10)
          
          #compare results
          print(rf_8020_n3)
          print(rf_8020_n3_mtry_best)
          
          #predict results of test set using trained models
          p2_8020_n3_iter1 <- predict(rf_8020_n3, TFIDF_df_test_8020_n3)
          p2_8020_n3_iter1_mtry_best<- predict(rf_8020_n3_mtry_best, TFIDF_df_test_8020_n3)
          confusionMatrix(p2_8020_n3_iter1, TFIDF_df_test_8020_n3$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n3_iter1_mtry_best, TFIDF_df_test_8020_n3$target, mode="prec_recall", positive="POS")

          
      #Random Forest for 80/20, i=2, N=3, results
          
          #train default random forest
          rf2_8020_n3 <- randomForest(target~.,data=TFIDF_df2_train_8020_n3, ntree=500, mtry=floor(mtry_N3))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 60% of the rounded down default mtry, which is 31. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n3 = seq(31-floor(31*0.6), 31+floor(31*0.6), by=2)
          mtry_table_iter2_n3 = data.frame()
          mtry_list_iter2_n3 = list()
          tempnum=1
          for (i in mtry_iter1_n3) {
            rf1 <- randomForest(target~.,data=TFIDF_df2_val_8020_n3, ntree=500, mtry=i)
            mtry_table_iter2_n3[tempnum,1] <- tempnum
            mtry_table_iter2_n3[tempnum,2] <- i
            mtry_table_iter2_n3[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter2_n3[[tempnum]] <- rf1
            tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=17
          rf2_8020_n3_mtry_best <- randomForest(target~.,data=TFIDF_df2_train_8020_n3, ntree=500, mtry=17)
          
          #compare results
          print(rf2_8020_n3)
          print(rf2_8020_n3_mtry_best)
          
          #results for test set
          p2_8020_n3_iter2 <- predict(rf2_8020_n3, TFIDF_df2_test_8020_n3)
          p2_8020_n3_iter2_mtry_best<- predict(rf2_8020_n3_mtry_best, TFIDF_df2_test_8020_n3)
          confusionMatrix(p2_8020_n3_iter2, TFIDF_df2_test_8020_n3$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n3_iter2_mtry_best, TFIDF_df2_test_8020_n3$target, mode="prec_recall", positive="POS")

          
      #Random Forest for 80/20, i=3, N=3, results
          
          #train default random forest
          rf3_8020_n3 <- randomForest(target~.,data=TFIDF_df3_train_8020_n3, ntree=500, mtry=floor(mtry_N3))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 60% of the rounded down default mtry, which is 31. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n3 = seq(31-floor(31*0.6), 31+floor(31*0.6), by=2)
          mtry_table_iter3_n3 = data.frame()
          mtry_list_iter3_n3 = list()
          tempnum=1
          for (i in mtry_iter1_n3) {
            rf1 <- randomForest(target~.,data=TFIDF_df3_val_8020_n3, ntree=500, mtry=i)
            mtry_table_iter3_n3[tempnum,1] <- tempnum
            mtry_table_iter3_n3[tempnum,2] <- i
            mtry_table_iter3_n3[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter3_n3[[tempnum]] <- rf1
            tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=33
          rf3_8020_n3_mtry_best <- randomForest(target~.,data=TFIDF_df3_train_8020_n3, ntree=500, mtry=33)
          
          #compare results
          print(rf3_8020_n3)
          print(rf3_8020_n3_mtry_best)
          
          #results for test set
          p2_8020_n3_iter3 <- predict(rf3_8020_n3, TFIDF_df3_test_8020_n3)
          p2_8020_n3_iter3_mtry_best<- predict(rf3_8020_n3_mtry_best, TFIDF_df3_test_8020_n3)
          confusionMatrix(p2_8020_n3_iter3, TFIDF_df3_test_8020_n3$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n3_iter3_mtry_best, TFIDF_df3_test_8020_n3$target, mode="prec_recall", positive="POS")                    
         
          
      #Random Forest for 80/20, i=1, N=4, results
          set.seed(71)
          
          #train default random forest
          rf_8020_n4 <- randomForest(target~.,data=TFIDF_df_train_8020_n4, ntree=500, mtry=floor(mtry_N4))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 58% of the rounded down default mtry, which is 25. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n4 = seq(25-floor(25*0.58), 25+floor(25*0.58), by=2)
          mtry_table_iter1_n4 = data.frame()
          mtry_list_iter1_n4 = list()
          tempnum=1
          for (i in mtry_iter1_n4) {
            rf1 <- randomForest(target~.,data=TFIDF_df_val_8020_n4, ntree=500, mtry=i)
            mtry_table_iter1_n4[tempnum,1] <- tempnum
            mtry_table_iter1_n4[tempnum,2] <- i
            mtry_table_iter1_n4[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter1_n4[[tempnum]] <- rf1
            tempnum=tempnum+1
          }

          #tuned random forest: best mtry=19
          rf_8020_n4_mtry_best <- randomForest(target~.,data=TFIDF_df_train_8020_n4, ntree=500, mtry=19)
          #tuned random forest: test mtry=31
          rf_8020_n4_mtry_test <- randomForest(target~.,data=TFIDF_df_train_8020_n4, ntree=500, mtry=31)
          
          #compare results
          print(rf_8020_n4)
          print(rf_8020_n4_mtry_best)
          
          #predict results of test set using trained models
          p2_8020_n4_iter1 <- predict(rf_8020_n4, TFIDF_df_test_8020_n4)
          p2_8020_n4_iter1_mtry_best<- predict(rf_8020_n4_mtry_best, TFIDF_df_test_8020_n4)
          p2_8020_n4_iter1_mtry_test<- predict(rf_8020_n4_mtry_test, TFIDF_df_test_8020_n4)
          confusionMatrix(p2_8020_n4_iter1, TFIDF_df_test_8020_n4$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n4_iter1_mtry_best, TFIDF_df_test_8020_n4$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n4_iter1_mtry_test, TFIDF_df_test_8020_n4$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n4_iter1_mtry_test, TFIDF_df_test_8020_n4$target, mode="prec_recall", positive="NEG")
          
      #Random Forest for 80/20, i=2, N=4, results
          
          #train default random forest
          rf2_8020_n4 <- randomForest(target~.,data=TFIDF_df2_train_8020_n4, ntree=500, mtry=floor(mtry_N4))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 58% of the rounded down default mtry, which is 25. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n4 = seq(25-floor(25*0.58), 25+floor(25*0.58), by=2)
          mtry_table_iter2_n4 = data.frame()
          mtry_list_iter2_n4 = list()
          tempnum=1
          for (i in mtry_iter1_n4) {
            rf1 <- randomForest(target~.,data=TFIDF_df2_val_8020_n4, ntree=500, mtry=i)
            mtry_table_iter2_n4[tempnum,1] <- tempnum
            mtry_table_iter2_n4[tempnum,2] <- i
            mtry_table_iter2_n4[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter2_n4[[tempnum]] <- rf1
            tempnum=tempnum+1
          }

          #tuned random forest: best mtry=29
          rf2_8020_n4_mtry_best <- randomForest(target~.,data=TFIDF_df2_train_8020_n4, ntree=500, mtry=29)
          #tuned random forest: test mtry=31
          rf2_8020_n4_mtry_test <- randomForest(target~.,data=TFIDF_df2_train_8020_n4, ntree=500, mtry=31)
          
          #compare results
          print(rf2_8020_n4)
          print(rf2_8020_n4_mtry_best)
          
          #results for test set
          p2_8020_n4_iter2 <- predict(rf2_8020_n4, TFIDF_df2_test_8020_n4)
          p2_8020_n4_iter2_mtry_best<- predict(rf2_8020_n4_mtry_best, TFIDF_df2_test_8020_n4)
          p2_8020_n4_iter2_mtry_test<- predict(rf2_8020_n4_mtry_test, TFIDF_df2_test_8020_n4)
          confusionMatrix(p2_8020_n4_iter2, TFIDF_df2_test_8020_n4$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n4_iter2_mtry_best, TFIDF_df2_test_8020_n4$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n4_iter2_mtry_test, TFIDF_df2_test_8020_n4$target, mode="prec_recall", positive="POS")
          
          
      #Random Forest for 80/20, i=3, N=4, results
          
          #train default random forest
          rf3_8020_n4 <- randomForest(target~.,data=TFIDF_df3_train_8020_n4, ntree=500, mtry=floor(mtry_N4))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 58% of the rounded down default mtry, which is 25. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n4 = seq(25-floor(25*0.58), 25+floor(25*0.58), by=2)
          mtry_iter1_n4 = seq(25-floor(25*0.58), 25+floor(25*0.58), by=2)
          mtry_table_iter3_n4 = data.frame()
          mtry_list_iter3_n4 = list()
          tempnum=1
          for (i in mtry_iter1_n4) {
            rf1 <- randomForest(target~.,data=TFIDF_df3_val_8020_n4, ntree=500, mtry=i)
            mtry_table_iter3_n4[tempnum,1] <- tempnum
            mtry_table_iter3_n4[tempnum,2] <- i
            mtry_table_iter3_n4[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter3_n4[[tempnum]] <- rf1
            tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=31
          rf3_8020_n4_mtry_best <- randomForest(target~.,data=TFIDF_df3_train_8020_n4, ntree=500, mtry=31)
          
          #compare results
          print(rf3_8020_n4)
          print(rf3_8020_n4_mtry_best)
          
          #results for test set
          p2_8020_n4_iter3 <- predict(rf3_8020_n4, TFIDF_df3_test_8020_n4)
          p2_8020_n4_iter3_mtry_best<- predict(rf3_8020_n4_mtry_best, TFIDF_df3_test_8020_n4)
          confusionMatrix(p2_8020_n4_iter3, TFIDF_df3_test_8020_n4$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n4_iter3_mtry_best, TFIDF_df3_test_8020_n4$target, mode="prec_recall", positive="POS")             
  
                  
          
      #Random Forest for 80/20, i=1, N=5, results
          set.seed(71)
          
          #train default random forest
          rf_8020_n5 <- randomForest(target~.,data=TFIDF_df_train_8020_n5, ntree=500, mtry=floor(mtry_N5))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 58% of the rounded down default mtry, which is 22. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n5 = seq(22-floor(22*0.58), 22+floor(22*0.58), by=2)
          mtry_table_iter1_n5 = data.frame()
          mtry_list_iter1_n5 = list()
          tempnum=1
          for (i in mtry_iter1_n5) {
            rf1 <- randomForest(target~.,data=TFIDF_df_val_8020_n5, ntree=500, mtry=i)
            mtry_table_iter1_n5[tempnum,1] <- tempnum
            mtry_table_iter1_n5[tempnum,2] <- i
            mtry_table_iter1_n5[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter1_n5[[tempnum]] <- rf1
            tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=30
          rf_8020_n5_mtry_best <- randomForest(target~.,data=TFIDF_df_train_8020_n5, ntree=500, mtry=30)
          
          #compare results
          print(rf_8020_n5)
          print(rf_8020_n5_mtry_best)
          
          #predict results of test set using trained models
          p2_8020_n5_iter1 <- predict(rf_8020_n5, TFIDF_df_test_8020_n5)
          p2_8020_n5_iter1_mtry_best<- predict(rf_8020_n5_mtry_best, TFIDF_df_test_8020_n5)
          confusionMatrix(p2_8020_n5_iter1, TFIDF_df_test_8020_n5$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n5_iter1_mtry_best, TFIDF_df_test_8020_n5$target, mode="prec_recall", positive="POS")
          
          
      #Random Forest for 80/20, i=2, N=5, results
          
          #train default random forest
          rf2_8020_n5 <- randomForest(target~.,data=TFIDF_df2_train_8020_n5, ntree=500, mtry=floor(mtry_N5))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 58% of the rounded down default mtry, which is 22. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n5 = seq(22-floor(22*0.58), 22+floor(22*0.58), by=2)
          mtry_table_iter2_n5 = data.frame()
          mtry_list_iter2_n5 = list()
          tempnum=1
          for (i in mtry_iter1_n5) {
            rf1 <- randomForest(target~.,data=TFIDF_df2_val_8020_n5, ntree=500, mtry=i)
            mtry_table_iter2_n5[tempnum,1] <- tempnum
            mtry_table_iter2_n5[tempnum,2] <- i
            mtry_table_iter2_n5[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter2_n5[[tempnum]] <- rf1
            tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=18
          rf2_8020_n5_mtry_best <- randomForest(target~.,data=TFIDF_df2_train_8020_n5, ntree=500, mtry=18)
          
          #compare results
          print(rf2_8020_n5)
          print(rf2_8020_n5_mtry_best)
          
          #results for test set
          p2_8020_n5_iter2 <- predict(rf2_8020_n5, TFIDF_df2_test_8020_n5)
          p2_8020_n5_iter2_mtry_best<- predict(rf2_8020_n5_mtry_best, TFIDF_df2_test_8020_n5)
          confusionMatrix(p2_8020_n5_iter2, TFIDF_df2_test_8020_n5$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n5_iter2_mtry_best, TFIDF_df2_test_8020_n5$target, mode="prec_recall", positive="POS")
          
          
      #Random Forest for 80/20, i=3, N=5, results
          
          #train default random forest
          rf3_8020_n5 <- randomForest(target~.,data=TFIDF_df3_train_8020_n5, ntree=500, mtry=floor(mtry_N5))
          
          #find best mtry using validation set 
          #default mtry is the square root of the number of predictor variables rounded down (According to r package doc)
          #the mtry range is +/- 58% of the rounded down default mtry, which is 22. Increments of 2.
          #only tuning mtry; other possible tuning can be done on ntree, min node size, and sample size
          mtry_iter1_n5 = seq(22-floor(22*0.58), 22+floor(22*0.58), by=2)
          mtry_table_iter3_n5 = data.frame()
          mtry_list_iter3_n5 = list()
          tempnum=1
          for (i in mtry_iter1_n5) {
            rf1 <- randomForest(target~.,data=TFIDF_df3_val_8020_n5, ntree=500, mtry=i)
            mtry_table_iter3_n5[tempnum,1] <- tempnum
            mtry_table_iter3_n5[tempnum,2] <- i
            mtry_table_iter3_n5[tempnum,3] <- rf1$err.rate[nrow(rf1$err.rate)]
            mtry_list_iter3_n5[[tempnum]] <- rf1
            tempnum=tempnum+1
          }
          
          #tuned random forest: best mtry=22
          rf3_8020_n5_mtry_best <- rf3_8020_n5
          
          #compare results
          print(rf3_8020_n5)
          print(rf3_8020_n5_mtry_best)
          
          #results for test set
          p2_8020_n5_iter3 <- predict(rf3_8020_n5, TFIDF_df3_test_8020_n5)
          p2_8020_n5_iter3_mtry_best<- predict(rf3_8020_n5_mtry_best, TFIDF_df3_test_8020_n5)
          confusionMatrix(p2_8020_n5_iter3, TFIDF_df3_test_8020_n5$target, mode="prec_recall", positive="POS")
          confusionMatrix(p2_8020_n5_iter3_mtry_best, TFIDF_df3_test_8020_n5$target, mode="prec_recall", positive="POS") 
          
          
          
          
          
          
          
          
          
                    
           
      #repetitions to be done
          # rf: explore the results of the following n: {3, 4, 5}
          # repeated training-test-val split 3 times
          
          

          
          
          
          
          
          

