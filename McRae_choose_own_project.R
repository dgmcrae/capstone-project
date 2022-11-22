if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(tm.plugin.factiva)) install.packages("tm.plugin.factiva", repos = "http://cran.us.r-project.org")
if(!require(guardianapi)) install.packages("guardianapi", repos = "http://cran.us.r-project.org")
if(!require(quanteda)) install.packages("quanteda", repos = "http://cran.us.r-project.org")
if(!require(quanteda.textstats)) install.packages("quanteda.textstats", repos = "http://cran.us.r-project.org")
if(!require(quanteda.textmodels)) install.packages("quanteda.textmodels", repos = "http://cran.us.r-project.org")
if(!require(quanteda.textplots)) install.packages("quanteda.textplots", repos = "http://cran.us.r-project.org")
if(!require(spacyr)) install.packages("spacyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(writexl)) install.packages("writexl", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(sentimentr)) install.packages("sentimentr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(tidytext)
library(tm)
library(tm.plugin.factiva)
library(guardianapi)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(spacyr)
library(readxl)
library(writexl)
library(data.table)
library(lubridate)
library(RColorBrewer)
library(sentimentr)
library(caret)
library(naivebayes)
library(kernlab)
library(Rborist)



##########################################################################
##  Please read this explanation of this script before starting         ## 
##########################################################################

## This project assembles its own dataset by scraping articles from 
## Factiva and from The Guardian API. After cleaning of the dataset,
## SpacyR is also used to tokenise articles into sentences and to extract 
## entities from them. I have provided the code that I used for these 
## stages as a reference, but there are two possible starting points 
## for executable code, apart from installing/loading the above packages:  
## i) Read in four .Rds files and an .xlsx at line 259 - this allows the user
##     to run the sentiment calculations and perform the fairly extensive
##     wrangling to assemble the df for machine learning use
## ii) (Recommended for most users) Read in analysis_df.rds at line 515.
##     analysis_df.rds is the data object used in the Rmd file that
##     generates the report for this project.


##########################################################################
##        Getting The Age, Herald Sun, ABC, Australian articles         ## 
##########################################################################

## First we read in articles from all pubs except Guardian Australia, 
## from htmls exported from Factiva. We use a Factiva_Reader function, 
## which combines several user functions from bit.ly/3UW2SOF
 
Factiva_Reader <- function(File_Path){
  pac <-  c("tidyverse", "tidytext", "tm", "tm.plugin.factiva")
  sapply(pac, require, character.only = TRUE) #Loading required packages,
  
  Filer <- list.files(File_Path)
  Filer <- str_extract(Filer, "\\w*\\.html")
  Filer <- Filer[!is.na(Filer)] #Creates a list of all valid files in folder
  html_files_path <- str_c(File_Path, Filer)
  
  
  html_files_path %>% 
    map(FactivaSource) %>% 
    map(Corpus, readerControl = list(language = NA)) %>% 
    map_dfr(tidy)
}

## For some reason one html file won't cooperate, 
## so we place it in a temp folder 
## and read it separately and then
## rbind the two df into one called newspapers
## arranged in chronological order
df <- Factiva_Reader("factiva_articles/")
df1 <- Factiva_Reader("temp/")
newspapers <- rbind(df, df1)

newspapers <- newspapers %>% arrange(datetimestamp) ## N.B. NZ timezone

##########################################################################
##               Getting The Guardian Australia articles                ## 
##########################################################################

##  As Guardian Australia is not available on Factiva, we use the Guardian API
## using the guardianapi package
election <- gu_content(query = "election", from_date = "2022-04-10",
                       to_date = "2022-05-20")

election_aus <- election %>% filter(production_office == "AUS")

## We now have two df - newspapers with articles from four media, 
## and election_aus with the guardian aus articles. Each contains different columns,
## we create two df to_merge_1 and to_marge_2 containing only the columns from each that
## we wish to retain, and rename the columns of to_merge_2 to match to_merge_1

to_merge1 <- newspapers %>% select(datetimestamp, origin, language, page, wordcount, author, 
                                   heading, section, text)
to_merge2 <- election_aus %>% select(web_publication_date, publication, lang, newspaper_page_number, 
                                     wordcount, byline, headline, section_id, body_text, type) %>% 
  mutate(wordcount = as.integer(wordcount))

colnames(to_merge2) <- c("datetimestamp", "origin", "language", "page", "wordcount", "author", 
                         "heading", "section", "text", "type")

## We now merge the df into an election_articles df, and carry out some basic data cleaning 
election_articles <- bind_rows(to_merge1, to_merge2)

election_articles <- election_articles %>% distinct() #removes 200 duplicate rows

## Standardise names of publications
election_articles <- election_articles %>% 
  mutate(publication = ifelse(origin %in% c("The Australian - Online", "The Australian", "The Weekend Australian Magazine"),
                              "The Australian",
                              ifelse(origin == "Australian Broadcasting Corporation News", "ABC News",
                                     ifelse(origin %in% c("theguardian.com", "The Guardian"),
                                            "Guardian Australia",
                                            ifelse(origin %in% c("Herald-Sun", "Herald Sun​ - ​Online"), "Herald Sun", "The Age"
                                            )))))

## filter out articles of less than 99 words, many of which are live blog sub-entries
election_articles_filtered <- election_articles %>% filter(wordcount > 99 & !duplicated(text))

## and filter out guardian liveblogs, as we effectively took out other pubs' liveblogs in the preceding step
## then create an index based on row numbers to use as unique document names
## as no existing identifiers of the articles are unique
election_articles_filtered <- election_articles_filtered %>% 
  filter(is.na(type) | !type == "liveblog") %>% 
  mutate(index = row_number())

## We manually standardise section codes across publications,
##and based on the article headlines, manually code the section of the articles
## where section is blank, in case we need sections in subsequent analysis

no_section <- election_articles_filtered %>% filter(is.na(section)) %>% select(-text) %>% arrange(publication)
writexl::write_xlsx(no_section, "no_section.xlsx")

no_section_coded <- readxl::read_excel("no_section_coded.xlsx")
no_section_coded <- no_section_coded %>% select(index, new_section)

sorted_section_coded <- readxl::read_excel("sorted_section_coded.xlsx")
sorted_section_coded <- sorted_section_coded %>% select(section, detailed_sections)

election_articles_filtered <- merge(x = election_articles_filtered, y = sorted_section_coded, 
                                    by.x = "section", by.y = "section", all.x = TRUE, all.y = FALSE)

election_articles_filtered <- merge(x = election_articles_filtered, y = no_section_coded, 
                                    by.x = "index", by.y = "index", all.x = TRUE, all.y = FALSE)

election_articles_filtered <- election_articles_filtered %>% mutate(new_sections = coalesce(new_section, detailed_sections))

## Checking completeness
election_articles_filtered %>% filter(is.na(new_sections))
table(election_articles_filtered$new_sections)

## we exclude articles where the section indicates that they are irrelevant
election_articles_filtered <- election_articles_filtered %>% filter(!new_sections %in% c("world", "quiz", "review")) %>% 
  select(-detailed_sections, -new_section)

## Here we create a jump out/in point to avoid repeating the basic df creation steps so far
## and rename the df with a shorter name as ndf, ie newspaper data filtered 
saveRDS(election_articles_filtered, 
        "election_articles_filtered.Rds")

ndf <- readRDS("election_articles_filtered.Rds")

## we continue with data cleaning, first removing URLs, both because they might allow an algortihm
## to identify the publications by name, and because they will not contribute to bias measures
## there are initially 648 articles containing a simple url-identifying string, http
# urls <- ndf %>% mutate(row = row_number()) %>% filter(str_detect(text, "http")) %>% pull(row) #648

## str_remove_all is extremely slow, so we use the base function gsub and example code from 
## bit.ly/3tNnnkL
## to create a function to remove URLs, then adapt this to create a new function 
## to also remove the new line markers from article text

removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
removeNEWLINE <- function(x) gsub("\n", " ", x)

## we create a clean_text column with these changes, to use in further analysis
ndf <- ndf %>% mutate(clean_text = removeURL(text),
                      clean_text = removeNEWLINE(clean_text))

## URLs and new line markers are gone
#ndf %>% filter(str_detect(clean_text, "http") | str_detect(clean_text,"\n")) %>% nrow() #0

## Next use quanteda to create corpus from the articles
corp_ndf <- corpus(ndf$clean_text, docnames = ndf$index, 
                   docvars = (ndf %>% select(-text, -clean_text, -index)))

corp_ndf_headings <- corpus(ndf$heading, docnames = ndf$index, 
                            docvars = (ndf %>% select(-text, -clean_text, -index)))

## Now use spacyr to create a tidy df of tokens
## Some set up is needed for spacyr prior to using for the first time
## See https://cran.r-project.org/web/packages/spacyr/readme/README.html

# Turn on spacy
spacy_initialize()

# spacy_parse()
# create tidy format df for headings and article text - slow step
parsed_ndf <- spacy_parse(corp_ndf)

# create entity lists for possible filtering use
parsed_ndf_entities_named <- entity_extract(parsed_ndf, type = "named", concatenator = "_")

## create a list of sentences as a df
sentences_ndf_df <- spacy_tokenize(corp_ndf, what = "sentence", output = "data.frame")

sentences_ndf_headings_df <- spacy_tokenize(corp_ndf_headings, what = "sentence", output = "data.frame")

# turn off spacy
spacy_finalize()

## We examine spacyr named entities that occur at least ten times to identify ALP-aligned and 
## Coalition-aligned entities. To examine the entities we write to an excel file which we 
## then manually code

ndf_entities_names_count10 <- parsed_ndf_entities_named %>% count(entity, sort = TRUE) %>% filter(n >= 10)
writexl::write_xlsx(ndf_entities_names_count10, "ndf_entities_names_count10.xlsx")

## we now save the rds files we will need for the executable for everyone
## section of the code
# bit.ly/3tN0aPx , bit.ly/3XiF0Xb
dfs_to_save <- list(ndf, parsed_ndf_entities_named, sentences_ndf_df, sentences_ndf_headings_df)
dfs_to_save_names <- c("ndf", "parsed_ndf_entities_named", "sentences_ndf_df", "sentences_ndf_headings_df")
currentDate <- Sys.Date()
for(i in 1:length(dfs_to_save_names)){
rdsFileName <- paste("data/", dfs_to_save_names[i], "_", currentDate,".rds",sep="")
saveRDS(dfs_to_save[[i]], file=rdsFileName)}

##########################################################################
## All code from this point onwards is executable for everyone.         ##
## To skip the calculation of sentiment scores & associated wrangling,  ##
## proceed instead to line x (recommended), where you can read in       ##
## analysis_df, the basic dataframe for the machine learning portion    ##
## of the project.                                                      ##
##########################################################################

## Files to read in
ndf <- readRDS("data/ndf_2022-11-22.rds")
parsed_ndf_entities_named <- readRDS("data/parsed_ndf_entities_named_2022-11-22.rds")
sentences_ndf_df <- readRDS("data/sentences_ndf_df_2022-11-22.rds")
sentences_ndf_headings_df <- readRDS("data/sentences_ndf_headings_df_2022-11-22.rds")
ndf_entities_names_count10_coded <- 
  readxl::read_xlsx("data/ndf_entities_names_count10_coded.xlsx")

alp_entities <-  ndf_entities_names_count10_coded %>% filter(alp_entities == 1) %>% pull(entity)
coalition_entities <-  ndf_entities_names_count10_coded %>% filter(coalition_entities == 1) %>% pull(entity)

## We now work out which articles contain keywords that indicate they discuss the primary political cleavage,
##  Labor vs Coalition
parsed_ndf_entities_named %>% filter(entity %in% alp_entities | entity %in% coalition_entities) %>% 
  distinct(doc_id) %>% nrow() #2578

## As well as how many distinct sentences - we need to group by doc_id first
## as spacyR restarts sentence count for every document
parsed_ndf_entities_named %>% filter(entity %in% alp_entities | entity %in% coalition_entities) %>% 
  group_by(doc_id) %>% distinct(sentence_id) %>% ungroup() %>% nrow() #24242

## We also seek to establish a minimum threshold for how many sentences 
## in each document contain an entity
entity_sentence_count <- parsed_ndf_entities_named %>% 
  filter(entity %in% alp_entities | entity %in% coalition_entities) %>% 
  group_by(doc_id) %>% distinct(sentence_id) %>% summarise(n = n())

## Based on testing thresholds of 5-10, we choose ten as a our threshold 
## as it leaves sufficient documents for analysis. NB. Code is hashed
## as you do not need to run it.
# sapply(c(5:10), function(x){
#  entity_sentence_count %>% filter(n>=x)%>% nrow() 
# })
 
# We create a list of doc_ids for articles that contain at least ten sentences containing
# relevant entities
ndf_docs_filtered_by_entity <- entity_sentence_count %>% filter(n>=10)%>% pull(doc_id) #1001

# A quick check on the distribution of publicationss in these articles
ndf %>% filter(index %in% ndf_docs_filtered_by_entity) %>% count(publication, sort = TRUE)
# publication   n
# 1     The Australian 362
# 2 Guardian Australia 239
# 3            The Age 230
# 4         Herald Sun  87
# 5           ABC News  83

# The list of doc ids containing these keywords, these are the articles we will use to attempt to identify political bias
ALP_Coalition_docs <- ndf_docs_filtered_by_entity
  
## Now let's filter each of our dfs and the list of sentences to only retain these docs

sentences_ndf_df_final <- sentences_ndf_df %>% filter(doc_id %in% ALP_Coalition_docs)

sentences_ndf_headings_df_final <- sentences_ndf_headings_df %>% filter(doc_id %in% ALP_Coalition_docs)

## matches the sentence numbering of parsed_ndf_final, and also
## contains a unique sentence_id which I think will actually be more use to us

sentences_ndf_df_final <- sentences_ndf_df_final %>% mutate(sentence_id = rowid(doc_id)) 
                                                
## create data frame to merge with sentences_ndf_df_final
## note sentences_ndf_df_final has one line per sentence, whereas
## parsed_ndf_entities_named_alp_coalition has as many lines per sentence as there are entities

p_ndf_e_n_alp_c_by_sentence <- parsed_ndf_entities_named %>% filter(entity %in% alp_entities | entity %in% coalition_entities) %>%
  filter(doc_id %in% ALP_Coalition_docs) %>% 
  mutate(alp_entity = ifelse(entity %in% alp_entities, TRUE, FALSE), coalition_entity= ifelse(entity %in% coalition_entities, TRUE, FALSE)) %>% 
  group_by(doc_id, sentence_id) %>% summarise(alp = max(alp_entity), coalition = max(coalition_entity)) %>% ungroup() %>% 
  mutate(new_sentence_id = row_number())

## We inner_join by doc_id and sentence_id - bit.ly/3AxmlwU
## NB: this stage cuts out all the sentences that don't contain entities.

sentences_entities_ndf_df_final <- 
  inner_join(sentences_ndf_df_final, p_ndf_e_n_alp_c_by_sentence, by = c("doc_id", "sentence_id")) %>% 
  mutate(element_id = row_number())

head(sentences_entities_ndf_df_final)

##########################################################################
##          Calculating sentences using sentimentr package              ## 
##########################################################################

## We first need to create a clean token column to prevent sentimentr 
## creating more sentences when it calculates sentiment (as it repeats
## its own process of sentence detection). This process removes 7 sentences.
## At this stage, we also create a final_stage_id based on post-filtering 
## row numbers, which we will use to match the output of sentimentr
##  with the appropriate sentence
sentences_entities_ndf_df_final <- sentences_entities_ndf_df_final %>% 
  mutate(clean_token = str_replace_all(token, "\\?(.+)", "?")) %>% 
  mutate(clean_token = str_replace_all(clean_token, "\\!(.+)", "!")) %>% 
  mutate(clean_token = str_replace_all(clean_token, "\\.(.+)", ".")) %>% 
  filter(str_detect(clean_token, "[a-zA-z]") & nchar(clean_token) > 4) %>% 
  mutate(final_stage_id = row_number())

## For headlines, we  hash out the filter line which removes sentences with 
## four characters or fewer because this deletes 5 instanced of 'The 
## Age' newspaper column, "CBD"
sentences_ndf_headings_df_final <- sentences_ndf_headings_df_final %>% 
  mutate(clean_token = str_replace_all(token, "\\?(.+)", "?")) %>% 
  mutate(clean_token = str_replace_all(clean_token, "\\!(.+)", "!")) %>% 
  mutate(clean_token = str_replace_all(clean_token, "\\.(.+)", ".")) %>% 
  #filter(str_detect(clean_token, "[a-zA-z]") & nchar(clean_token) > 4) %>% 
  mutate(final_stage_id = row_number())

## We now calculate sentiment for sentences and headlines

sentiment_sentences <- sentiment(sentences_entities_ndf_df_final$clean_token)

sentiment_headings <- sentiment(sentences_ndf_headings_df_final$clean_token)

## And add these sentiment scores back into our dfs of sentences and headlines
s_e_ndf_df_f_with_sentiment <- merge(sentences_entities_ndf_df_final, sentiment_sentences,
                                   by.x = c("final_stage_id"),
                                   by.y = c("element_id"),
                                   all.x = TRUE,
                                   all.y = FALSE)

s_ndf_h_df_f_with_sentiment <- merge(sentences_ndf_headings_df_final, sentiment_headings,
                                     by.x = c("final_stage_id"),
                                     by.y = c("element_id"),
                                     all.x = TRUE,
                                     all.y = FALSE)

## We next generate a per document sentiment score for headlines 
## by grouping by doc_id and taking the mean. This stage is necessary
## because some headlines have more than one sentence!

headline_sentiment_per_doc <- s_ndf_h_df_f_with_sentiment %>% 
  group_by(doc_id) %>% summarise(heading_sentiment = mean(sentiment))


##########################################################################
##          Assembly of our dataframe for machine learning              ## 
##########################################################################

## First step is to add back in the details of each article, we use
## quanteda's nomenclature of document variables
ndf_docvars <- ndf %>% 
  select(index, section, new_sections, datetimestamp, 
         author, heading, publication)


working_df <- merge(s_e_ndf_df_f_with_sentiment, ndf_docvars,
                    by.x = c("doc_id"),
                    by.y = c("index"),
                    all.x = TRUE,
                    all.y = FALSE)

## There are 4364 sentences that contain both a Labor and a Coalition word.
## We this cannot group_by alp and coalition as the conditions overlap
## we instead need to calculate conditional means
working_df %>% filter(alp == 1 & coalition == 1) %>% nrow() #2343

## We nevertheless create a pol_group column, in case we want to 
## easily distinguish these cases
working_df <- working_df %>% 
  mutate(pol_group = case_when(alp == 1 & coalition == 0 ~ "labor_only",
                               alp == 1 & coalition == 1 ~ "both",
                               alp == 0 & coalition == 0 ~ "neither",
                               alp == 0 & coalition == 1 ~ "coalition_only"))

## Calculating the conditional means for each document
working_df <- working_df %>% group_by(doc_id) %>% 
  mutate(alp_sentiment = mean(sentiment[alp == 1]),
         coalition_sentiment = mean(sentiment[coalition == 1]))

## We create a df to use for machine learning with average sentiment per article
## for sentences mentioning ALP and Coalition, counts of these sentences, and 
## na_0_ columns with the NAs replaced with zero value.
analysis_df <- working_df %>% group_by(doc_id) %>% 
  summarise(alp_sentiment = mean(sentiment[alp == 1]),
            coalition_sentiment = mean(sentiment[coalition == 1]),
            alp_sentences = sum(alp),
            coalition_sentences = sum(coalition)) %>% ungroup() %>% 
  mutate(na_0_alp_sentiment = coalesce(alp_sentiment, 0),
         na_0_coalition_sentiment = coalesce(coalition_sentiment,0))

## Machine learning algorithms will only work if missing values are replaced or removed, 
## We show why the na_) columns were necessary
analysis_df %>% filter(is.na(alp_sentiment)) %>% nrow() #37
analysis_df %>% filter(is.na(coalition_sentiment)) %>% nrow() #17

## We now add in the headline_sentiment_per_doc as well
analysis_df <- merge(analysis_df, headline_sentiment_per_doc,
                    by.x = c("doc_id"),
                    by.y = c("doc_id"),
                    all.x = TRUE,
                    all.y = FALSE)

## For machine learning classification based on political bias measures,
## you only need ndf_docvars, but to show the baseline
## attempt at classification using full text you need ndf
analysis_df <- merge(analysis_df, ndf,
                     by.x = c("doc_id"),
                     by.y = c("index"),
                     all.x = TRUE,
                     all.y = FALSE)

## Add weekly date bins and use to create a date-adjusted sentiment score
## by subtracting each document's ALP and Coalition sentiment score
## from their respective weekly means
analysis_df <- analysis_df %>% mutate(date_bins = floor_date(datetimestamp, "week", 
                                      week_start = getOption("lubridate.week.start", 7)))
analysis_df <- analysis_df %>% 
  group_by(date_bins) %>% mutate(ave_alp = mean(na_0_alp_sentiment),
                                ave_coalition = mean(na_0_coalition_sentiment)) %>% 
  ungroup()

analysis_df <- analysis_df %>% 
  mutate(dated_alp_sentiment = ave_alp - na_0_alp_sentiment,
          dated_coalition_sentiment = ave_coalition - na_0_coalition_sentiment)

## We return to working_df for one final step. Our final set of predictors 
## for machine learning is the text of political sentences collapsed into
## a single string for each document.
## First we need to collapse sentences into one string for each document.
## The difficulty here is that some documents do not contain either 
## ALP or Coalition sentences, so we end up with missing values again.
## We cannot substitute anything for these missing values so remove them
##  bit.ly/3EraHFe shows the required field in paste0 to collapse sentences
## 

alp_corpus <- working_df %>% filter(alp == 1) %>% group_by(doc_id) %>% 
  summarise(alp_composite = paste0(clean_token, collapse = " ")) 

coalition_corpus <- working_df %>% filter(coalition == 1) %>% group_by(doc_id) %>% 
  summarise(coalition_composite = paste0(clean_token, collapse = " ")) 

pol_sentences_corpus <- working_df %>% group_by(doc_id) %>% 
  summarise(pol_composite = paste0(clean_token, collapse = " "))

# We next create a version of analysis_df that contains our three sentence corpus
# Method to merge multiple dfs: bit.ly/3i2Ser2
dfs_to_merge <- list(analysis_df, alp_corpus, coalition_corpus, pol_sentences_corpus)
analysis_df <- dfs_to_merge %>% reduce(full_join, by='doc_id')



## Save analysis_df file to create a jump in point without
## repeating the calculation of sentiment scores and extensive
## data data wrangling. We include the date the file is created so that we 
## can be sure we are using the latest version. Code hashed out as assumed 
## other users will not be replicating this stage of code
# bit.ly/3tN0aPx
# currentDate <- Sys.Date()
# rdsFileName <- paste("data/analysis_df_",currentDate,".rds",sep="")
# saveRDS(analysis_df, file=rdsFileName)

##########################################################################
##              Machine learning only jump in point.                    ##
##                Start by reading in analysis_df.                      ##
##########################################################################

analysis_df <- readRDS("data/analysis_df_2022-11-22.rds")

## Generate a train and test set
set.seed(22, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(22)`
test_index <- createDataPartition(y = as.factor(analysis_df$publication), times = 1, p = 0.1, list = FALSE)
analysis_train_full <- analysis_df[-test_index,]
analysis_test <- analysis_df[test_index,]

## Repeat the process to obtain a probe set for model testing
set.seed(23, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(22)`
test_index1 <- createDataPartition(y = as.factor(analysis_train_full$publication), times = 1, p = 0.1, list = FALSE)
analysis_probe <- analysis_train_full[test_index1,]
analysis_train <- analysis_train_full[-test_index1,]

# Create lists of doc ids for train, test, probe, train_full to subset dfms
# Method to assign variable name: bit.ly/3XiF0Xb
# Method to use string as variable name: bit.ly/3GxcuLh
sets <- c("train", "probe", "test", "train_full")
for(i in 1:length(sets)){
  assign(paste(sets[i], "_doc_ids", sep = ""), eval(as.name(paste("analysis_", sets[i], sep = ""))) %>% pull(doc_id))
}

##########################################################################
##              Classification using full test of articles              ##
##########################################################################

## We first create a dfm from the full text of the articles,
## removing stopwords, punctuation and keywords related to the names
## of the publication outlets that our algorithms might use in classification

pub_names <- c("ABC", "abc", "Herald", "herald", "Sun", "sun", 
               "Age", "age", "Australian", "australian", "Guardian", "guardian")

analysis_dfm <- corpus(analysis_df$clean_text, 
                       docnames = analysis_df$doc_id, 
                       docvars = data.frame(publication = analysis_df$publication)) %>% #create corpus
  tokens(., remove_punct = TRUE) %>%  # tokenise to individual words; remove punctuation
  tokens_select(., pattern = stopwords("en"), 
              selection = "remove", padding = TRUE) %>% # remove stopwords
  tokens_select(., pattern = pub_names, 
              selection = "remove", padding = TRUE) %>% # remove pub name keywords
  dfm() #create dfm

## We now subset analysis_dfm into train and probe. 
## name of dfm is dfm_ at/p_ (analysis_train/probe) ns (no stopwords)
dfm_at_ns <- analysis_dfm %>% dfm_subset(., docnames(analysis_dfm) %in% train_doc_ids)
dfm_ap_ns <- analysis_dfm %>% dfm_subset(., docnames(analysis_dfm) %in% probe_doc_ids)

## Pre-machine learning data processing - Use only features in train set,
## (for bigger datasets could set trim to greater number than 1)
## then padding probe set plus removing features not in train
dfm_at_ns <- dfm_at_ns %>% dfm_trim(1)
dfm_ap_ns <- dfm_match(dfm_ap_ns, featnames(dfm_at_ns))

### Full text Naive Bayes model implemented using quanteda.textmodels ###
set.seed(25, sample.kind = "Rounding")
ft_nb_model <- textmodel_nb(dfm_at_ns, docvars(dfm_at_ns,
                                            "publication"))

# we also test a model which takes into account document frequency, as we have unbalanced numbers of 
# the classes we are seeking to classify
ft_nb_priors_model <- textmodel_nb(dfm_at_ns, docvars(dfm_at_ns,
                                                   "publication"), prior = "docfreq")

# predictions for unadjusted and adjusted based on document frequency
test_predictions_ft_nb <- predict(ft_nb_model,
                            newdata=dfm_ap_ns)

test_predictions_ft_nb_priors <- predict(ft_nb_priors_model,
                                    newdata=dfm_ap_ns)

# Calculate accuracy for Naive Bayes classification

full_text_nb_accuracy <- confusionMatrix(as.factor(docvars(dfm_ap_ns,"publication")),
                                         as.factor(test_predictions_ft_nb))$overall["Accuracy"]

# setting priors makes little difference, 
# potentially because the proportion in the probe set is very similar to the train set
full_text_nb_priors_accuracy <- confusionMatrix(as.factor(docvars(dfm_ap_ns,"publication")),
                                                as.factor(test_predictions_ft_nb_priors))$overall["Accuracy"]

### Full text Support Vector Machine model implemented using quanteda.textmodels ###
set.seed(25, sample.kind = "Rounding")
ft_svm_model <-  textmodel_svm(dfm_at_ns, docvars(dfm_at_ns, "publication"))

test_predictions_ft_svm <- predict(ft_svm_model,
                                newdata=dfm_ap_ns)

## Calculate accuracy metric for this classification effort

full_text_svm_accuracy <- confusionMatrix(as.factor(docvars(dfm_ap_ns,"publication")),
                                          as.factor(test_predictions_ft_svm))$overall["Accuracy"]


##############################################################################
##                Sentence sentiment score classification                   ##
##                                  &                                       ##
##                heading sentiment score classification                    ##
##############################################################################

## First we create a set of dfs containing the features we will use for 
## classification, and the column we are predicting, publication

raw_analysis_train_data <- analysis_train %>% select(na_0_alp_sentiment, na_0_coalition_sentiment, publication)
raw_analysis_probe_data <- analysis_probe %>% select(na_0_alp_sentiment, na_0_coalition_sentiment, publication)

dated_analysis_train_data <- analysis_train %>% select(dated_alp_sentiment, dated_coalition_sentiment, publication)
dated_analysis_probe_data <- analysis_probe %>% select(dated_alp_sentiment, dated_coalition_sentiment, publication)

combined_analysis_train_data <- analysis_train %>% select(na_0_alp_sentiment, na_0_coalition_sentiment,
                                                          dated_alp_sentiment, dated_coalition_sentiment, publication)
combined_analysis_probe_data <- analysis_probe %>% select(na_0_alp_sentiment, na_0_coalition_sentiment,
                                                          dated_alp_sentiment, dated_coalition_sentiment, publication)

headings_analysis_train_data <- analysis_train %>% select(na_0_alp_sentiment, na_0_coalition_sentiment,
                                                          #dated_alp_sentiment, dated_coalition_sentiment, 
                                                          heading_sentiment, publication)
headings_analysis_probe_data <- analysis_probe %>% select(na_0_alp_sentiment, na_0_coalition_sentiment,
                                                          #dated_alp_sentiment, dated_coalition_sentiment, 
                                                          heading_sentiment, publication)


## SVM classification ##
## We refer to https://rpubs.com/uky994/593668 tutorial on using SVM in caret
## We leave trControl at its default setting as calculation speed is not an issue
set.seed(25, sample.kind = "Rounding")
sentiment_types <- c("raw", "dated", "combined", "headings")

for(i in 1:length(sentiment_types)){
  assign(paste("train_svm_linear", sentiment_types[i], sep = "_"), 
         train(publication ~., data = eval(as.name(paste(sentiment_types[i], "analysis_train_data", sep = "_"))), 
               method = "svmLinear", 
               preProcess = c("center","scale"),
               tuneGrid = expand.grid(C = seq(0.1, 1, length = 9))))
}

svm_models <- list(train_svm_linear_raw, train_svm_linear_dated, train_svm_linear_combined, train_svm_linear_headings)
probe_sets <- list(raw_analysis_probe_data, dated_analysis_probe_data, combined_analysis_probe_data, headings_analysis_probe_data)

for(i in 1:length(svm_models)){
  assign(paste("svm_linear_accuracy", sentiment_types[i], sep = "_"),
         confusionMatrix(predict(svm_models[[i]], probe_sets[[i]], type = "raw"),
                         as.factor(probe_sets[[i]]$publication))$overall["Accuracy"] )
}


## We also try non-linear SVM on the raw sentiment scores,  but performs no better
set.seed(25, sample.kind = "Rounding")
train_svm_poly_raw <- train(publication ~., data = raw_analysis_train_data, method = "svmPoly", 
                            preProcess = c("center","scale"), 
                        tuneLength = 4)
svm_poly_raw_accuracy <- confusionMatrix(predict(train_svm_poly_raw, raw_analysis_probe_data, type = "raw"),
                           as.factor(raw_analysis_probe_data$publication))$overall["Accuracy"]


## the problem becomes clear if we examine predictions - 
## the svm model predicts every article is "The Australian" so perfectly matches prevalence
predict(train_svm_linear_raw, raw_analysis_probe_data, type = "raw")

## KNN classification ##

set.seed(25, sample.kind = "Rounding")
for(i in 1:length(sentiment_types)){
  assign(paste("train_knn", sentiment_types[i], sep = "_"), 
         train(publication ~., data = eval(as.name(paste(sentiment_types[i], "analysis_train_data", sep = "_"))), 
               method = "knn", 
               tuneGrid = data.frame(k = seq(5, 31, 2))))
}

knn_models <- list(train_knn_raw, train_knn_dated, train_knn_combined, train_knn_headings)

## best k
ggplot(knn_models[[1]], highlight = TRUE)
ggplot(knn_models[[2]], highlight = TRUE)
ggplot(knn_models[[3]], highlight = TRUE)
ggplot(knn_models[[4]], highlight = TRUE)

## accuracy
for(i in 1:length(knn_models)){
  assign(paste("knn_accuracy", sentiment_types[i], sep = "_"),
         confusionMatrix(predict(knn_models[[i]], probe_sets[[i]], type = "raw"),
                         as.factor(probe_sets[[i]]$publication))$overall["Accuracy"] )
}



## Naive Bayes classification ##
set.seed(25, sample.kind = "Rounding")
for(i in 1:length(sentiment_types)){
  assign(paste("train_nb", sentiment_types[i], sep = "_"), 
         train(publication ~., data = eval(as.name(paste(sentiment_types[i], "analysis_train_data", sep = "_"))), 
               method = "naive_bayes",
               tuneGrid = expand.grid(laplace = 0, usekernel = c(TRUE, FALSE), adjust = seq(0.5, 5, 0.5)),
               usepoisson = TRUE))
}

nb_models <- list(train_nb_raw, train_nb_dated, train_nb_combined, train_nb_headings)

## accuracy
for(i in 1:length(nb_models)){
  assign(paste("nb_accuracy", sentiment_types[i], sep = "_"),
         confusionMatrix(predict(nb_models[[i]], probe_sets[[i]], type = "raw"),
                         as.factor(probe_sets[[i]]$publication))$overall["Accuracy"] )
}




## Random Forest Classification ##
## Would need to tune but there is nothing promising 
## about a quick run that makes it seem worth the time to develop here
## Plots below suggest it's the predictors, not the algorithms
getModelInfo("Rborist")
set.seed(25, sample.kind = "Rounding")

train_rf_raw <- train(publication ~ ., method = "Rborist", 
                       data = raw_analysis_train_data)

rf_raw <- confusionMatrix(predict(train_rf_raw, raw_analysis_probe_data, type = "raw"),
                           as.factor(raw_analysis_probe_data$publication))$overall["Accuracy"]

## Plots of predictors shows why our algorithms perform so poorly - there is little or no separation
## Note we cannot plot combined as we do not have a way to visually plot four dimensions

#Set a colour scheme
pub_colours <- brewer.pal(n = 5, name = "Set2")
pub_colours

## Raw sentiment scores
analysis_train %>%  
  ggplot(aes(na_0_alp_sentiment, na_0_coalition_sentiment, colour = publication)) +
  geom_point(shape = "bullet") + 
  labs(x = "Sentiment scores - ALP sentences", y = "Sentiment scores - Coalition sentences") +
  scale_color_manual(values = pub_colours) 

## Dated sentiment scores
analysis_train %>%  
  ggplot(aes(na_0_alp_sentiment, na_0_coalition_sentiment, colour = publication)) +
  geom_point(shape = "bullet") + 
  scale_color_manual(values = pub_colours) +
  labs(x = "Sentiment scores - ALP sentences", y = "Sentiment scores - Coalition sentences") +
  facet_wrap(~date_bins)

## Boxplot of headline sentiment scores
analysis_train %>% 
  ggplot(aes(publication, heading_sentiment)) +
  geom_boxplot() +
  labs(x = "Publication outlet", y = "Headline sentiment scores")

##########################################################################
##       Classification using full test of political sentences          ##
##########################################################################

# We use the alp_composite, coalition_composite and pol_composite columns of 
# analysis_df, which are the text of all ALP, Coalition and ALP+Coalition
# sentences in each document collapsed into a single string.
# Now create dfms for the ALP, Coalition and all political sentences
# Note - quanteda will replace the NAs with empty strings where there are no
# ALP or Coalition sentences in a particular document, so we first need to 
# generate lists of NAs for alp and Coalition to subset those dfms
alp_composite_NAs <- analysis_df %>% filter(is.na(alp_composite)) %>% pull(doc_id)
coalition_composite_NAs <- analysis_df %>% filter(is.na(coalition_composite)) %>% pull(doc_id)


# we now create the dfms
pol_groups <- c("alp", "coalition", "pol")
pol_groups_list <- list(analysis_df$alp_composite, analysis_df$coalition_composite, analysis_df$pol_composite)

for(i in 1:length(pol_groups)){
  assign(paste(pol_groups[i], "_dfm", sep = ""), corpus(pol_groups_list[[i]], 
                                                        docnames = analysis_df$doc_id, 
                                                        docvars = data.frame(publication = analysis_df$publication)) %>% 
  tokens(., remove_punct = TRUE) %>% 
  tokens_select(., pattern = stopwords("en"), selection = "remove", padding = TRUE) %>% 
  tokens_select(., pattern = pub_names, selection = "remove", padding = TRUE) %>% dfm())
}

# Now subset each of three dfms into train, probe, validation
pol_dfm_names <- c("alp_dfm", "coalition_dfm", "pol_dfm")
pol_dfm_list <- list(alp_dfm, coalition_dfm, pol_dfm)
sets_doc_ids_list <- list(train_doc_ids, probe_doc_ids, test_doc_ids, train_full_doc_ids)

for(i in 1:length(pol_dfm_list)){
  for(j in 1:length(sets)){
  assign(paste(pol_dfm_names[i], sets[j], sep = "_"), pol_dfm_list[[i]] %>% dfm_subset(., docnames(pol_dfm_list[[i]]) %in% sets_doc_ids_list[[j]]))
}}

## we also need to remove the docs for which quanteda replaced NAs with empty strings,
## as we cannot expect to have any predictive capacity for these docs

alp_dfm_list <- list(alp_dfm_train, alp_dfm_probe, alp_dfm_test, alp_dfm_train_full)
for(i in 1:length(alp_dfm_list)){
  alp_dfm_list[[i]] <- alp_dfm_list[[i]] %>% dfm_subset(., !docnames(alp_dfm_list[[i]]) %in% alp_composite_NAs)}

coalition_dfm_list <- list(coalition_dfm_train, coalition_dfm_probe, coalition_dfm_test, coalition_dfm_train_full)
for(i in 1:length(coalition_dfm_list)){
  coalition_dfm_list[[i]] <- coalition_dfm_list[[i]] %>% dfm_subset(., !docnames(coalition_dfm_list[[i]]) %in% coalition_composite_NAs)
}

## Using only features in train set, then padding probe set plus removing features not in train
train_set_dfm_list <- list(alp_dfm_train, coalition_dfm_train, pol_dfm_train)
for(i in 1:length(train_set_dfm_list)){
  train_set_dfm_list[[i]] <- train_set_dfm_list[[i]] %>% dfm_trim(1)
}

probe_set_dfm_list <- list(alp_dfm_probe, coalition_dfm_probe, pol_dfm_probe)
for(i in 1:length(probe_set_dfm_list)){
  probe_set_dfm_list[[i]] <- dfm_match(probe_set_dfm_list[[i]], featnames(train_set_dfm_list[[i]]))
}

## we first run the quanteda_textplots on train and probe
## The lists and groups do not need to be rerun, I have pasted them here as a reminder 
## of what the for loop is looping over 
  # train_set_dfm_list <- list(alp_dfm_train, coalition_dfm_train, pol_dfm_train)
  # probe_set_dfm_list <- list(alp_dfm_probe, coalition_dfm_probe, pol_dfm_probe)
  # pol_groups <- c("alp", "coalition", "pol")

## SVM - Output model, predictions and accuracy for ALP, Coalition and Political Sentences DFM
set.seed(25, sample.kind = "Rounding")
for(i in 1:length(train_set_dfm_list)){
  assign(paste("svm_model_comp", pol_groups[i], sep = "_"), textmodel_svm(train_set_dfm_list[[i]], docvars(train_set_dfm_list[[i]], "publication")))
  assign(paste("test_predictions_svm_comp", pol_groups[i], sep = "_"), predict(eval(as.name(paste("svm_model_comp", pol_groups[i], sep = "_"))), 
                                                                      newdata = probe_set_dfm_list[[i]]))
  assign(paste("accuracy_svm_comp", pol_groups[i], sep = "_"), 
         confusionMatrix(as.factor(docvars(probe_set_dfm_list[[i]], "publication")),
        as.factor(eval(as.name(paste("test_predictions_svm_comp", pol_groups[i], sep = "_")))))$overall["Accuracy"])
}

## NB - Output model, predictions and accuracy for ALP, Coalition and Political Sentences DFM
set.seed(25, sample.kind = "Rounding")
for(i in 1:length(train_set_dfm_list)){
  assign(paste("nb_model_comp", pol_groups[i], sep = "_"), textmodel_nb(train_set_dfm_list[[i]], docvars(train_set_dfm_list[[i]], "publication")))
  assign(paste("test_predictions_nb_comp", pol_groups[i], sep = "_"), predict(eval(as.name(paste("nb_model_comp", pol_groups[i], sep = "_"))), 
                                                                      newdata = probe_set_dfm_list[[i]]))
  assign(paste("accuracy_nb_comp", pol_groups[i], sep = "_"), 
         confusionMatrix(as.factor(docvars(probe_set_dfm_list[[i]], "publication")),
                         as.factor(eval(as.name(paste("test_predictions_nb_comp", pol_groups[i], sep = "_")))))$overall["Accuracy"])
}

## NB priors model - no clear difference, but these all perfrom somewhat above no information rate
set.seed(25, sample.kind = "Rounding")
for(i in 1:length(train_set_dfm_list)){
  assign(paste("nb_priors_model_comp", pol_groups[i], sep = "_"), textmodel_nb(train_set_dfm_list[[i]], docvars(train_set_dfm_list[[i]], "publication"), 
                                                                          prior = "docfreq"))
  assign(paste("test_predictions_nb_priors_comp", pol_groups[i], sep = "_"), predict(eval(as.name(paste("nb_priors_model_comp", pol_groups[i], sep = "_"))), 
                                                                         newdata = probe_set_dfm_list[[i]]))
  assign(paste("accuracy_nb_priors", pol_groups[i], sep = "_"), 
         confusionMatrix(as.factor(docvars(probe_set_dfm_list[[i]], "publication")),
                         as.factor(eval(as.name(paste("test_predictions_nb_priors_comp", pol_groups[i], sep = "_")))))$overall["Accuracy"])
}

## Performance is best across all three models on combination of ALP and Coalition
## sentences. Before proceeding to final use of the model on test set,
## we consider an ensemble. However, this is impractical as 
## examination of the predictions reveal nb and nb_prior differ only on 
## one prediction,meaning an ensemble would equate to choosing naive bayes over
## svm. This makes no sense as svm performs better. Consequently for our final model,
## we fit svm to the full train set and use it to predict the test set

ensemble <- data.frame(svm = test_predictions_svm_comp_pol,
                       nb = test_predictions_nb_comp_pol,
                       nb_priors = test_predictions_nb_priors_comp_pol)

ensemble

##########################################################################
##                        Final model                                   ##
##########################################################################

## use svm model on train_full and test_set
## Pre-machine learning data processing - Use only features in train set,
## then padding probe set plus removing features not in train
pol_dfm_train_full <- pol_dfm_train_full %>% dfm_trim(1)
pol_dfm_test <- dfm_match(pol_dfm_test, featnames(pol_dfm_train_full))

### Pol sentences Support Vector Machine model implemented     ### 
### on full dataset using quanteda.textmodels                  ###
set.seed(25, sample.kind = "Rounding")
final_svm_model <-  textmodel_svm(pol_dfm_train_full, docvars(pol_dfm_train_full, "publication"))

test_predictions_final_svm <- predict(final_svm_model,
                                   newdata=pol_dfm_test)

## Calculate accuracy metric for this classification effort

final_svm_accuracy <- confusionMatrix(as.factor(docvars(pol_dfm_test,"publication")),
                                          as.factor(test_predictions_final_svm))$overall["Accuracy"]
##########################################################################
##                        Feature keyness                               ##
##########################################################################

### Keyness plots ###
## Produce keyness dfs
abc_keyness <- textstat_keyness(pol_dfm_train_full, 
                                target = pol_dfm_train_full$publication == "ABC News")
guardian_keyness <- textstat_keyness(pol_dfm_train_full, 
                                     target = pol_dfm_train_full$publication == "Guardian Australia")
australian_keyness <- textstat_keyness(pol_dfm_train_full, 
                                       target = pol_dfm_train_full$publication == "The Australian")
theage_keyness <- textstat_keyness(pol_dfm_train_full, 
                                   target = pol_dfm_train_full$publication == "The Age")
heraldsun_keyness <- textstat_keyness(pol_dfm_train_full, 
                                      target = pol_dfm_train_full$publication == "Herald Sun")

## produce keyness plots
p1 <- textplot_keyness(abc_keyness, show_legend = FALSE, labelsize = 3) + ggtitle("ABC News") +
  theme(plot.title=element_text(size=12))
p2 <- textplot_keyness(guardian_keyness, show_legend = FALSE, labelsize = 3)+ ggtitle("Guardian Australia") +
  theme(plot.title=element_text(size=12))
p3 <- textplot_keyness(australian_keyness, show_legend = FALSE, labelsize = 3) + ggtitle("The Australian") +
  theme(plot.title=element_text(size=12))
p4 <- textplot_keyness(theage_keyness, show_legend = FALSE, labelsize = 3) + ggtitle("The Age") +
  theme(plot.title=element_text(size=12))
p5 <- textplot_keyness(heraldsun_keyness, show_legend = FALSE, labelsize = 3) + ggtitle("Herald Sun") +
  theme(plot.title=element_text(size=12))

## Print the plots - we use individual printing as grid.arrange compresses the display excessively 
p1
p2
p3
p4
p5

##########################################################################
##                         Script ends                                  ##
##########################################################################
