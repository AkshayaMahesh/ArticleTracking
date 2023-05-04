# Adding the necessary libraries
library(XML)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(DBI)
library(knitr)
library(RMySQL)
detach("package:RMySQL")
library(sqldf)
library(ggplot2)

# Parsing the xml file and connecting to a database
xmlfile <- "pubmed-tfm-xml/pubmed22n0001-tf.xml"
xmlDOM = xmlParse(xmlfile)
r <- xmlRoot(xmlDOM)
article_num <- xmlSize(r)
article_num
# Inspecting what are all there in the xml data for the first record just to get an idea of what is there
r[[1]][[1]]
r[[1]]

# ERD diagram
knitr::include_graphics("erd.png")

 #[](https://lucid.app/lucidchart/57be54df-285b-4274-828c-51c020b3fda3/edit?invitationId=inv_cc0c8b79-1ef1-4e6b-801d-cb62358574f8#)

#' Above is a normalized schema including Authors, Articles and Journals. A mapping table is included in between Authors and Articles to achieve 3rd normal form. 

#' *Authors table - auid is the primary key for the table. Last name, First name and Initials are the attributes of the Authors table

#' * Articles table - ArticleId is the primary key for the table. journal_Id and ArticleTitle are the attributes of the table. 
#' ISSN is the foreign key to the Journals table that follows

#' * authorArticleMap table - auid and ArticleId are the primary keys for this table. auid references the Authors table and the ArticleId references the Articles table

#' * Journals table - This table has journal_id as the primary key. Following are the attributes: Title, Volume, Issue, Publish Month, ISSN and Publish Year

#' The schema is replicated in the SQLite database in the steps that follow. 

# Initializing Dataframes

journal_df <- data.frame(
  journal_id = integer(),
  JournalTitle = character(),
  ISSN = integer(),
  JournalVolume = integer(),
  JournalIssue = integer(),
  PublishYear = integer(),
  PublishMonth = integer(),
  stringsAsFactors = F)


for (i in 1:article_num){
  row <- nrow(journal_df) + 1
  if(xmlSize(r[[i]][[1]][[1]][[1]])==1){
    ISSN_number <- xmlValue(r[[i]][[1]][[1]][[1]])} else {
      ISSN_number <- 0  # keeping ISSN as 0 if it's not there
    }
  
  journal_data <- r[[i]][[1]][[1]]
  
  journal_volume <- (xpathApply(journal_data,"./JournalIssue/Volume", xmlValue))
  journal_issue <-  (xpathApply(journal_data,"./JournalIssue/Issue", xmlValue))
  journaldate_year <- as.numeric(xpathApply(journal_data,"./JournalIssue/PubDate/Year", xmlValue))
  journalDateMonth <- (xpathApply(journal_data,"./JournalIssue/PubDate/Month", xmlValue))
  
  journal_title <- xmlValue(r[[i]][[1]][[1]][[3]])
  
  
  if(length(journaldate_year)==0){
    journaldate_year = 0
  }
  
  if(length(journalDateMonth)==0){
    journalDateMonth = 0
  }
  
  if(length(journal_volume)==0){
    journal_volume = 0
  }
  
  if(length(journal_issue)==0){
    journal_issue = 0
  }
  
  journal_df[row,1]<- row
  journal_df[i, 2] <- journal_title
  journal_df[i, 3] <-ISSN_number
  journal_df[i, 4] <- journal_volume 
  journal_df[i, 5] <- journal_issue
  journal_df[i, 6] <- journaldate_year
  journal_df[i, 7] <- (journalDateMonth)
  
  
}

# Changing months to chars
journal_df$PublishMonth[journal_df$PublishMonth == "04"]<- "Apr"
journal_df$PublishMonth[journal_df$PublishMonth == "12"]<- "Dec"
journal_df$PublishMonth[journal_df$PublishMonth == "11"]<- "Nov"
journal_df$PublishMonth[journal_df$PublishMonth == "09"]<- "Sep"
journal_df$PublishMonth[journal_df$PublishMonth == "10"]<- "Oct"

# using month.abb to convert them into corresponding integers
journal_df$PublishMonth <-match((journal_df$PublishMonth),month.abb)
# Assumption : I have taken articles that do not have month or year to have a value 
# of 0. I have later converted the month character to a corresponding integer
#(Similarly for journal volume and issue)

unique(journal_df$PublishMonth)

unique(journal_df$PublishYear)
# keeping the journals having No Year as well as it contains other useful information 
#such as journal title etc

head(journal_df,10)

# Article Df

article_df <- data.frame(ArticleId = integer(),
                         ArticleTitle = character(),
                         journal_id = integer(),
                         stringsAsFactors = F)

for(i in 1:article_num){
  row <- nrow(article_df) + 1
  article_df[i,1] <- as.numeric(xmlGetAttr(node = r[[i]],name="PMID"))
  article_df[i,2] <- xmlValue(r[[i]][[1]][[3]])
  article_df[i,3]<- row
}

# Displaying records
head(article_df,10)

# Author Dataframe

author_df <- data.frame(auid =integer(),
                        LastName = character(),
                        FirstName = character(),
                        Initials = character(),
                        
                        stringsAsFactors = F)


for (i in 1:article_num){
  au_list_size <- xmlSize(r[[i]][[1]][[4]])
  if(au_list_size != 0){ # when authors are present
    for(j in 1:au_list_size ){
      
      row <- nrow(author_df) + 1
      k<-r[[i]][[1]][[4]][[j]]
      
      LastName <- xpathApply(k,"./LastName",xmlValue)
      
      FirstName <- xpathApply(k,"./ForeName",xmlValue)
      
      Initials <- xpathApply(k,"./Initials",xmlValue)
      
      if(length(LastName)==0){ # when no lastname it's taken as NA
        LastName = "NA"
      }
      if(length(FirstName)==0){ # when no first name it's taken as NA
        FirstName = "NA"
      }
      if(length(Initials)==0){ # when no Initials it is NA
        Initials = "NA"
      }
      author_df[row,1]<-row
      author_df[row,2]<- LastName
      author_df[row,3]<-FirstName
      author_df[row,4]<-Initials
    }
  }
  
}

#Keeping distinct authors
author_df<-author_df %>% distinct(LastName, FirstName, .keep_all = TRUE)

head(author_df)

#Author Article Map

# creating a df that helps us to map the articles and all their authors

authorArticleMap_df <- data.frame(ArticleId = integer(),
                                  auid = integer(),
                                  stringsAsFactors = F)

z<-1
for(i in 1 : article_num){
  au_list_size <- xmlSize(r[[i]][[1]][[4]])
  if(au_list_size != 0){
    for(j in 1:au_list_size ){
      authorArticleMap_df[z,1] <- as.numeric(xmlGetAttr(node = r[[i]],name="PMID"))
      row <- nrow(author_df) + 1
      k<-r[[i]][[1]][[4]][[j]]
      a <- as.character(xpathApply(k,"./LastName",xmlValue))
      b <- as.character(xpathApply(k,"./ForeName",xmlValue))
      if(length(a)==0){
        a = "NA"}
      if(length(b)==0){
        b = "NA"}
      #row <- nrow(authorArticleMap_df) + 1
      sql<-paste0("select auid from author_df where LastName=",'"',a ,'"',
                  " and"," FirstName=","'",b,"'")
      auid<-sqldf(sql)
      authorArticleMap_df[z,2] <- auid
      z<-z+1
    } 
  }
}

# keeping distinct records
authorArticleMap_df<-authorArticleMap_df %>% distinct(ArticleId, auid, .keep_all = TRUE)

head(authorArticleMap_df)

# Connecting to sqlite database
dbfile = "pubmed.db"
con <- dbConnect(RSQLite::SQLite(), dbfile)

src_dbi(con)

# Creating the Authors table and loading data to it
dbSendQuery(con, "DROP TABLE if exists Authors")

dbSendQuery(conn = con, "CREATE TABLE Authors
(auid INT PRIMARY KEY,
LastName TEXT,
FirstName TEXT,
Initials TEXT)")

dbWriteTable(conn = con, 
             name = "Authors",
             value = author_df,append=TRUE)

#Viewing few results of the tables
au <- DBI::dbGetQuery(conn = con,
                       statement = "
                SELECT * FROM Authors
                LIMIT 5
                ")
au

# Creating the Journals table and loading data to it
dbSendQuery(con, "DROP TABLE if exists Journals")

dbSendQuery(conn = con, "CREATE TABLE Journals(
journal_id INT PRIMARY KEY,
JournalTitle TEXT,
ISSN TEXT,
JournalVolume TEXT,
JournalIssue TEXT,
PublishYear INT,
PublishMonth TEXT)
            ")

dbWriteTable(conn = con, 
             name = "Journals",
             value = journal_df, append=TRUE)

j <- DBI::dbGetQuery(conn = con,
                      statement = "
                SELECT * FROM Journals
                LIMIT 5
                ")
j


# Creating the Articles table and loading data to it
dbSendQuery(con, "DROP TABLE if exists Articles")

dbSendQuery(conn = con, "CREATE TABLE Articles
(ArticleId INT PRIMARY KEY,
ArticleTitle TEXT,
journal_id INT,
FOREIGN KEY(journal_id) REFERENCES Journals(journal_id))")

dbWriteTable(conn = con, 
             name = "Articles",
             value = article_df,append=TRUE)

ar <- DBI::dbGetQuery(conn = con,
                      statement = "
                SELECT * FROM Articles
                LIMIT 5
                ")
ar

# Creating the AuthorArticleMap table and loading data to it
dbSendQuery(con, "DROP TABLE if exists AuthorArticleMap")

dbSendQuery(conn = con, "CREATE TABLE AuthorArticleMap(
ArticleId INT NOT NULL ,
auid INT NOT NULL ,
PRIMARY KEY(ArticleId,auid) ,
FOREIGN KEY(ArticleId) REFERENCES Articles(ArticleId),
FOREIGN KEY(auid) REFERENCES Authors(auid)
)")

dbWriteTable(conn = con, 
             name = "AuthorArticleMap",
             value = authorArticleMap_df, append = TRUE)

aamap <- DBI::dbGetQuery(conn = con,
                      statement = "
                SELECT * FROM AuthorArticleMap
                LIMIT 5
                ")
aamap

# Listing the tables
dbListTables(con)

# disconnecting db
dbDisconnect(conn = con)
