#Loading required libraries

library(RMySQL)
library(DBI)
library(sqldf)

#Setting up db

db_user <- 'admin'
db_password <- 'Dbms2022'
db_name <- 'practicum2db'
db_host <- 'practicum2db.c5harneqrwsu.us-east-1.rds.amazonaws.com'
db_port <- 3306


mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)


detach("package:RMySQL")
library(sqldf)

#Loading the SQLite database to connect with the db created in Part1
con <- dbConnect(RSQLite::SQLite(),"./pubmed.db")

#Preparation of data for authorFact table

#Using the tables in Part 2 q3 to create a authorFacts table as requested. 
#This table has data in the level of auid (i.e) auid is the primary key for the table. 
# It has co authors and number of articles published by author

sqlCmd = "WITH article_count
AS (
  WITH final_temp_authors AS (
    WITH temp_co_author AS (
      SELECT a.auid
      ,b.auid AS coauthor
      ,COUNT(DISTINCT a.auid) AS cnt_author
      FROM AuthorArticleMap a
      INNER JOIN AuthorArticleMap b ON a.ArticleId = b.ArticleId
      GROUP BY a.auid
      ,b.auid
    )
    SELECT auid
    ,(SUM(cnt_author)-1 )AS number_of_co_authors
    FROM temp_co_author
    GROUP BY auid
  )
  SELECT a.auid
  ,(TRIM(a.FirstName)||' '||TRIM(a.LastName)) AS authorName
  ,ft.number_of_co_authors
  FROM final_temp_authors ft
  JOIN Authors a ON a.auid = ft.auid
)
SELECT ac.auid
,ac.authorName
,ac.number_of_co_authors
,COUNT(al.ArticleId) as number_of_articles
FROM article_count ac
JOIN AuthorArticleMap al ON ac.auid = al.auid
GROUP BY ac.auid
,ac.authorName
,ac.number_of_co_authors
"

authorFacts_df = dbGetQuery(con, sqlCmd)

# displaying few records

authorFacts_df<- filter(authorFacts_df, authorFacts_df$authorName != "NA NA")
head(authorFacts_df)

# Creating the AuthorFacts table 
dbSendQuery(mydb, "DROP TABLE if exists AuthorFacts")

dbSendQuery(mydb,"CREATE TABLE AuthorFacts
(auid INT PRIMARY KEY,
authorName TEXT,
number_of_co_authors INT,
number_of_articles INT
)")

# Loading data
dbWriteTable(mydb, 'AuthorFacts', authorFacts_df, row.names=F,append=T, overwrite=F)

af <- DBI::dbGetQuery(conn = mydb,
                      statement = "
                SELECT * FROM AuthorFacts
                LIMIT 5
                ")
af

#It must include the journal name, number of articles per year, per quarter, and per month.
sqlCmd2 = "SELECT j.JournalTitle, 
                 COUNT(*) as TotalPublications,
                 ROUND(((COUNT(*)*1.0)/(SELECT COUNT(DISTINCT PublishYear) as CalcField 
                       FROM Journals)),2) as perYear,
                 ROUND(((COUNT(*)*1.0)/(SELECT COUNT(DISTINCT PublishMonth) as CalcField2 
                       FROM Journals)),2) as perMonth
          FROM Authors au
          JOIN authorArticleMap map on map.auid = au.auid
          JOIN Articles ar on map.ArticleId = ar.ArticleId
          JOIN Journals j on j.journal_id = ar.journal_id
          GROUP BY j.JournalTitle"

jfacts_df = dbGetQuery(con, sqlCmd2)

head(jfacts_df)

# Creating the AuthorFacts table 
dbSendQuery(mydb, "DROP TABLE if exists JournalFacts")

dbSendQuery(mydb,"CREATE TABLE JournalFacts
(JournalTitle VARCHAR(255) PRIMARY KEY,
TotalPublications INT,
perYear INT,
perMonth INT
)")

# Loading data
dbWriteTable(mydb, 'JournalFacts', jfacts_df, row.names=F,append=T, overwrite=F)

jf <- DBI::dbGetQuery(conn = mydb,
                      statement = "
                SELECT * FROM JournalFacts
               LIMIT 5
                ")
jf

