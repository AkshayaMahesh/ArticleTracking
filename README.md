# Article Tracking
## Overview 
This project involves extracting data from an XML document and storing it relationally in a SQLite database. The SQLite database represents a "transactional" database. After that, the data is extracted from the transactional database and an "analytical" database is created using a star schema in MySQL. Finally, facts are queried from the MySQL analytical database.

### Technologies Used
R, SQLite, MySQL

### Libraries and Packages
dplyr,tidyr
xml,xpath
sqlite3
mysql-connector-python

![ERD ](https://github.com/AkshayaMahesh/ArticleTracking/blob/main/erd.png)
