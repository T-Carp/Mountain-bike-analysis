

### Essential Packages
suppressMessages(library(tidyverse))
suppressMessages(library(rvest))
suppressMessages(library(xml2))
suppressMessages(library(jsonlite))
suppressMessages(library(stringr)) 
suppressMessages(library(lubridate))
suppressMessages(library(RMySQL))
suppressMessages(library(DBI))
suppressMessages(library(cronR))


### Scrape Pink Bike, Create Data Frame
get_date <- function(session, link) {
              more_details_pg <- jump_to(session,link)  
              row_with_date <- html_node(more_details_pg,'.buysell-details-column+ .buysell-details-column')
              date_text <- str_extract(row_with_date %>% html_text(),
                         "([a-zA-Z]{3,}-[0-9]{1,2}-[0-9]{4}) ([0-9]{1,2}:[0-9]{2}:[0-9]{2})")
              return(date_text)
}


get_sold_status <- function(session, link) {
                      more_details_pg <- jump_to(session,link)  
                      row_with_sold <- html_node(more_details_pg,'.buysell-details-column span')
                      sold_status <- html_text(row_with_sold, trim=TRUE) 
                      return(sold_status)
}




### Web Scrape
df_list <- list()

url <- paste("https://www.pinkbike.com/buysell/list/?location=194-*-*&page=1&category=2")

###Start web session. Imagine this is the script actively doing stuff on the page
session <- html_session(url)

### Find total results
total_count <- html_node(session, 'li:nth-child(13) a') %>% 
               html_text() %>% 
               as.numeric()
  
### Create vector of page results.  
pages <- c(1:total_count)

for (l in 1:length(pages))
{
  new_url <- paste("https://www.pinkbike.com/buysell/list/?location=194-*-*&page=",pages[l],"&category=2", sep = "")
  session <- html_session(new_url)
  
  title <- html_nodes(session, '.bsitem div a') %>% 
           html_text(trim = TRUE) 
  
  seller <- html_nodes(session, '.bsitem b+ a') %>% 
            html_text(trim = TRUE) 
   
  url <- html_nodes(session, '.bsitem div a') %>% 
         html_attr("href")
   
  dates <- get_more_links <- html_nodes(session, '.bsitem td:nth-child(1) a') %>%
           html_attr("href") %>% .[matches("buysell", vars=.)] %>% 
           unique() %>%
           sapply(get_date,session = session) 
  
  sold <- get_more_links <- html_nodes(session, '.bsitem td:nth-child(1) a') %>%
          html_attr("href") %>% .[matches("buysell", vars=.)] %>% unique() %>%
          sapply(get_sold_status,session = session) 
  
  location <- html_nodes(session, '.bsitem td tr:nth-child(1) td') %>% 
              html_text(trim=TRUE) 
        
  
  price <- html_nodes(session, 'tr~ tr+ tr td > b') %>% 
           html_text(trim=TRUE)  
  
  df_list[[l]] <- tibble(title, seller, location, price, url,dates,sold)
}

df <- as_tibble(bind_rows(df_list)) 




### Data Prep to master dataframe

### Remove duplicate posts
  df <- distinct(df)

### Clean price column
  df$price <- str_remove(df$price,"USD")
  df$price <- as.integer(str_remove(df$price,"\\$"))

### Remove outliers
  df <- df %>% 
        filter(price > 1000 & price < 10000) 

### Create date columns and Sold/Available Flag
  df <- df %>% 
        mutate(year = str_extract(df$title, "\\d{4}"),
              title = str_remove(df$title,"\\d{4}"),
              sold = ifelse(str_detect(df$sold, "Sold"),"Sold","Available")) %>% 
        rename(sold_status = sold)

### Prep for string
  df$title <- str_replace_all(df$title, "Santa Cruz", "Santa_Cruz")
  df$title <- str_replace_all(df$title, "SANTA CRUZ", "Santa_Cruz")
  df$title <- str_replace_all(df$title, "Rocky Mountain", "Rocky_Mountain")

  df <- df %>% 
        mutate(title  = trimws(title))

### Create model and brand columns
  df <- df %>% 
        mutate(brand = trimws(word(df$title,1,1,)),
          model = trimws(word(df$title,2,2,)),
          model = tolower(model),
          year = as.numeric(year)) %>% 
        separate(location,c("city", "state", "country"), sep = ",", remove = FALSE)

### Convert date to datetime
  df <- df %>% mutate(dates = mdy_hms(dates)) %>% rename(updated_at = dates) 
  df_current <- df





### Evaluate what scraped data does not allready exist in database 

creds <- read.csv("/Users/t-carpen93/Data_Projects/Credentials/creds.csv")
user <- as.character(creds[,2:2])
password <- as.character(creds[,3:3])

cn <- dbConnect(RMySQL::MySQL(), 
                username = user, 
                password = password, 
                host = "bike-spotter.ch4frojqjmlp.us-west-2.rds.amazonaws.com", 
                port = 3306,
                dbname = "bikes"
                )

query <- "SELECT url FROM bikes_master"
db_data <- dbGetQuery(cn, query)


`%notin%` <- Negate(`%in%`)

df <- df %>% 
  filter(url %notin% db_data$url)



### Create dataframe of sold bikes
sold_df <- df %>% 
           filter(sold_status == "Sold")



### Clean data and create child-dataframes for price analysis.  
####Parent dataframe = df.  Child dataframes = c("df_price","df_model_names") and are derived from df

### Get list of most popular brands in dataset.  Brand has to appear > 30 times.
names <- df_current %>% 
         select(brand) %>% 
         group_by(brand) %>% 
         summarize(count = n()) %>% 
         filter(count >= 20, brand != c("Large", "Custom", "Stumpjumper", "Carbon"))

### Build df_price which can be used to measure % depreciation in asking price over time. Overwrite table in database
df_price <- df_current %>% 
            select(brand,price, year) %>% 
            filter(year >= 2013, 
                   brand %in% names$brand) %>% 
            group_by(brand,year) %>% 
            summarise(count = n(),avg_price = round(mean(price)))%>% 
            arrange(desc(year), .by_group = TRUE) %>% 
            mutate(pct_change = round((avg_price/lag(avg_price) - 1) * 100),
                  pct_depreciation = round(avg_price/max(avg_price) * 100))


df_price$pct_change[is.na(df_price$pct_change)] <- 100


top_brands <- unique(names$brand)
df_price <- df_price %>% filter(brand %in% top_brands)
df_price <- as_tibble(df_price)

### Create df_model_price which is the data source for the shiny app. 

### Create df for reactive filters
df_model_names <- df_current %>% 
                  select(brand, model) %>% 
                  filter(brand %in% top_brands, model != "carbon") %>% 
                  group_by(brand,model) %>% 
                  summarise(count = n())

### Model has to appear at least 6 times to appear in data.
df_model_names <- df_model_names %>% filter(count >= 6)


### Create df for brand+model price analysis
df_model_price <- df_current %>% 
                  select(title, brand,model,price, year, location, url, sold_status) %>%
                  filter(year >= 2013, brand %in% names$brand, 
                         model %in% df_model_names$model, 
                         brand != "Custom", 
                         brand != "Looking") %>% 
                  arrange(brand)




### Append/ overwrite data to database tables.
###Master table to adds not cleaned. Always Growing
dbWriteTable(cn, name = "bikes_master", value = df, append = TRUE)

### Master list of bikes that are sold.  Always growing
dbWriteTable(cn, name = "sold_bikes", value = sold_df, append = TRUE)

### Used for shiny app, table is always over-written to show bikes currently on pinkbike
dbWriteTable(cn, name = "model_price_view", value = df_model_price, overwrite = TRUE)

### Datamart for price analysis, table is always over-written for bikes currently on pinkbike
dbWriteTable(cn, name = "brand_price_view", value = df_price, overwrite = TRUE)


dbDisconnect(cn)


