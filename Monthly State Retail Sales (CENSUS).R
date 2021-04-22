# https://www.census.gov/data/developers/data-sets/economic-indicators.html

setwd('~/projects/adhoc/CensusEconomicIndicator/')
library(dplyr)
library(ggplot2)

MARTScat <- read.csv('./data/MARTS_categories.csv')[,-1]
View(MARTScat)

###census api query for Economic Indicators
json_file <- paste0("https://api.census.gov/data/timeseries/eits/marts?"
        ###variable list
        ,"get=geo_level_code,data_type_code,category_code"
        ,",time_slot_id,error_data,seasonally_adj,cell_value"
       
        ###data filters
        ,"&time=from+1998-01+to+2021"
        # ,"&category_code=451"
        # ,"&category_code=722"
        # ,"&category_code=4451"
        ,"&data_type_code=SM"
      )


### read data from Census Bureau into local session
json_data <- rjson::fromJSON(file=json_file)

mydata <- do.call(rbind, json_data) %>% data.frame
names(mydata) <- mydata[1,]
mydata <- mydata[-1,] %>% data.frame(stringsAsFactors =T)
mydata %>% str


### join category code and convert string variables
mydata <- mydata %>% 
        transform(
            cell_value = cell_value %>% as.numeric
            , time = paste0(time, "-01") %>%  as.Date()
          ) %>% 
        transform( 
              year = time %>% format("%Y") %>% as.factor
              , year2 = time %>% format("%Y") %>% as.numeric
              , month = time %>% format("%m") %>% as.numeric
            ) %>% 
      left_join(MARTScat)


### to generate plot location for custom text on ggplot
text_loc <- mydata %>% subset(seasonally_adj=='no' & month==1 & year2 %in% c(2002, 2010, 2020)) %>% 
                select(category, category_code, year, month, cell_value) 
        
    
text_loc <- text_loc %>% 
                    left_join( data.frame(year=c(2002, 2010, 2020) %>% as.factor ,period= c('Dot-com', 'Subprime Mortgage', 'COVID-19') )) %>% 
                    left_join( mydata %>% group_by(category) %>% summarize(inc = max(cell_value)*0.10)) %>% 
                    mutate( 
                            month = month %>% as.numeric
                            , cell_value = cell_value+inc*0 )


### subset of years to view
yearset = c(2000:2002, 2008:2010, 2019, 2020)

### manual enter category code to appear in the plot
catset = c('44X72' )

### generate monthly sales of the category, grouped by years
mydata %>% subset(category_code %in% catset ) %>% 
    subset(seasonally_adj=='no') %>% 
    subset( year2 %in% yearset ) %>% 
    ggplot(aes(month, cell_value, group=year, color=year)) +
        geom_line(size=1.5) +
        geom_point(size=3) +
  
        geom_text(aes(month, cell_value, label=period) 
                  , text_loc %>% subset(category_code %in% catset ) 
                  , hjust ='right', nudge_x = -0.1 , size=5) +
  
        facet_wrap(.~ category, scales = 'free', ncol=1) +
  
        ggtitle("US Census: Monthly Sales") +
        xlab("Month") + ylab("Sales (Millions of Dollars)") +
        theme(
          
          axis.text.y=element_text(size=18)
          , axis.text.x = element_text(size=18)
          
          , axis.title=element_text(size=18)
          , plot.title=element_text(size=22)
          , plot.tag=element_text(size=18)
          , text=element_text(size=18)
          
          ) +
        xlim(-1, 12) +
        scale_y_continuous(labels = scales::comma)
    


