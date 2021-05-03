########### get unique values contained in all vars

unique(iphone$iphone)

iphone %>%
  map(unique)



### duplicates #####
# i need to figure out which function returns obs.that have the same value for every observation in the df.

iphone %>% 
  duplicated.data.frame() %>% 
  sum()

get_dupes(iphone)


##### CHANGE TO FACTOR AND RENAME LEVELS ####
iphone_test <- iphone_df

iphone_test$iphonesentiment <- as_factor(iphone_test$iphonesentiment)

class(iphone_test$iphonesentiment)

 
iphone_test$iphonesentiment <- fct_recode(iphone_test$iphonesentiment,
             unclear = "0",
             neg = '1',
             vneg = '2',
             neutral = '3',
             pos = '4',
             vpos = '5')

head(iphone_test$iphonesentiment)



