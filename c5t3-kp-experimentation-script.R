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
