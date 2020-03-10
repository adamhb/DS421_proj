
test <- "NLCD2002"

as.numeric(gsub("NLCD", "", test))





treatments <- as.character(paste0('TREAT_F',0:5))
df <- 
  gather(treatments, key = "TRT_year", value = "treatment") %>%
  gather(landcover, key = "LC_year", value = "landcover_code", convert = TRUE) %>%  
  
  
  
  
  
  #treatment df
  
  treat_df <- df %>%
  gather(treatments, key = "TRT_year", value = "treatment") %>%
  
  mutate(.v = TRT_year, .funs = ) %>%
  str()

```
