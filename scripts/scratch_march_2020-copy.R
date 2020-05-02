







Export raster of landfire codes
```{r}

lf_lookup_code <- landfire_codes %>% pull(EVT_PHYS) %>% unique() %>% as.tibble() %>% rowid_to_column(var = "lf_code")

lf_code <- landfire_codes %>% pull(EVT_PHYS) %>% unique() %>% as.tibble() %>% rowid_to_column(var = "lf_code") %>% rename(EVT_PHYS = value) %>%
  left_join(df) %>% dplyr::select(x,y,lf_code)

summary(lf_code$lf_code)

library(raster)

SUID_polygons <- st_read(dsn = "~/cloud/gdrive/fire_project/local_data/facts/SUIDSin1987StudyArea.shp")


lf_coderaster <- rasterFromXYZ(xyz = lf_code, res = 30, crs = crs(SUID_polygons))
plot(lf_coderaster)

#writeRaster(lf_coderaster,filename = "~/cloud/gdrive/fire_project/local_data/Landfire_EVC/lf.tiff")



```












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
