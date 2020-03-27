```{r}
df %>%
  #filter(burnSev > 1) %>%
  filter(clusterID %in% clusters_with_more_than8_pixels) %>%
  #filter(WID > 0) %>%
  drop_na(land_cover_name) %>%
  group_by(clusterID,year) %>%
  summarise(pct_tree = sum(landcover_code %in% 41:43) / length(landcover_code),
            n_pixels = length(unique(pixelID))) %>%
  group_by(clusterID) %>%
  summarise(pct_change_tree = pct_tree[year==2016] - pct_tree[year==1992],
            n_pixels = head(n_pixels,1)) %>%
  ggplot(aes(pct_change_tree)) +
  geom_histogram() +
  adams_theme +
  xlab(label = "% change tree cover (2016-1992)")

#A_mean = mean(values[value_type=="A"])
```




Analyzing differences in recovery between patches that received plantings from those that did not. From this we see that the treatment does not take up 
```{r}
patch_data %>%
  ggplot(aes(factsTreatmentLogical,pct_change_tree)) +
  geom_boxplot()
```