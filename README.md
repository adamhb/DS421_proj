# Decadal-scale post-fire conifer regeneration in the Klamath Mountains

This research builds off of prior studies (Tepley et al. 2017, Shive et al. 2018, and Young et al. 2019) which seek to predict natural conifer regeneration probabilities as functions of fire characteristics, site conditions, seed availability, climate, and short-term weather in the years following fire. What makes this approach different is the focus on longer term regeneration outcomes rather than looking at the density of seedlings and saplings in the post-fire plant community. We explicitly incorporate post-fire management variables (i.e. spatially explicit data on tree planting events) from the USFS FACTS database. The response variable of interest is (currently: the proportional increase in forest area over a 30 year time frame after severe wildfire in the Klamath eco-region; soon to be: the relative canopy dominance of conifers versus non-conifer vegetation 30 years after wildfire). The research questions are 1) What fire attributes, site characteristics, and climate variables influence conifer forest recovery in severley burned patches? 2) Under what circumstances does tree planting influence the forest recovery outcomes?

See "analysis_1987_fires.md" for the complete analysis


References

Shive, K. L., Preisler, H. K., Welch, K. R., Safford, H. D., Butz, R. J., O’Hara, K. L., & Stephens, S. L. (2018). From the stand scale to the landscape scale: predicting the spatial patterns of forest regeneration after disturbance. Ecological Applications, 28(6), 1626–1639. https://doi.org/10.1002/eap.1756

Tepley, A. J., Thompson, J. R., Epstein, H. E., & Anderson-Teixeira, K. J. (2017). Vulnerability to forest loss through altered postfire recovery dynamics in a warming climate in the Klamath Mountains. Global Change Biology, 23(10), 4117–4132. https://doi.org/10.1111/gcb.13704

Young, D. J. N., Werner, C. M., Welch, K. R., Young, T. P., Safford, H. D., & Latimer, A. M. (2019). Post-fire forest regeneration shows limited climate tracking and potential for drought-induced type conversion. Ecology, 100(2), 1–13. https://doi.org/10.1002/ecy.2571



Todo

-bring in latest dataset from GEE: the additional climate variables, new facts data, landfire and/or lemma, and new way of calculating the propagule pressure (from # pixels in radius that are severley burned), upated clusters more regular size and shape

-model this as logistic, pixel-based, check for spatial auto-correlation, have clusters as random effects to deal with random effects, create plot that focuses on seedling planting

-create map of the study area

-write 1-pager of the key findings and send to Andy at USFS
