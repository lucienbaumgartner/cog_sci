df <- read.csv('../input/TC+Cognition+Exp+1a_Preregistered_July+22,+2020_09.55.csv', stringsAsFactors = F)
df <- select(df, LocationLongitude, LocationLatitude)
df <- mutate(df, experiment = '1a')
dfx <- read.csv('../input/TC+Embeddings+Valence+Control_July+23,+2020_15.39.csv')
dfx <- select(dfx, LocationLongitude, LocationLatitude)
dfx <- mutate(dfx, experiment = 'Embeddings')
df <- rbind(dfx, df)
df <- mutate(df, 
             LocationLongitude = as.numeric(LocationLongitude),
             LocationLatitude = as.numeric(LocationLatitude)
             )
df <- filter(df, ((LocationLongitude >= -130 & LocationLongitude <= -60) | (LocationLatitude >= 20 & LocationLatitude <= 55)))
usa <- map_data("usa")
ggplot(data = usa) +
  geom_polygon(aes(x=long, y=lat)) +
  geom_point(data = df, aes(x=LocationL
                            ongitude, y=LocationLatitude, colour = experiment)) +
  geom_density2d(data = df, aes(x=LocationLongitude, y=LocationLatitude, colour = experiment)) +
  scale_x_continuous(limits = c(-130, -60)) +
  scale_y_continuous(limits = c(20,55)) +
  coord_fixed(1.3) +
  facet_wrap(~experiment, nrow = 2) +
  labs(title = 'Geolocation of respondents')
