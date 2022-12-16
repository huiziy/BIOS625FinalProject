library(naniar)
library(ggplot2)
load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned_v2.RData")
# df_final <- df_final[,!names(df_final) %in% c("country_names","abbre","v000","v024","v005","v007")]
df_final <- df_final[!is.na(df_final$anemia),]
plot <- gg_miss_var(df_final, show_pct = T)
# df_final$anemia <- as.factor(df_final$anemia)




### MAR
plot <- gg_miss_fct(x = df_final, fct = anemia) + labs(title = "NA in Risk Factors and Anemia Status")
ggsave(plot, width = 6, height = 6 , file = "~/Dropbox/Apps/Overleaf/GLM Final Project Presentation/Plots/Missingnesspattern.png")
colSums(is.na(df_final))

### Trend in distribution of Anemia level by year#####
df_final$v000 <- as.factor(substr(df_final$v000, 1, 2))

anemia_trend <- df_final %>%
  group_by(country_names, v007) %>%
  dplyr::summarise(Count = n()
                   , Ratio = mean(anemia, na.rm = TRUE)) %>%
  group_by(country_names) %>% dplyr::summarise(pct = mean(Ratio, na.rm = TRUE))

colnames(anemia_trend)<-c("country","prevalence")

# ggplot(anemia_trend, aes(x = reorder(v000,-pct), y = pct)) +
  geom_bar(stat = "identity") + theme_bw()


######map plot######
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "gray50") +
  theme_classic()
world_map %>%
  left_join(anemia_trend, by = c("region" = "country")) -> prevalence_world_map

mp<-ggplot(prevalence_world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = prevalence), colour = "white") +
  scale_x_continuous(
    breaks = seq(-180, 210, 45),
    labels = function(x) {
      paste0(x, "°")
    }
  ) +
  scale_y_continuous(
    breaks = seq(-60, 100, 30),
    labels = function(x) {
      paste0(x, "°")
    }
  ) +
  scale_fill_gradient(low = "green", high = "red") +
  labs(
    title = "Anemia Prevalence Around the World",
    y = "Latitude",
    x = "Longitude",
    subtitle = "The plot picks 32 countries in East Africa"
  ) +
  # geom_text(aes(x = long, y = lat + 3, label = c(region="Mozambique")), col = "orange")+
  theme_bw()
ggsave(mp, file="test.svg", width=25, height=15)

