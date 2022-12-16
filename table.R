load("/Volumes/GoogleDrive/My Drive/GLM_FinalProject/Data_clean/data_cleaned_v2.RData")
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
summary(df_final)

df_final <- df_final[,!names(df_final) %in% c("country_names","abbre","v000","v024","v005","v007")]
df_final$anemia <- as.factor(df_final$anemia)
data <- df_final

levels(df_final$edu) <- c("No", "1", "2", "3+")
levels(df_final$marriage) <- c("No", "M", "S")
levels(df_final$job) <- c("No", "Yes")
levels(df_final$wealth) <- c("1","2","3","4","5")
levels(df_final$toilet) <- c("Unimp", "Imp")
levels(df_final$water) <- c("Unimp", "Imp")
levels(df_final$housesize) <- c("1-2", "3-5", "6+")
levels(df_final$facility_dist) <- c("No Prob", "Prob")
data <- df_final

my_plots <- lapply(names(data), function(var_x){
  p <-
    ggplot(data) +
    aes_string(var_x)

  if(is.numeric(data[[var_x]])) {
    p <- p + geom_density() + theme_minimal()

  } else {
    d <- summary(data[[var_x]])
    # grid.table(d, theme=tt)
    p <- p + geom_bar() + theme_minimal()
    # p <- p + grid.table(d, theme=tt)
  }

})

a <- plot_grid(plotlist = my_plots)
ggsave(a, filename = "Summ.jpeg",width = 7.65, height = 12.5)

for(i in 1:dim(data)[2]) { 
  nam <- paste("p", i, sep = "")
  if(is.numeric(data[,i])) {
    assign(nam, ggplot(data) +
             aes_string(names(data)[i]) + geom_density() + theme_minimal())
  } else {
    assign(nam, tableGrob(t(summary(data[,i])), vp = ""))
  }
}

grid.arrange(p1,p2,p3, ncol = 3)

grid.arrange(p2, p1, main=textGrob("Total Data and Image", gp=gpar(cex=3)), ncol = 2,
             widths=unit.c(grobWidth(p2), unit(1,"npc") - grobWidth(p2)))

grid.arrange(p2, p1, main="Total Data and Image", ncol = 2, widths=c(1,2))
