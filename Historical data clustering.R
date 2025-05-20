#Historical data clustering

library(data.table) # data handling
library(ggplot2) # visualisations
library(gridExtra) # visualisations
library(grid) # visualisations
library(cluster) # PAM - K-medoids

set.seed(54321)

#data_example <- data.table(x = c(rnorm(10, 3.5, 0.1), rnorm(10, 2, 0.1),
#                                 rnorm(10, 4.5, 0.1), c(5, 1.9, 3.95)),
#                           y = c(rnorm(10, 3.5, 0.1), rnorm(10, 2, 0.1),
#                                 rnorm(10, 4.5, 0.1), c(1.65, 2.9, 4.2)))

data_example <- "Erin_SWCC_Retrospective_TEST_SET.xlsx"

gg1 <- ggplot(data_example, aes(x, y)) +
  geom_point(alpha = 0.75, size = 8) +
  theme_bw()

kmed_res <- pam(data_example, 3)$clustering

data_example[, class := as.factor(kmed_res)]

gg2 <- ggplot(data_example, aes(x, y, color = class, shape = class)) +
  geom_point(alpha = 0.75, size = 8) +
  theme_bw()

define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}

grid.newpage()
# Create layout : nrow = 2, ncol = 2
pushViewport(viewport(layout = grid.layout(1, 2)))
# Arrange the plots
print(gg1, vp = define_region(1, 1))
print(gg2, vp = define_region(1, 2))

library(ggdendro)
library(dendextend)

data_m <- iris[,-5]

hie_single <- hclust(dist(data_m), method = "single")
dend <- as.dendrogram(hie_single)
dend <- dend %>% set("branches_k_color", k = 3) %>%
  set("branches_lwd", 1.2) %>% 
  set("labels", rep(c("set", "ver", "vir"), each = 50)) %>%
  set("labels_colors", rep(c("red", "green", "blue"), each = 50)) %>%
  set("labels_cex", 0.6)
ggd1 <- as.ggdend(dend)
ggplot(ggd1)

hie_ave <- hclust(dist(data_m), method = "average")
dend <- as.dendrogram(hie_ave)
dend <- dend %>% set("branches_k_color", k = 3) %>% 
  set("branches_lwd", 1.2) %>% 
  set("labels", rep(c("set", "ver", "vir"), each = 50)) %>%
  set("labels_colors", rep(c("red", "green", "blue"), each = 50)) %>%
  set("labels_cex", 0.6)
ggd1 <- as.ggdend(dend)
ggplot(ggd1)
