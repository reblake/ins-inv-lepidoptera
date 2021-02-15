########################################
# Creating figures for the Lepidoptera paper


########################################

# loading libraries
library(readxl)
library(tidyverse)
library(ggrepel)
require(scales)
library(grid)
library(gridExtra)
library(cowplot)
library(vegan)
library(ggcorrplot)

##########################################
# setting working directory

# setwd("C:\\Users\\mally\\Desktop")

##############
# loading data

# establishment data
Est <- read_tsv("EstLep_2021-01-25.txt")
colnames(Est) <- c("superfamily", "family", "Australia", "Europe", "Galapagos", "Hawaii", "Japan", "South Korea",  "New Zealand",  "North America", "Ogasawara",  "Nansei", "sum_regions")

# native data
Nat <- read_tsv("NatLep_2021-01-25.txt")
colnames(Nat) <- c("superfamily", "family", "Australia", "Europe", "Galapagos", "Hawaii", "Japan", "South Korea",  "New Zealand",  "North America", "Ogasawara",  "Nansei", "world")




######## FIGURE 1 ########


# Organize your data for your plots

# this creates a data frame of the areas
area <- c(7692024, 10180000, 7880, 28311, 377975, 100210, 268020, 19819000, 106, 1207) # this creates a character vector
names(area) <- names(Est[,3:12])  # this names the elements of the character vector with the names of alien
area <- as.data.frame(area) %>% rownames_to_column("name") # this converts a named character vector to a data frame of 2 columns

native1 <- Nat[,2:12] %>% 
  summarise(across(where(is.double), sum)) %>% 
  pivot_longer(everything()) %>% 
  full_join(area) %>% 
  mutate(value_log = log10(value),    # calculate the log10 in your data rather than your plot
         area_log = log10(area))

native2 <- Nat %>% filter(family == "Noctuidae") %>%
  select(,3:12) %>%
  pivot_longer(everything()) %>%
  full_join(area, by=c("name")) %>%
  mutate(value_log = log10(value + 1), area_log = log10(area + 1))

native3 <- Nat %>% filter(family == "Crambidae") %>%
  select(,3:12) %>%
  pivot_longer(everything()) %>%
  full_join(area, by=c("name")) %>%
  mutate(value_log = log10(value + 1), area_log = log10(area + 1))

alien1 <- Est[,2:12] %>% 
  summarise(across(where(is.double), sum)) %>% 
  pivot_longer(everything()) %>% 
  full_join(area) %>% 
  mutate(value_log = log10(value),    # calculate the log10 in your data rather than your plot
         area_log = log10(area))

alien2 <- Est %>% filter(family == "Noctuidae") %>%
  select(,3:12) %>%
  pivot_longer(everything()) %>%
  full_join(area, by=c("name")) %>%
  mutate(value_log = log10(value + 1), area_log = log10(area + 1))

alien3 <- Est %>% filter(family == "Crambidae") %>%
  select(,3:12) %>%
  pivot_longer(everything()) %>%
  full_join(area, by=c("name")) %>%
  mutate(value_log = log10(value + 1), area_log = log10(area + 1))


# # fit linear log-log species-area linear models for each assemblage
mod_alien1 <- lm(alien1$value_log ~ alien1$area_log, data=alien1)
mod_alien2 <- lm(alien2$value_log ~ alien2$area_log, data=alien2)
mod_alien3 <- lm(alien3$value_log ~ alien3$area_log, data=alien3)
mod_native1 <- lm(native1$value_log ~ native1$area_log, data=native1)
mod_native2 <- lm(native2$value_log ~ native2$area_log, data=native2)
mod_native3 <- lm(native3$value_log ~ native3$area_log, data=native3)


# Function to create scatterplots  
make_scatterplots <- function(df, title){
  model <- lm(df$value_log ~ df$area_log, data = df)
  
  p <- ggplot(df, aes_string(x = df$area, y = df$value)) +
    geom_point() +
    geom_text_repel(label = df$name) +  # from the package ggrepel; puts the text in good places
    geom_smooth(method = lm, se = FALSE) + 
    theme_cowplot() + 
    xlab(expression(paste("Area (km" ^ "2", ")"))) + ylab("No. Species") +
    scale_x_log10() + 
    scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000)) +
    labs(title = title)
  
  p <- ggdraw(p) + 
    draw_label(label = paste0("R-squared = ", round(summary(model)$r.squared, 3)), 
               x = 0.85, y = 0.2, size = 12, fontface = "italic") +
    draw_label(label = paste0("Slope = ", round(summary(model)$coefficients[[2]], 6)), 
               x = 0.85, y = 0.25, size = 12, fontface = "italic") 
  
  print(paste0("plot_", deparse(substitute(df))))
  
  return(p)
  
}


a1 <- make_scatterplots(alien1, "b)") 
a2 <- make_scatterplots(alien2, "d)")
a3 <- make_scatterplots(alien3, "f)")
n1 <- make_scatterplots(native1, "a)")
n2 <- make_scatterplots(native2, "c)")
n3 <- make_scatterplots(native3, "e)")

Fig1 <- plot_grid(n1, a1, n2, a2, n3, a3, ncol = 2)

save_plot("Fig1_Lep_species-area_2021-02.png", Fig1, base_height = 16, base_width = 12)
save_plot("Fig1_Lep_species-area_2021-02.pdf", Fig1, base_height = 16, base_width = 12)

dev.off()


######## END FIGURE 1 ########





######## FIGURE 2 ######## Numbers of global Lepidoptera species within each of the top 18 families versus numbers of non-native species in these families


Nat_2 <- Nat[,c("superfamily", "family", "world")]

EstvsW <- left_join(Nat_2, Est)


### select families with at least 10 established species worlwide, arranged in descending order of world species diversity
top18 <- EstvsW[EstvsW$"sum_regions" > 9,] %>% arrange(desc(world))


### order top18 families
world_order <- c("Pterophoridae", "Psychidae", "Coleophoridae", "Sphingidae", "Cosmopterygidae", "Gracillariidae", "Depressariidae", "Tineidae", "Oecophoridae", "Gelechiidae", "Lycaenidae", "Pyralidae", "Nymphalidae", "Tortricidae", "Crambidae", "Noctuidae", "Geometridae", "Erebidae")


Est_long <- gather(top18,"region","established",4:13) # changing from wide to long format

Nat_long <- gather(Nat,"region","native",3:12) # changing from wide to long format

### join data from Est_long and Nat_long
df <- Est_long %>% left_join(Nat_long)
df <- df[,c(1, 2, 6, 3, 4, 5, 7)] # reorder columns into a more reasonable way, with "world" and "sum_regions" referring to the family, and "established" and "native" referring to the region


### bar plots of world and non-native family diversities
g1 <- ggplot(top18, aes(x = world, y = factor(family,level = world_order))) +
  geom_col() +
  theme(axis.ticks.y = element_blank(), panel.background = element_rect(fill = "white")) +
  scale_x_reverse() +
  scale_y_discrete(position="right") +
  ylab("family")

g2 <- ggplot(top18, aes(x = sum_regions, y = factor(family, level = world_order))) +
  geom_col() +
  labs(x="non-natives", y=element_blank()) +
  theme(axis.text.y = element_blank(), panel.background = element_rect(fill = "white"), 
        axis.ticks.y = element_blank())


Fig2 <- grid.arrange(g1, g2, ncol=2, widths = c(7/12, 5/12))

save_plot("Fig2_top18_world-vs-nonnative_2021-02.png", Fig2, base_width = 22, base_height = 15, units = "cm", dpi = 400)
save_plot("Fig2_top18_world-vs-nonnative_2021-02.pdf", Fig2, base_width = 22, base_height = 15, units = "cm", dpi = 400)

dev.off()


######## END FIGURE 2 ########







######## FIGURE 3 - scatterplots "non-native species versus native species" per family for the 10 different regions ########


# adding a column of predicted establishments based o n native numbers
df_1 <- df %>% group_by(region) %>% mutate(pred = native * sum(established)/sum(native))

# adding lower and upper bounds - upper and lower 1% quantiles widened by bonferroni type correction
df_2 <- df_1 %>% 
  group_by(region) %>% 
  mutate(upper = qbinom(p = 1-0.02/2/length(established), 
    size = sum(established), 
    prob = native/sum(native)),
    lower = qbinom(p = 0.02/2/length(established),
    size = sum(established),
    prob = native/sum(native)))

# df_2 is a dataframe with all combinations of family (top 18) and region.

# the following creates a dataframe for plotting the grey bounding region with geom_smooth
# otherwise the lowerbounds can be cut off for the larger regions

for (i in unique(df_2$region)){
  # creating a row for the predicted number of establishments if only 1 native species
  df_3 <- df_2[1,]
  df_3$region = i
  df_3$native = 1
  df_3$pred = sum(df[df$region==i,]$established)/sum(df[df$region==i,]$native)
  df_3$established = df_3$pred
  df_3$upper = qbinom(p=1-0.02/2/length(df[df$region==i,]$established),
    size = sum(df[df$region==i,]$established),
    prob = 1/sum(df[df$region==i,]$native))
  df_3$lower = qbinom(p=0.02/2/length(df[df$region==i,]$established),
    size=sum(df[df$region==i,]$established),
    prob=1/sum(df[df$region==i,]$native))
  if(i=="Australia"){ # Australia is the first region
    df_4 <- bind_rows(df_2,df_3)
  }else{
    df_4 <- bind_rows(df_4,df_3)
  }
}


datas = df_4[,c("region","native","pred","upper","lower")]

Fig3 <- ggplot(df_2,aes(x=native)) + 
  geom_point(aes(y=established)) + 
  geom_smooth(data = datas, mapping = aes(ymin = lower, ymax = upper, y = pred), stat = "identity", color = "black", size = 0.5) +
  scale_x_log10() + 
  scale_y_log10(labels = comma, breaks = c(1,10,100)) + 
  facet_wrap(.~region, nrow = 2) + 
  theme_bw() +
  xlab("number of natives in family") + 
  ylab("number of non-natives in family") +
  geom_text_repel(data = subset(df_2, established > upper*1 | established < lower*1), aes(native, established, label = family), size = 2, family = "sans", col = "black")

save_plot("Fig3_nonnative-vs-world_2021-02.png", Fig3, base_width = 22, base_height = 12, units = "cm", dpi = 400)
save_plot("Fig3_nonnative-vs-world_2021-02.pdf", Fig3, base_width = 22, base_height = 12, units = "cm", dpi = 400)

dev.off()



######## END FIGURE 3 ########






######## FIGURE 4 - for all 10 regions pooled ########



df_world <- EstvsW %>% filter(sum_regions > 0) %>% arrange(desc(world))

Est_long2 <- gather(df_world, "region", "established", 4:13) # changing from wide to long format

### adding a column of predicted establishments based on native numbers
df_1_world <- Est_long2 %>% group_by(region) %>% mutate(pred = (world*sum_regions)/world)


### adding lower and upper bounds - upper and lower 1% quantiles widened by bonferroni type correction
 df_2_world <- df_1_world %>%
  group_by(region) %>%
  mutate(upper = qbinom(p = 1-0.02/2/length(sum_regions),
    size = sum(sum_regions),
    prob = world/sum(world)),
    lower = qbinom(p = 0.02/2/length(sum_regions),
    size = sum(sum_regions),
    prob = world/sum(world)))


datas2 = df_2_world[,c("world", "pred", "upper", "lower")]
df_2_world <- df_2_world %>% select(family, region, world, sum_regions, pred, upper, lower) %>% filter(sum_regions > 0, region == "Australia")



### simple scatterplot of non-native species against world species
# ggplot(df_2_world2, aes(world, sum_regions)) + geom_point() + scale_x_log10() + scale_y_log10(limits=c(1, 100)) + theme_bw() + xlab("number of world spp.") + ylab("number of non-native spp.")



Fig4 <- ggplot(df_2_world, aes(x = world)) +
  geom_point(aes(y = sum_regions)) + 
  geom_smooth(data = datas2, mapping = aes(ymin = lower, ymax = upper, y = pred), stat = "identity", color = "black", size = 0.5) +     # to add a regression line, add [method = "lm"] to the arguments in geom_smooth() - doesn't give the result I was looking for though...
  scale_x_log10() +
  scale_y_log10(breaks = c(1,10,100)) +
  theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(linetype = "solid", fill = NA)) +
  xlab("world (spp. in family)") +
  ylab("non-natives (spp. in family)") +
  geom_text_repel(data = subset(df_2_world, sum_regions > upper*1 | sum_regions < lower*1),
    aes(world, sum_regions, label = family), size = 5, family = "sans", col = "black")

save_plot("Fig4_nonnative-vs-world-pooled_2021-02.png", Fig4, base_width = 22, base_height = 15, units = "cm", dpi = 400)
save_plot("Fig4_nonnative-vs-world-pooled_2021-02.pdf", Fig4, base_width = 22, base_height = 15, units = "cm", dpi = 400)

dev.off()


######## END FIGURE 4 ########






######## FIGURE 5 - HEATMAP ########


total <- read_tsv("totalLep_2021-01-25.txt")

df_heat <- df %>% left_join(total, by = "region") 

df_heat1 <- df_heat %>% mutate(heat_native = sqrt(native/native_region), heat_nonnative = sqrt(established/established_region))

df_heat2 <- df_heat1 %>% pivot_longer(11:12, names_to = "heat_subset", values_to = "heat_count")

df_heat2 <- df_heat2[c(1, 2, 6, 3, 7, 11, 12, 8, 9, 10, 5, 4)]

Fig5 <- ggplot(df_heat2, aes(x = region, y = factor(family, level = world_order))) +
  geom_tile(aes(fill = heat_count)) + 
  facet_wrap(~heat_subset) +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(0,0.6)) +              ### defines the colour gradient and range
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

save_plot("Fig5_heatmap_top18_native-vs-nonnative_2021-02.png", Fig5, base_width = 21, base_height = 15, units = "cm", dpi = 600)
save_plot("Fig5_heatmap_top18_native-vs-nonnative_2021-02.pdf", Fig5, base_width = 21, base_height = 15, units = "cm", dpi = 600)

dev.off()


######## END FIGURE 5 ########






######## FIGURE 6 - RDA ordination ########

## join native and non-native assemblages
df_Fig6 <- Nat[,2:13] %>% inner_join(Est[,2:13], by="family")

## select 18 most species rich families in total invasions
df_Fig6 <- df_Fig6[df_Fig6$"sum_regions" > 9,] %>% arrange(desc(world))


## transpose matrix
df_Fig6t <- t(df_Fig6 %>% select(-1))
colnames(df_Fig6t) <- df_Fig6$family

## Hellinger transformation to stabilize the result sqrt(yij/yi+), Legendre (2001) Oecologia
df_Fig6tH <- data.frame(sqrt(df_Fig6t/rowSums(df_Fig6t))) %>% as_tibble()

df_Fig6tH.property <- df_Fig6tH %>% 
  transmute(Region=rownames(df_Fig6t) %>%
  str_replace(., ".x", "") %>%  str_replace(., ".y", ""), 
    Status=c(rep("Nat",11),rep("Est",11)),
    WorldRegion=rep(c("Oceania", "Nearctic", "PI", "Asia", "Asia", "PI", "Oceania", "PI", "Asia", "Europe", "World"),2),  ## To check Wallace world devision
    Lat = rep(c(37.8, 38.9, 21.3, 35.7, 37.5,-0.7, -41.3, 27.1, 26.2,51.0, 0),2))                                         ## To check lattitudial gredients

## RDA with permutation test!
df_Fig6.rda <- rda(df_Fig6tH~Region+Status, data=df_Fig6tH.property)
## assemb.rda <- rda(assembtH~WorldRegion+Status, data=assembtH.property)
## assemb.rda <- rda(assembtH~Lat+Status, data=assembtH.property)
anova(df_Fig6.rda, by="term")
plot(df_Fig6.rda)

scor.rda = scores(df_Fig6.rda, display = c("sites", "species", "cn", "bp"), scaling = 2)
site.rda = data.frame(scor.rda$sites) %>%
  as_tibble() %>%
  mutate(label = df_Fig6tH.property$Region, status = df_Fig6tH.property$Status)

p.sites <-
  ggplot(data = site.rda, mapping = aes(x = RDA1, y = RDA2, colour = status)) +
  geom_point() +
  scale_fill_manual(values = c("blue", "black")) +
  geom_text_repel(data=site.rda, aes(x = RDA1, y = RDA2, label = label, colour = status), size = 4, family = "sans") +
  scale_colour_manual(values = c("red","blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", axis.text = element_text(size = 12))
print(p.sites)

species.rda = data.frame(scor.rda$species) %>% rownames_to_column() %>% as_tibble()


p.species <-
  ggplot(data = species.rda, mapping = aes(x = RDA1, y = RDA2)) +
  geom_point() +
  scale_fill_manual(values = c("blue", "black"))+
  geom_text_repel(data = species.rda, aes(x = RDA1, y = RDA2, label = rowname), size = 4, family = "sans") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none", axis.text = element_text(size = 12))
print(p.species)

(Fig6 <- gridExtra::grid.arrange(p.sites, p.species, nrow = 1))

save_plot("Fig6_RDA-ordination_2021-02.png", Fig6, base_width = 22, base_height = 15, units = "cm", dpi = 600)
save_plot("Fig6_RDA-ordination_2021-02.pdf", Fig6, base_width = 22, base_height = 15, units = "cm", dpi = 600)

dev.off()


######## END FIGURE 6 ########






######## FIGURE 7 - Correlation matrix ########



EstvsNatvsW <- EstvsW %>% inner_join(Nat[,2:13], by=c("family", "world"))

EstvsNatvsW_2 <- EstvsNatvsW %>% select(c(-1, -2))
EstvsNatvsW_2 <- EstvsNatvsW_2[c(1:6, 11, 8:10, 7, 12:17, 22, 19:21, 18)]

corrmatrix <- cor(EstvsNatvsW_2)

Fig7 <- ggcorrplot(corrmatrix, hc.order = FALSE, type = "lower", 
  outline.col = "white", ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")) +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(0, 1)) +              ### defines the colour gradient and range
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))

save_plot("Fig7_corrmatrix_2021-02.png", Fig7, base_width = 16, base_height = 15, units = "cm", dpi = 600)
save_plot("Fig7_corrmatrix_2021-02.pdf", Fig7, base_width = 16, base_height = 15, units = "cm", dpi = 600)

dev.off()



######## END FIGURE 7 ########





######## CODE FOR EXPORTING TABLES ########

### save "df" to a file
# write.table(df_heat2, file="df_heatmap.txt", sep="\t", row.names = TRUE, col.names = NA)