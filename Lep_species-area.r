#occurr10.r plotting species-area relationships for native and alien Lepidoptera


#pdf("C:\Users\mally\Desktop\Lep_species-area_output.pdf")

# load packages
library(tidyverse)
library(ggrepel)
library(grid)
library(cowplot)

# read in the data
# read numbers of species by family for native assemblages
native <- read_tsv("NatLep_2021-01-05.txt")
colnames(native)  <- c("superfamily", "family", "Australia", "Europe", "Galapagos", "Hawaii", "Japan", "South Korea",  "New Zealand",  "North America", "Ogasawara",  "Nansei", "world")
# native.SF <- native %>%
#              select(-superfamily) %>% ## delete columns not for use
#              group_by(family) %>%  ## grouping by family
#              summarise(across(where(is.double), sum))  ## sum up all in each family in each region

# read numbers of species by family for non-native assemblages
alien <- read_tsv("EstLep_2021-01-05.txt")
colnames(alien)  <- c("superfamily", "family", "Australia", "Europe", "Galapagos", "Hawaii", "Japan", "South Korea",  "New Zealand",  "North America", "Ogasawara",  "Nansei", "total")
# alien.SF <- alien %>%
#             select(-superfamily) %>% ## delete columns not for use
#             group_by(family) %>%  ## grouping by family
#             summarise(across(where(is.double), sum)) ## sum up all in each family in each region

# Organize your data for your plots
# this creates a data frame of the areas
area <- c(7692024, 10180000, 7880, 28311, 377975, 100210, 268020, 19819000, 106, 1207) # this creates a character vector
names(area) <- names(alien[,3:12])  # this names the elements of the character vector with the names of alien
area <- as.data.frame(area) %>% rownames_to_column("name") # this converts a named character vector to a data frame of 2 columns

alien1 <- alien[,3:12] %>% 
  summarise(across(where(is.double), sum)) %>% 
  pivot_longer(everything()) %>% 
  full_join(area) %>% 
  mutate(value_log = log10(value),    # calculate the log10 in your data rather than your plot
         area_log = log10(area))


#ERROR HERE: 'cols' must select at least one column


native1 <- native[,3:12] %>% 
  summarise(across(where(is.double), sum)) %>% 
  pivot_longer(everything()) %>% 
  full_join(area) %>% 
  mutate(value_log = log10(value),    # calculate the log10 in your data rather than your plot
         area_log = log10(area))


#ERROR HERE: 'cols' must select at least one column







# # fit linear log-log species-area linear models for each assemblage
# mod_alien1 <- lm(alien1$value_log ~ alien1$area_log, data=alien1)
# mod_alien2 <- lm(alien2$value_log ~ alien2$area_log, data=alien2)
# mod_alien3 <- lm(alien2$value_log ~ alien2$area_log, data=alien3)
# mod_native1 <- lm(native1$value_log ~ native1$area_log, data=native1)
# mod_native2 <- lm(native2$value_log ~ native2$area_log, data=native2)
# mod_native3 <- lm(native3$value_log ~ native3$area_log, data=native1)

family_function <- function(df, family){
  
  fam_area <-  df %>%
    filter(Family == family) %>% 
    select(-suborder, -superfamily, -Family) %>% 
    pivot_longer(everything()) %>% 
    full_join(area) %>% 
    mutate(value_log = log10(value + 1),    # calculate the log10 in your data rather than your plot
           area_log = log10(area + 1))   
  
  return(fam_area)     
  
}

alien2 <- family_function(alien, "Curculionidae")
alien3 <- family_function(alien, "Staphylinidae")
native2 <- family_function(native, "Curculionidae")
native3 <- family_function(native, "Staphylinidae")

# Some reasons why your code didn't work:
# ggplot expects a data frame; you gave it a matrix/list
# you have to put the plus sign at the end of a row, not the beginning
# you were using different super family names than your data

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

all_panels <- plot_grid(n1, a1, n2, a2, n3, a3, ncol = 2)

save_plot("/Users/andrewliebhold/sandy/SESYNC/insect establishment/Analysis/occurr10_output.pdf", all_panels, base_height = 12, base_width = 11)
