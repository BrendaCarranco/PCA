##Valores agregados 
##Calcular promedio de cada trait por rosto evaluado
data_agg <- data %>%
  group_by(lab, trait, stim_id) %>%
  summarise(rating = mean(rating)) %>%
  ungroup() %>%
  spread(trait, rating)

## Grafica cada trait x ratings totales
data_agg %>%    
  gather("trait", "rating", aggressive:weird) %>%
  ggplot(aes(rating, fill = trait)) +
  geom_density(show.legend = F) +
  facet_grid(lab~trait) +
  PSA_theme

##PCA

##################################################

pca1<- prcomp(na.omit(data_agg), center = TRUE, scale. = TRUE) 
summary(pca1)
pca1$x
corvar <- pca1$rotation %*% diag(pca1$sdev)
plot(pca1)
pca1$rotation
biplot(x = pca1, scale = 0, cex = 0.6,)
pca1$sdev






# function to calculate PCA

psa_pca <- function(d) {
  traits <- select(d, -stim_id) %>% 
    select_if(colSums(!is.na(.)) > 0) # omits missing traits
  
  # principal components analysis (SPSS-style, following Oosterhof & Todorov)
  ev <- eigen(cor(traits))$values
  nfactors <- sum(ev > 1)
  
  pca <- principal(
    traits, 
    nfactors=nfactors, 
    rotate="none"
  )
  
  stats <- pca$Vaccounted %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(type = "stat")
  
  unclass(pca$loadings) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(type = "trait") %>%
    bind_rows(stats) %>%
    gather("pc", "loading", 2:(ncol(.)-1))
}

