##Graficar base de datos
bgcolor <- "white"    # formato de las gráficas
textcolor <- "black"
PSA_theme <- theme(
  plot.background = element_rect(fill = bgcolor, color = NA),
  panel.background = element_rect(fill = NA, color = "grey"),
  legend.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  text = element_text(color = textcolor, size=15),
  axis.text = element_text(color = textcolor, size=10),
  strip.text.y = element_text(angle = 0, hjust = 0)
)

##Graficar edad de los p por sexo
data %>%         
  group_by(user_id, sex,lab, age,country) %>%
  summarise() %>%
  ungroup() %>%
  group_by(lab) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  ggplot(aes(as.numeric(age), fill = sex)) +
  geom_histogram(binwidth = 5, color = "grey") +
  geom_text(aes(x=0, y=5, label = paste0("n=",n)), color = "black") +
  labs(title="", y="", x="Edad de los participantes") +
  facet_grid(lab~., scales="free_y") +
  PSA_theme

##Graficar número de p por trait
data %>%  
  group_by(trait, lab) %>%
  summarise(n = n_distinct(user_id)) %>%
  ggplot(aes(trait, n)) +
  geom_col(aes(fill = trait), show.legend = F) +
  facet_grid(lab~., scale = "free") +
  labs(title="", x="", y="Participantes por rasgo") +
  theme( axis.text.x = element_text(angle = -45, hjust = 0) ) + 
  PSA_theme

##Formato gráfica
bgcolor <- "white"
textcolor <- "black"
PSA_theme <- theme(
  plot.background = element_rect(fill = bgcolor, color = NA),
  panel.background = element_rect(fill = NA, color = "grey"),
  legend.background = element_rect(fill = NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  text = element_text(color = textcolor, size=15),
  axis.text = element_text(color = textcolor, size=10),
  strip.text.y = element_text(angle = 0, hjust = 0)
)

##Graficar ratings totales por trait
ggplot(data, aes(rating, fill = trait)) + 
  geom_histogram(binwidth = 1, color = "grey", show.legend = F) +
  facet_grid(lab ~trait, space = "free") +
  scale_x_continuous(breaks = 1:9) +
  PSA_theme  
