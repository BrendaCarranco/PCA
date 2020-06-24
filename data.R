##Cargar base de datos cruda
mex002 <- read_excel("mex002.xlsx")

##Conteo de participantes (n=129)
participants <- mex002 %>%
  group_by(user_id, sex, age, trait, lab, country) %>%   
  summarise(trials = n(),
            stim_n = n_distinct(stim_id)) %>%
  ungroup() 

##Conteo de px con al menos 120 trials (n=205)
participants %>%         
  mutate(n120 = ifelse(stim_n == 120, "rated all 120", "rated < 120")) %>%
  count(lab, n120) %>%
  spread(n120, n) %>%
  knitr::kable("html") %>%
  kable_styling("striped")

##Conteo de los px que completaron los 240 trials (n=158) 
participants %>%       
  mutate(n240 = case_when(
    trials == 240 ~ "rated 240", 
    trials > 240 ~ "rated > 240",
    trials < 120 ~ "rated < 120",
    trials < 240 ~ "rated 120-239"
  )) %>%
  count(lab, n240) %>%
  spread(n240, n, fill = 0) %>%
  knitr::kable("html") %>%
  kable_styling("striped")  

##Verificamos la varianza, eliminamos los px con 75% de respuestas iguales en el bloque 1
identical_rating_threshold <- 0.75 * 120 

inv_participants <- mex002 %>%
  filter(block == 1) %>%
  count(user_id, lab, trait, rating) %>%
  group_by(user_id, lab, trait) %>%
  filter(n == max(n)) %>% # encontramos el rate más repetido
  ungroup() %>%
  filter(n >= identical_rating_threshold) # seleccionamos los px que dieron el mismo rate >= 75% 

inv <- inv_participants %>%
  count(lab, trait) %>%
  spread(lab, n, fill = 0) %>%
  mutate(TOTAL = rowSums(select_if(., is.numeric), na.rm = T))

inv_total <-  group_by(inv) %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>%
  mutate(trait = "TOTAL")

bind_rows(inv,inv_total) %>%
  knitr::kable("html") %>%
  kable_styling("striped")


##Conteo final

partfinal <- data %>%       ##Nueva base para el conteo total de los p finales
  group_by(user_id, sex, age, trait, lab, country) %>%   #Conteo de los participantes con trials 
  summarise(trials = n(),
            stim_n = n_distinct(stim_id)) %>%
  ungroup()

##No corre en R cloud, creo es por las versiones del paquete dplyr, 
# aquí no está actualizado, pero cargo la base que ya pude correr en la
# app desde la app de la computadora 'partfinal y data'.








