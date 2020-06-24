##Importar data de las fotos estímulo
psa001_cfd_faces1 # base de datos de las fotos

stiminfo <- psa001_cfd_faces1 %>%
  mutate(ethnicity = recode(Race, "A" = "asian", "B" = "black", "L" = "latinx", "W" = "white"),
         gender = recode(Gender, "M" = "male", "F" = "female")  ##Desglosar raza y género de las fotografías
  )

##Graficar número de fotografías por raza y género
stiminfo %>%         
  group_by(ethnicity, gender) %>%
  summarise(
    n = n(),
    mean_age = round(mean(Age), 2),
    sd_age = round(sd(Age), 2)
  ) %>%
  knitr::kable("html") %>%
  kable_styling("striped")  

##Calcular alfa x c/u de los traits
data_alpha <- data %>%
  select(user_id, lab, stim_id, rating, trait) %>%
  spread(stim_id, rating, sep = "_") %>%
  group_by(trait, lab) %>%
  nest() %>%
  
  mutate(alpha = map(data, function(d) {
    if (dim(d)[1] > 2) {
      # calculate cronbach's alpha
      subdata <- d %>%
        as_tibble() %>%
        select(-user_id) %>%
        t()
      
      capture.output(suppressWarnings(a <- psych::alpha(subdata)))
      a$total["std.alpha"] %>% pluck(1) %>% round(3)
    } else {
      NA
    }
  })) %>%
  select(-data) %>%
  unnest(alpha) %>%
  ungroup()

