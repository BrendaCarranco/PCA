##Base de datos del estudio original Oosterhof y Todorov (2008)

Karolinska_14trait_judgmentswithlabels

traits <- mex002 %>%
  filter(trait != "old", !is.na(trait)) %>%
  arrange(trait) %>%
  pull(trait) %>%
  unique()

ot_data <- Karolinska_14trait_judgmentswithlabels %>%
  mutate(region = "(Oosterhof & Todorov, 2008)") %>%
  rename(stim_id = `Todorov Label`,
         emostable = `emotionally stable`) %>%
  select(region, stim_id, traits)
