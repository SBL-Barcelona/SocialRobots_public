library(tidyverse)

file <- "C:/Social Brain Lab/VALAWAI/Validar_dilemas_morales/UCD_Completo.csv" 

data <- read.csv(file = file, sep = ",")

data <- data[-(1:2), ]

# Delete participants that did not pay attention
data_filtrada <- data %>%
  filter(if_all(26:38, ~ is.na(.) | . == "") | AttentionCheck2 == 2)

#Select dilemmas 
dilemas <- data_filtrada %>% select(39:64, 66:91) 

# Percentage
conteo_porcentajes <- dilemas %>%
  pivot_longer(everything(), names_to = "Dilema", values_to = "Porcentaje") %>%
  group_by(Dilema) %>%
  summarise(
    PorcentajeU = sum(Porcentaje == 1, na.rm = TRUE) / n() * 100,
    PorcentajeC = sum(Porcentaje == 2, na.rm = TRUE) / n() * 100,
    PorcentajeD = sum(Porcentaje == 3, na.rm = TRUE) / n() * 100
  ) %>% 
  mutate(Dilema_num = as.numeric(str_extract(Dilema, "\\d+"))) %>%
  arrange(Dilema_num) %>%
  select(-Dilema_num)

porcentaje_filtrado60 <- conteo_porcentajes %>%
  filter( PorcentajeU >= 60 | PorcentajeC >= 60 |PorcentajeD >= 60)

Conteo_U <- porcentaje_filtrado60 %>%
  filter(PorcentajeU >= 60) %>%
  arrange(PorcentajeU)

Conteo_C <- porcentaje_filtrado60 %>%
  filter(PorcentajeC >= 60)

Conteo_D <- porcentaje_filtrado60 %>%
  filter(PorcentajeD >= 60)


Conteo_C_50 <- conteo_porcentajes %>%
  filter(PorcentajeC >= 50)%>%
  arrange(PorcentajeC)
