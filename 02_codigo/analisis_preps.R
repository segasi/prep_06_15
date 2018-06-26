### Nota: para poder usar el código a continuación, antes debes haber corrido el código en el script cargar_limpiar_datos.R. Si aún no lo has hecho, puedes "descomentar" la siguiente línea de código y hacerlo de forma remota

source(file = "02_codigo/cargar_limpiar_datos.R")

### Cálculo y gráfica de paquetes sin acta en casillas con una, tres o cuatro elecciones procesadas en 2015 ----

# Cálculo
p_2015 %>% 
  mutate(observaciones_bis = ifelse(str_detect(observaciones, "Sin acta"), "Sin acta", as.character(observaciones))) %>% 
  group_by(num_act, observaciones_bis) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(num_act) %>% 
  mutate(por = (n/sum(n)*100)) %>% 
  ungroup() 


# Gráfica
p_2015 %>% 
  mutate(observaciones_bis = ifelse(str_detect(observaciones, "Sin acta"), "Sin acta", as.character(observaciones))) %>% 
  group_by(num_act, observaciones_bis) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(num_act) %>% 
  mutate(por = round((n/sum(n)*100), 1)) %>% 
  ungroup() %>% 
  filter(observaciones_bis == "Sin acta") %>% 
  mutate(num_act = as.character(num_act), 
         num_act = recode(num_act, "1" = "Un acta", "3" = "Tres actas", "4" = "Cuatro actas"),
         num_act = fct_relevel(num_act, "Un acta", "Tres actas", "Cuatro actas")) %>% 
  ggplot(aes(num_act, por)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste(por, "%", sep = "")), size = 7, fontface = "bold", vjust = 1.5, col = "white") +
  labs(title = "% DE PAQUETES ELECTORALES SIN ACTA PREP DE ACUERDO CON EL NÚMERO DE ACTAS\nPROCESADAS EN CASILLA | PREP 2015", 
       x = "",
       y = "",
       caption = "Sebastián Garrido de Sierra / @segasi / Javier Aparicio / @javieraparicio / Fuente: INE") +
  tema +
  theme(axis.text.y = element_blank())


ggsave(filename = paste("2015_paquetes_sin_acta_prep_por_num_actas_procesadas.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)


