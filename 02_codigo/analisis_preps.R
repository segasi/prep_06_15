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




### Cálculo y gráfcia de paquetes sin acta en 2009, 2012 y 2015  ----
p_2009 %>% 
  group_by(observaciones) %>% 
  tally() %>%  
  mutate(por = round((n/sum(n))*100, 2))

p_2012 %>% 
  group_by(observaciones) %>% 
  tally() %>%  
  mutate(por = round((n/sum(n))*100, 2))

p_2015 %>% 
  mutate(observaciones_bis = ifelse(str_detect(observaciones, "Sin acta"), "Sin acta", as.character(observaciones))) %>% 
  group_by(observaciones_bis) %>% 
  tally() %>%  
  mutate(por = round((n/sum(n))*100, 2))


df_sin_acta <-  data_frame(año = c(2009, 2012, 2015),
                           por = c(1.35, 1.14, 5.29))

df_sin_acta %>% 
  ggplot(aes(año, por)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste(por, "%", sep = "")), size = 7, fontface = "bold", vjust = 1.5, col = "white") +
  scale_x_continuous(breaks = c(2009, 2012, 2015)) +
  labs(title = "% DE PAQUETES ELECTORALES SIN ACTA PREP, 2009-2015", 
       x = "",
       y = "",
       caption = "Sebastián Garrido de Sierra / @segasi / Javier Aparicio / @javieraparicio / Fuente: INE") +
  tema +
  theme(axis.text.y = element_blank())


ggsave(filename = paste("2009_vs_2012_vs_2015_paquetes_sin_acta_prep.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)


### Cálculo y gráfica de entidades con entre tres y seis elecciones en 2018 ----

df_edos <- data_frame(num_elecciones = c("Tres", "Cuatro", "Cinco", "Seis"), frec = c(2, 7, 15, 8))

df_edos %>% 
  mutate(num_elecciones = fct_relevel(num_elecciones, "Tres", "Cuatro", "Cinco", "Seis")) %>% 
  ggplot(aes(num_elecciones, frec)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = frec), size = 7, fontface = "bold", vjust = 1.5, col = "white") +
  labs(title = "NÚM. DE ESTADOS CON __ TIPOS DE ELECCIONES EN 2018", 
       x = "\nNúm. de elecciones\n",
       y = "",
       caption = "Sebastián Garrido de Sierra / @segasi / Javier Aparicio / @javieraparicio / Fuente: INE") +
  tema +
  theme(axis.text.y = element_blank())

ggsave(filename = paste("2018_frec_edos_num_eleccioens.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)



### Gráfica de núm. acumulado de actas capturadas 2006 ----
p_2006 %>% 
  arrange(hora_captura) %>% 
  # filter(!is.na(ubicacion_casilla)) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(hora_captura, acumuladas, group = 1), col = "steelblue", size = 1.5) +
  # scale_color_manual(values = c("salmon", "steelblue")) +
  scale_x_datetime(breaks=date_breaks("1 hour"), labels = date_format("%H:%M"))+ 
  scale_y_continuous(limits = c(0, 150000), breaks = seq(0, 150000, 25000), labels = comma) +
  labs(title = "NÚM. DE ACTAS CAPTURADAS | PREP DE 2006",
       subtitle = "Datos de la elección presidencial",
       x = "\nHora de captura",
       y = "Núm. acumulado de actas\n",
       caption = "Sebastián Garrido de Sierra / @segasi / Javier Aparicio / @javieraparicio / Fuente: INE",
       color = "Tipo de casilla") + 
  tema +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = c(.1, .9),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(filename = paste("2006_actas_capturadas.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)



### Gráfica de núm. acumulado de actas capturadas 2006, por tipo de casilla ----
p_2006 %>% 
  arrange(hora_captura) %>% 
  filter(!is.na(casilla)) %>% 
  group_by(casilla) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id),
         c_urb_rur = ifelse(casilla == 1, "Urbana", "Rural")) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(hora_captura, acumuladas, group = c_urb_rur, col = c_urb_rur), size = 1.5) +
  scale_color_manual(values = c("salmon", "steelblue")) +
  scale_x_datetime(breaks=date_breaks("1 hour"), labels = date_format("%H:%M"))+ 
  scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, 10000), labels = comma) +
  labs(title = "NÚM. DE ACTAS CAPTURADAS, POR TIPO | PREP DE 2006",
       subtitle = "Datos de la elección presidencial",
       x = "\nHora de captura",
       y = "Núm. acumulado de actas\n",
       caption = "Sebastián Garrido de Sierra / @segasi / Fuente: INE",
       color = "Tipo de casilla") + 
  tema +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = c(.1, .9),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(filename = paste("2006_actas_capturadas_por_tipo.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)

### Gráfica de núm. acumulado de actas capturadas 2009 ----
p_2009 %>% 
  arrange(hora_captura) %>% 
  filter(!is.na(ubicacion_casilla)) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(hora_captura, acumuladas, group = 1), col = "steelblue", size = 1.5) +
  # scale_color_manual(values = c("salmon", "steelblue")) +
  scale_x_datetime(breaks=date_breaks("1 hour"), labels = date_format("%H:%M"))+ 
  scale_y_continuous(limits = c(0, 150000), breaks = seq(0, 150000, 25000), labels = comma) +
  labs(title = "NÚM. DE ACTAS CAPTURADAS | PREP DE 2009",
       subtitle = "Datos de la elección de diputados federales",
       x = "\nHora de captura",
       y = "Núm. acumulado de actas\n",
       caption = "Sebastián Garrido de Sierra / @segasi / Fuente: INE",
       color = "Tipo de casilla") + 
  tema +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = c(.1, .9),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(filename = paste("2009_actas_capturadas.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)



### Gráfica de núm. acumulado de actas capturadas 2006, por tipo de casilla ----
p_2009 %>% 
  arrange(hora_captura) %>% 
  filter(!is.na(ubicacion_casilla)) %>% 
  group_by(ubicacion_casilla) %>% 
  mutate(id = 1,
         acumuladas = cumsum(id),
         c_urb_rur = ifelse(ubicacion_casilla == 1, "Urbana", "Rural")) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(hora_captura, acumuladas, group = c_urb_rur, col = c_urb_rur), size = 1.5) +
  scale_color_manual(values = c("salmon", "steelblue")) +
  scale_x_datetime(breaks=date_breaks("1 hour"), labels = date_format("%H:%M"))+ 
  scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, 10000), labels = comma) +
  labs(title = "NÚM. DE ACTAS CAPTURADAS, POR TIPO | PREP DE 2009",
       subtitle = "Datos de la elección presidencial",
       x = "\nHora de captura",
       y = "Núm. acumulado de actas\n",
       caption = "Sebastián Garrido de Sierra / @segasi / Fuente: INE",
       color = "Tipo de casilla") + 
  tema +
  theme(plot.title = element_text(face = "bold"), 
        legend.position = c(.1, .9),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(filename = paste("2009_actas_capturadas_por_tipo.png", sep = ""), path = "03_graficas/actas_capturadas/", width = 15, height = 10, dpi = 100)

