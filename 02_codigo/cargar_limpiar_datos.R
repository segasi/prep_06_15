### Paquetes ----
library(pacman)
p_load(animation, cowplot, extrafont, forcats, ggforce, gganimate, 
       ggrepel, ggridges, janitor, lubridate, purrr, 
       readxl, scales, stringi, stringr, tweenr, 
       tidyr, tidyverse, treemapify, zoo)

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)


### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

### Definir tema de gifs ----
tema_gif <-  theme_minimal() +
  theme(plot.title = element_text(size = 30, face = "bold", margin = margin(10,0,10,0)),
        plot.subtitle = element_text(size = 22, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0)),
        plot.caption = element_text(hjust = 0, size = 14),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16),
        legend.title.align = 0.5, 
        axis.title = element_text(size = 22, hjust = 1, face = "bold", margin = margin(0,0,0,0)),
        axis.text = element_text(size = 20, face = "bold"))

### Importar y transformar datos de PREPs ----

p_2006 <- read_delim("01_datos/PREP2006-Presidente/PREP2006-Presidente.txt", "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "latin1"))

p_2009 <- read_delim("01_datos/20090706_2000-listaActas/diputados.txt", "|", escape_double = FALSE, trim_ws = TRUE, skip = 5, locale = locale(encoding = "latin1"))

p_2012 <- read_delim("01_datos/20120702_2000-listaActas/presidente.txt", "|", escape_double = FALSE, trim_ws = TRUE, skip = 5, locale = locale(encoding = "latin1"))

p_2015 <- read_delim("01_datos/20150608_2010-listaActas/diputados.csv", "|", escape_double = FALSE, trim_ws = TRUE, skip = 6, locale = locale(encoding = "latin1"))

# Todos los datos recien impotados fueron previamente obtenidos del INE:

# PREP 2006: http://portalanterior.ine.mx/documentos/proceso_2005-2006/prep2006/bd_prep2006/bd_prep2006.htm

# PREP 2009: http://prep2009.ife.org.mx/PREP2009/20090706_2000-listaActas.tar.gz

# PREP 2012: http://prep2012.ife.org.mx/prep/20120702_2000-listaActas.tar.gz

# PREP 2015: http://prep2015.ine.mx/20150608_2010-listaActas.tar.gz

### "Limpiar" nombres de variables----
p_2006 <- p_2006 %>% clean_names()
p_2006 %>% glimpse()

p_2009 <- p_2009 %>% clean_names()
p_2009 %>% glimpse()

p_2012 <- p_2012 %>% clean_names()
p_2012 %>% glimpse()

p_2015 <- p_2015 %>% clean_names()
p_2015 %>% glimpse()

### Eliminar primer y último renglón para PREP 2006 ----
p_2006 <- p_2006 %>% 
  slice(2:n()-1) 

### Transformar columnas de tiempo ----

# PREP 2006
p_2006 <- p_2006 %>% 
  mutate(hora_captura = str_replace_all(hora_captura_cedat, "/", "-"), 
         hora_captura = str_replace_all(hora_captura, "-06", "-2006"),
         hora_captura = str_replace_all(hora_captura, ",000000", ""),
         hora_captura = dmy_hms(hora_captura),
         hora_recepcion = str_replace_all(hora_recepcion_cedat, "/", "-"), 
         hora_recepcion = str_replace_all(hora_recepcion, "-06", "-2006"),
         hora_recepcion = str_replace_all(hora_recepcion, ",000000", ""),
         hora_recepcion = dmy_hms(hora_recepcion)) %>%
  separate(hora_registro, into = c("hora_registro", "basura"), sep = ",") %>% 
  select(-basura) %>% 
  mutate(hora_registro = str_replace_all(hora_registro, "/", "-"), 
         hora_registro = str_replace_all(hora_registro, "-06", "-2006"),
         hora_registro = dmy_hms(hora_registro)) 

p_2006 %>% glimpse()

# PREP 2009
p_2009 <- p_2009 %>% 
  mutate(hora_acopio = ifelse(hora_acopio == "null", NA, hora_acopio), # Reemplazar valores "null" por NA
         hora_captura = ifelse(hora_captura == "null", NA, hora_captura), # Reemplazar valores "null" por NA
         hora_acopio = dmy_hms(hora_acopio), # Transformar tipo de dato a ymd_hms
         hora_captura = dmy_hms(hora_captura)) # Transformar tipo de dato a ymd_hms

p_2009 %>% glimpse()


# PREP 2012
p_2012 <- p_2012 %>% 
  mutate(hora_acopio = ifelse(hora_acopio == "null", NA, hora_acopio), # Reemplazar valores "null" por NA
         hora_captura = ifelse(hora_captura == "null", NA, hora_captura), # Reemplazar valores "null" por NA
         hora_acopio = ymd_hms(hora_acopio), # Transformar tipo de dato a ymd_hms
         hora_captura = ymd_hms(hora_captura)) # Transformar tipo de dato a ymd_hms

p_2012 %>% glimpse()


# PREP 2015
p_2015 <- p_2015 %>% 
  mutate(hora_acopio = ymd_hms(hora_acopio), # Transformar tipo de dato a ymd_hms
         hora_captura = ymd_hms(hora_captura)) # Transformar tipo de dato a ymd_hms

p_2015 %>% glimpse()


### Transformar columnas de votación de partidos y coaliciones ----

# PREP 2012
p_2012 <- p_2012 %>%
  mutate_if(is.character, funs(ifelse(. == "Ilegible", NA, ifelse(. == "Sin dato", NA, .)))) %>% 
  mutate(observaciones = as.factor(observaciones),
         tipo_casilla = as.factor(tipo_casilla),
         cryt = as.factor(cryt)) %>% 
  mutate_if(is.character, as.numeric) 

# PREP 2015
p_2015 <- p_2015 %>%
  mutate_if(is.character, funs(ifelse(. == "Ilegible", NA, ifelse(. == "Sin dato", NA, ifelse(. == "-", NA, .))))) %>% 
  mutate(estado = as.factor(estado),
         distrito = as.factor(distrito),
         tipo_casilla = as.factor(tipo_casilla),
         observaciones = as.factor(observaciones),
         cryt = as.factor(cryt),
         sha = as.factor(sha)) %>% 
  mutate_if(is.character, as.numeric)


### Generar variable para registrar el número de actas que se tuvieron que procesar en las casillas de cada estado en 2015 ----

# Obtuvimos los datos del número de elecciones por entidad de (i) http://portalanterior.ine.mx/archivos3/portal/historico/recursos/IFE-v2/DECEYEC/DECEYEC-ProcesosElectorales/Calendario-Docs/ISU_Cal_Elect-2015.pdf y (ii) http://portal.te.gob.mx/sites/default/files/calendario/calendario_electoral_2015.pdf
p_2015 <- p_2015 %>% 
  mutate(num_act = case_when(estado == "BAJA CALIFORNIA SUR" ~ 4,
                             estado == "CAMPECHE" ~ 4,
                             estado == "COLIMA" ~ 4,
                             estado == "DISTRITO FEDERAL" ~ 3,
                             estado == "GUANAJUATO" ~ 3,
                             estado == "GUERRERO" ~ 4,
                             estado == "JALISCO" ~ 3,
                             estado == "MÉXICO" ~ 3,
                             estado == "MICHOACÁN" ~ 4,
                             estado == "MORELOS" ~ 3,
                             estado == "NUEVO LEÓN" ~ 4,
                             estado == "QUERÉTARO" ~ 4,
                             estado == "SAN LUIS POTOSÍ" ~ 4,
                             estado == "SONORA" ~ 4,
                             estado == "TABASCO" ~ 3,
                             estado == "YUCATÁN" ~ 3),
         num_act = ifelse(is.na(num_act), 1, num_act))
