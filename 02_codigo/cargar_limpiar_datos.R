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
