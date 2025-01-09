###########################################
#####MERGE Y ANALISIS CON DATOS REALES#####
###########################################


#################
####LIBRERIAS####
#################
library(readxl)
library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)
library(lubridate)
library(fastDummies)
library(expss)
library(car)


#############################
####DIRECTORIO DE TRABAJO####
#############################
setwd("C:/SEGUNDO SEMESTRE/Taller de Estadística/SEGUNDO PARCIAL")


#####################################
###ABRAMOS NUESTRAS BASES DE DATOS###
#####################################
#Exportaciones 2023
bd_exp2023 <- read_excel("EXPORTACIONES 2023p.xlsx")

#Exportaciones 2022
bd_exp2022 <- read_excel("EXPORTACIONES 2022p.xlsx")

#Games Workshop
bd_gw <- read_excel("GamesWorkshop.xlsx")

#Hasbro
bd_hasbro <- read_excel("Hasbro.xlsx")

#######################################
####HAGAMOS LAS UNIONES RESPECTIVAS####
#######################################
#Exportaciones
bd_exp <- rbind(bd_exp2023,
                bd_exp2022)

#Precio de acciones (fechas)
bd_gw$Date <- as.Date(bd_gw$`Exchange Date`, format = "%Y-%b-%d")
bd_hasbro$Date <- as.Date(bd_hasbro$`Exchange Date`, format = "%Y-%b-%d")

#Precio de acciones (union)
bd_ts <- left_join(bd_gw,
                   bd_hasbro,
                   by = c("Date" = "Date"),
                   suffix = c("_gw", "_has"))

###########################
####ANALISIS UNIVARIADO####
###########################
#Nos concentraremos en el valor
ggplot(bd_exp,
       aes(x = VALOR)) +
  geom_histogram()

#Empecemos con algunas correcciones a los titulos y ejes
ggplot(bd_exp,
       aes(x = VALOR)) +
  geom_histogram(col = "black",
                 fill = "transparent") +
  labs(
    title = "Histograma del valor de exportaciones",
    x = "Valor exportaciones",
    y = "Frecuencia"
  )

#Mucho mejor, pero eliminemos todos los elementos del fondo
ggplot(bd_exp,
       aes(x = VALOR)) +
  geom_histogram(col = "black",
                 fill = "#D3D3D3") +
  labs(
    title = "Histograma del valor de exportaciones",
    x = "Valor exportaciones",
    y = "Frecuencia"
  ) +
  theme_minimal()

#Ajustemos los valores del eje x
ggplot(bd_exp,
       aes(x = VALOR/1000000)) +
  geom_histogram(col = "black",
                 fill = "#D3D3D3") +
  labs(
    title = "Histograma del valor de exportaciones",
    x = "Valor exportaciones (en millones de USD)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Podemos observar que nuestros datos parecen seguir una distribucion exponencial
#Intentemos transformar nuestros valores
#Ajustemos los valores del eje x
ggplot(bd_exp,
       aes(x = log(VALOR))) +
  geom_histogram(col = "black",
                 fill = "#D3D3D3") +
  labs(
    title = "Histograma del valor de exportaciones",
    x = "Valor exportaciones (en logaritmo)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Pongamos encima una distribucion normal perfecta
ggplot(bd_exp,
       aes(x = log(VALOR))) +
  geom_histogram(aes(y = ..density..),
                 col = "black",
                 fill = "#D3D3D3",
                 alpha = 0.7) +
  labs(
    title = "Histograma del valor de exportaciones",
    x = "Valor exportaciones (en logaritmo)",
    y = "Densidad"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  stat_function(fun = dnorm,
                args = list(mean = mean(log(bd_exp$VALOR)),
                            sd = sd(log(bd_exp$VALOR))),
                color = "red",
                linewidth = 1)

#En cierta forma parece ser que nuestros pueden seguir una distribucion normal
#Utilicemos un qq-plot para ver que tan posible es
qqnorm(log(bd_exp$VALOR), 
       col = 'blue',
       cex = 0.5)
qqline(log(bd_exp$VALOR), 
       col = 'red',
       lwd = 2)

#Veamos esta version
qqPlot(log(bd_exp$VALOR),
       dist = "norm",
       envelope = c(
         level = 0.99),
       cex = 0.5,
       id = FALSE,
       ylim = c(-10,30),
       xlab = "Cuantil muestral",
       ylab = "Cuantil teorico",
       main = "QQ-Plot de log valor exportaciones")

#Hagamos un boxplot para analizar
ggplot(bd_exp,
       aes(y = log(VALOR))) +
  geom_boxplot(fill = "#D3D3D3",
               color = "black") +
  labs(title = "Boxplot del valor",
       x = "Valor exportaciones",
       y = "Valor") +
  theme_minimal()

#Podemos diferencias por anho
bd_exp$GESTION <- factor(bd_exp$GESTION)
ggplot(bd_exp,
       aes(x = GESTION,
           y = log(VALOR),
           group = GESTION,
           fill = GESTION)) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot del valor",
       x = "Anho",
       y = "Valor") +
  theme_minimal() +
  scale_fill_manual(values = c("2022" = "lightblue",
                               "2023" = "lightgreen"),
                    name = "Valor exportaciones",
                    labels = c("2022" = "2022",
                               "2023" = "2023"))

#################
####EJERCICIO####
#################
#1. Creen una base de datos del Banco Mundial con todos los paises para los anhos
#2020, 2010, 2000, 1990, 1980
install.packages("WDI")  
library(WDI)
# Indicadores a descargar
indicadores <- c("NY.GDP.MKTP.CD", "SP.POP.TOTL", "SP.DYN.LE00.IN")

# Descargar datos del Banco Mundial
bd_mundial <- WDI(
  country = "all",           # Todos los países
  indicator = indicadores,   # Indicadores seleccionados
  start = 1980,              # Año inicial
  end = 2020                 # Año final
)

# Filtrar los años de interés
bd_mundial <- bd_mundial[bd_mundial$year %in% c(1980, 1990, 2000, 2010, 2020), ]

# Mostrar una vista previa
head(bd_mundial)

# Renombrar columnas
colnames(bd_mundial) <- c(
  "Pais", "Codigo", "PIB", "Poblacion", "Esperanza_de_Vida", "Anho"
)

# Vista previa
head(bd_mundial)

write.csv(bd_mundial, "bd_banco_mundial.csv", row.names = FALSE)


#2. Realicen un analisis de la poblacion a traves de cada uno de estos anhos
#similar al que acabamos de hacer
# Histograma de la población en 2020
ggplot(BDM_filtrada %>% filter(anho == 2020),
       aes(x = valor)) +
  geom_histogram(color = "black", 
                 fill = "#D3D3D3", 
                 bins = 30) +
  labs(
    title = "Histograma del tamaño de población (2020)",
    x = "Población",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Histograma logarítmico para varios años
ggplot(BDM_filtrada,
       aes(x = log(valor))) +
  geom_histogram(color = "black", 
                 fill = "#D3D3D3", 
                 bins = 30) +
  facet_wrap(~anho) +
  labs(
    title = "Histograma del tamaño de población (Log transformado)",
    x = "Log(Población)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# QQ-Plot por año
library(car)
for (year in c(2020, 2010, 2000, 1990, 1980)) {
  qqPlot(
    log(BDM_filtrada$valor[BDM_filtrada$anho == year]),
    dist = "norm",
    main = paste("QQ-Plot de log(Población) para", year),
    envelope = c(level = 0.99)
  )
}

# Boxplot comparativo por año
ggplot(BDM_filtrada,
       aes(x = as.factor(anho), 
           y = log(valor), 
           fill = as.factor(anho))) +
  geom_boxplot(color = "black") +
  labs(
    title = "Distribución de población (Log transformado)",
    x = "Año",
    y = "Log(Población)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1980" = "lightblue",
                               "1990" = "lightgreen",
                               "2000" = "yellow",
                               "2010" = "orange",
                               "2020" = "red"))
