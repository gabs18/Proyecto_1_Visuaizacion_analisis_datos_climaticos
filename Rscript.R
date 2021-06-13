## Pasos preliminares
## Primero se cambió el tipo de separación decimal en el archivo csv de coma a punto, antes de importar los datos


# Carga de bibliotecas dplyr, ggpolt2 
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(viridis)
library(grid)
library(gridExtra)
library(scales)

pal <- viridis(10)
pal2 <- viridis(12)


## Creación de la variable
data <- read.csv(file = "liberia_datos_climaticos.csv", sep = ";", na.strings = "")
data <- na.omit(data)
View(data)
head(data)

## Limpiar: buscar si hay celdas vacías NA
data[!complete.cases(data),] 

## Cambio de nombre de las columnas para más facilidad
names(data) <- c("Fecha", "Temperatura", "Humedad", "VelocidadViento", "Lluvia", "Irradiacion", "Evapotranspiracion")

## Cambiar los datos de tipo numérico y fecha y verificación con class()
data$Temperatura <- as.numeric(data$Temperatura)
data$Fecha <- as.Date(data$Fecha, "%d/%m/%Y")

class(data$Fecha)
class(data$Temperatura)
class(data$Humedad)
class(data$VelocidadViento)
class(data$Lluvia)
class(data$Irradiacion)
class(data$EvapoTranspiracion)


## Histogramas de los datos climáticos en la estación de Liberia entre 2015-2019

hist1 <- ggplot(data) +
  geom_histogram(
    aes(x = Temperatura),
    binwidth = 1,
    color = "black",
    fill = "#cc3300"
  ) +
  ggtitle("Temperatura") +
  xlab("Temperatura (°C, Celsius)") +
  ylab("Frecuencia") +
  theme_ipsum()


hist2 <- ggplot(data) +
  geom_histogram(
    aes(x = Humedad),
    binwidth = 1,
    color = "black",
    fill = "#339900"
  ) +
  ggtitle("Humedad relativa") +
  xlab("Huemdad relativa (%)") +
  ylab("Frecuencia") +
  theme_ipsum()

hist3 <-  ggplot(data) +
  geom_histogram(
    aes(x = VelocidadViento),
    binwidth = 1,
    color = "black",
    fill = "#6666cc"
  ) +
  ggtitle("Velocidad del viento") +
  xlab("Velocidad (m/s)") +
  ylab("Frecuencia") +
  theme_ipsum()

hist4 <-  ggplot(data) +
  geom_histogram(
    aes(x = Lluvia),
    binwidth = 10,
    color = "black",
    fill = "#0099cc"
  ) +
  ggtitle("Precipitación") +
  xlab("Precipitación (mm)") +
  ylab("Frecuencia") +
  theme_ipsum() 

hist5 <-  ggplot(data) +
  geom_histogram(
    aes(x = Irradiacion),
    binwidth = 10,
    color = "black",
    fill = "#ffcc00"
  ) +
  ggtitle("Irradiación solar") +
  xlab("Irradación solar (W / m2)") +
  ylab("Frecuencia") +
  theme_ipsum()

hist6 <-  ggplot(data) +
  geom_histogram(
    aes(x = Evapotranspiracion),
    binwidth = 1,
    color = "black",
    fill = "#006699"
  ) +
  ggtitle("Evapotranspiración") +
  xlab("Evapotranspiración (mm)") +
  ylab("Frecuencia") +
  theme_ipsum()

# grid.arrange nos permite ordenar todos los paneles
grid.arrange(hist1, hist2, hist3, hist4, hist5, hist6,
             top = textGrob(
               gp = gpar(fontface = 2, fontsize = 23),
               "Histogramas con los datos climáticos de la estación de Liberia entre 2015 y 2019"))





# Cálculo de promedios y de sumas
monthly_data <- data %>%
  select(Fecha, Temperatura, VelocidadViento, Humedad, Lluvia, Irradiacion, Evapotranspiracion) %>%
  mutate (Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>%
  group_by(mes = format(Fecha, "%m")) %>%
  summarise(prom_temp = mean (Temperatura),
            prom_hum = mean (Humedad),
            prom_vel = mean (VelocidadViento),
            prom_lluvia = sum (Lluvia),
            prom_rad = mean (Irradiacion),
            prom_evapo = sum (Evapotranspiracion))

# Graficación de datos mensuales
graph1 <- ggplot(monthly_data, aes(x = mes, y = prom_temp, group = 1)) + 
  geom_line(size = 1.07, colour = "#cc3300") +
  geom_point(size = 2, colour = "#cc3300") +
  ggtitle('Temperatura mensual promedio') +
  xlab('Mes') + 
  ylab('°C, grados Celsius') +
  theme_ipsum()

graph2 <- ggplot(monthly_data, aes(x = mes, y = prom_hum, group = 1)) + 
  geom_line(size = 1.07, colour = "#339900") +
  geom_point(size = 2, colour = "#339900") +
  ggtitle('Humedad relativa mensual promedio') +
  xlab('Mes') + 
  ylab('% de humedad relativa') +
  theme_ipsum()

graph3 <- ggplot(monthly_data, aes(x = mes, y = prom_vel, group = 1)) + 
  geom_line(size = 1.07, colour = "#6666cc") +
  geom_point(size = 2, colour = "#6666cc") +
  ggtitle('Velocidad del viento mensual promedio') +
  xlab('Mes') + 
  ylab('m/s') +
  theme_ipsum()

graph4 <- ggplot(monthly_data, aes(x = mes, y = prom_lluvia, group = 1)) + 
  geom_line(size = 1.07, colour = "#0099cc") +
  geom_point(size = 2, colour = "#0099cc") +
  ggtitle('Precipitación mensual acumulada') +
  xlab('Mes') + 
  ylab('Precipitación acumulada (mm)') +
  theme_ipsum()

graph5 <- ggplot(monthly_data, aes(x = mes, y = prom_rad, group = 1)) + 
  geom_line(size = 1.07, colour = "#ffcc00") +
  geom_point(size = 2, colour = "#ffcc00") +
  ggtitle('Irradación mensual promedio') +
  xlab('Mes') + 
  ylab('Irradación solar (W / m2)') +
  theme_ipsum()


graph6 <- ggplot(monthly_data, aes(x = mes, y = prom_evapo, group = 1)) + 
  geom_line(size = 1.07, colour = "#006699") +
  geom_point(size = 2, colour = "#006699") +
  ggtitle('Evapotranspiración mensual acumulada') +
  xlab('Mes') + 
  ylab('Evapotranspiración (mm)') +
  theme_ipsum()

# grid.arrange nos permite ordenar todos los paneles
grid.arrange(graph1, graph2, graph3, graph4, graph5, graph6,
             top = textGrob(
               gp = gpar(fontface = 2, fontsize = 23),
               "Gráficos lineales con los datos climáticos de la estación de Liberia entre 2015 y 2019"))




# Gráficos de dispersión x-y para relacionar las variables entre si con 6 paneles en un solo gráfico
# 1. Temperatura e irradación: potencia de los rayos ultravioleta
scatter1 <- ggplot(data, aes(x = Temperatura, group = 1)) + 
  geom_point(aes(y = Irradiacion), colour = "#cc3300") +
  ggtitle('Temperatura en función de la irradiación') +
  xlab('Temperatura (°C)') + 
  ylab('Irradiación solar (W / m2)') +
  theme_ipsum()


# 2. Temperatura y humedad relativa: qué tan tropical está
scatter2 <- ggplot(data, aes(x = Temperatura, group = 1)) + 
  geom_point(aes(y = Humedad), colour = "#339900") +
  ggtitle('Temperatura en función de la humedad relativa') +
  xlab('Temperatura (°C)') + 
  ylab('Humedad relativa (%)') +
  theme_ipsum()


# 3. Humedad relativa y lluvia: cuánta lluvia se queda en el ambiente
scatter3 <- ggplot(data, aes(x = Humedad, group = 1)) + 
  geom_point(aes(y = Lluvia), colour = "#0099cc") +
  ggtitle('Humedad en función de la lluvia') +
  xlab('Temperatura (°C)') + 
  ylab('Precipitación (mm)') +
  theme_ipsum()


# 4. Humedad relativa y evapotranspiración: qué tanta humedad se evotranspira
scatter4 <- ggplot(data, aes(x = Humedad, group = 1)) + 
  geom_point(aes(y = Evapotranspiracion), colour = "#006699") +
  ggtitle('Humedad relativa en función de la evapotranspiración') +
  xlab('Humedad relativa (%)') + 
  ylab('Evapotranspiración (mm)') +
  theme_ipsum()


# 5. Evapotranspiración y velocidad del viento: qué tan rápido se evapotranspira
scatter5 <- ggplot(data, aes(x = Evapotranspiracion, group = 1)) + 
  geom_point(aes(y = VelocidadViento), colour = "#6666cc") +
  ggtitle('Evapotranspiración en función de la velocidad del viento') +
  xlab('Evapotranspiración (mm)') + 
  ylab('Velocidad del viento (m/s)') +
  theme_ipsum()


# 6. Velocidad del viento e irradación: clima árido y seco
scatter6 <- ggplot(data, aes(x = Evapotranspiracion, group = 1)) + 
  geom_point(aes(y = Irradiacion), colour = "#ffcc00") +
  ggtitle('Evapotranspiración en función de la irradiación') +
  xlab('Evapotranspiración (mm)') + 
  ylab('Irradiación solar (W / m2)') +
  theme_ipsum()


# grid.arrange nos permite ordenar todos los paneles
grid.arrange(scatter1, scatter2, scatter3, scatter4, scatter5, scatter6,
             top = textGrob(
               gp = gpar(fontface = 2, fontsize = 23),
               "Gráficos de dispersión con los datos climáticos de la estación de Liberia entre 2015 y 2019"))


  
  
  
    
  
  
  
 

