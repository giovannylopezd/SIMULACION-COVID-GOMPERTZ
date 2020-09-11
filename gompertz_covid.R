library(readr)
library(dplyr)
library(easynls)
library(ggplot2)
#IMPORTAR DATASET
dataset = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
#CREAR DF DEL PAIS
pais <- "BOL" #Indicar el codigo iso del pais
dspais <- dataset %>% 
    filter(iso_code == pais)
#LIMPIEZA DE DATOS
dspais$new_cases[is.na(dspais$new_cases)] <- 0 ##Nas a 0s
findatos <- as.Date("2020-07-19") #max(dspais$date) #Puede indicar una fecha limite manualmente (ej:"26-06-2020")
dspais <- dspais %>% filter(date <= findatos)
dias <- dspais %>% nrow()
dstotalcasos <- dspais %>%
    mutate(acumulados = cumsum(new_cases), dia = c(1:dias)) %>%  
    select(dia,acumulados)
dstotalcasos = as.data.frame(dstotalcasos)
#Modelar nls gompertz
maximo <- max(dstotalcasos)*1.1
options(scipen = 999)
modelo = nlsfit(dstotalcasos,model = 10, start = c(a = maximo,b = 2,c = 0.1))
#----------------------Gompertz con NLS----------------------#
modelo1 = nls(acumulados ~ a*exp(-b*exp(-c*dia)),
              data = dstotalcasos, 
              start = list(a = maximo, b = 2, c = 0.1))
intervalos <- confint(modelo1,level = 0.95)
#------------------------------------------------------------#
nlsplot(dstotalcasos,model = 10,start = c(a = maximo,b = 2,c = .1),
        xlab = "Días", ylab = "Infectados por Covid en Bolivia")

#Establecer fecha pronostico
fechapronostico = as.Date("2020-09-30")
#conteo de dias de pronostico
diaspronostico <- as.numeric(as.Date(fechapronostico) - as.Date(min(dspais$date)) +1)

#Simulación hasta fecha de pronostico,
time_simul <- c(1:diaspronostico)
#Extraer los coeficientes del modelo
modelo <- as.data.frame(modelo)
alpha <- modelo$acumulados[1]
beta <- modelo$acumulados[2]
k <- modelo$acumulados[3]
#Reemplazar las variables guardadas
y_simul <- alpha*exp(-beta*exp(-k*time_simul))

#-------------------PLOT CASOS NUEVOS------------------------------
#Titulo de graficas
plotecuacion <- c("Infectados por Covid en Bolivia")
plotnuevos <- c("Escenario base de nuevos infectados por COVID-19 en Bolivia")
plotacumulados <- c("Pronóstico de Infectados por COVID-19 en Bolivia, estimación Gompertz")

dsimul <- as.data.frame(diff(y_simul))
dsimul <- dsimul %>%
    mutate(n_simul = diff(y_simul),
           dia = as.Date(seq(as.Date(min(dspais$date)),
                             as.Date(fechapronostico)-1,
                             by = 1))) %>% 
    select(dia,n_simul)

comienzadatos <- as.Date(min(dspais$date))
datasetdia <- dspais %>% 
    mutate(dia = seq(comienzadatos,
                     findatos,
                     by=1),
           interdia = new_cases) %>%
    select(dia,interdia)
datasetdia <- as.data.frame(datasetdia)

ggplot() +
    geom_line(data = dsimul,
              aes(dia,n_simul),color = "#619CFF",size = 1.1,alpha = 0.8) + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line(data = datasetdia,
              aes(dia, interdia), colour = "#00BFC4",size = 1,alpha = 0.8) +
    geom_vline(xintercept=findatos+1,
               linetype=2, size = 1, colour="#F8766D") +
    theme_gray() +
    labs(
        title = plotnuevos,
        y = "Casos diarios",
        x = " ")

#-----------------PARA PLOT CASOS ACUMULADOS------------------------
datasetacumulados <- dspais %>% 
    mutate(dia = seq(comienzadatos,findatos,by=1),
           acumulados = cumsum(new_cases)) %>%
    select(dia,acumulados)
datasetacumulados <- as.data.frame(datasetacumulados)
simul <- as.data.frame(y_simul)
simul <- simul %>%
    mutate(day = as.Date(seq(comienzadatos,fechapronostico,by = 1)))

ggplot() +
    geom_line(data = simul,
              aes(day,y_simul),color = "#619CFF",size = 1.1,alpha = 0.8) + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    geom_line(data = datasetacumulados,
              aes(dia, acumulados), colour = "#00BFC4",size = 1,alpha = 0.8) +
    geom_vline(xintercept=findatos+1,
               linetype=2, size = 1, colour="#F8766D") +
    theme_gray() +
    labs(
        title = plotacumulados,
        y = "Casos totales",
        x = " ")

#crear CSV de simulacion diaria
write.csv2(dsimul, file = "Simulacion Bolivia1.csv")

#Error Cuadrático Medio
RMSE = sqrt(1/nrow(dspais)*sum((dsimul %>% filter(dsimul$dia <= findatos) %>% select(n_simul) - dspais$new_cases)^2))
RMSE


