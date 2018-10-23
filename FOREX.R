# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()

# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl","RMySQL", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 17
# Uso horario
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "EUR_USD"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
OA_Pr <- "H4"
# Multiplicador de precios para convertir a PIPS
MultPip_MT1 <- 10000

Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                           DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                           Instrument = OA_In, 
                           Start = NULL, End = NULL, Count = 900)
suppressMessages(library(plotly)) # Graficas interactivas
Datos <- list()
Datos <- Precios_Oanda
Historico <- data.frame("Date"= Datos[[1]],
                        "Precio"= Datos[[5]])
EMA1 <- EMA(Historico[,2],n=10, wilder = FALSE, ratio = NULL)
Historico <- data.frame("Date"= Datos[[1]],
                        "Preciocierre"= Datos[[5]],
                        "Precioapertura"=Datos[[2]],
                        "EMA" = EMA1,
                        "R_Precio" = 0,
                        "R_Activo" = 0,
                        "R_Cuenta" = 0,
                        "Capital"=0,
                        "Balance"=0,
                        "Unidades" = 0,
                        "unidades_a" = 0,
                        "Flotante"=0,
                        "Ganancia" = 0, "Ganancia_Acumulada" = 0,
                        "Mensaje"=NA)
Runidades <- 10000
Capitalinicial <- 10000000
#Unidades posicion inicial
Historico$Unidades[9] <- Runidades


#Calcular los titulos acumulados
Historico$unidades_a[9] <- Historico$Unidades[9]

#Calcular Ganancia
Historico$Ganancia[9]<- (Historico$Preciocierre[9]-Historico$Precioapertura[9])*Runidades

#Ganancia ACumulada
Historico$Ganancia_Acumulada[9]<- Historico$Ganancia[9]
Historico$Flotante[9]<- Historico$unidades_a[9]*Historico$Preciocierre[9]
#Todo remanente se deja registrado en la cuenta de efectivo
Historico$Capital[9] <- Capitalinicial-Historico$Flotante[9]
#Calcular el Balance
Historico$Balance[9] <- Historico$Flotante[9]+ Historico$Capital[9]
Historico$Mensaje[9] <- "Inicializacion de cartera"
#Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Preciocierre))),4)

for(i in 10:length(Historico$Date)){
  
  if(Historico$Precioapertura[i]  <= Historico$EMA[i]){
  if(Historico$Capital[i-1] > 0){  
  if(Historico$Capital[i-1]>Runidades*Historico$Precioapertura[i]){ # Si Capital minimo
      
    Historico$Unidades[i]   <- Runidades
    
    Historico$unidades_a[i] <- Historico$unidades_a[i-1]+Historico$Unidades[i]
    
    
    
    Historico$Flotante[i]<- Historico$Preciocierre[i]*Historico$Unidades[i]
    Historico$Capital[i] <- Historico$Capital[i-1]-Runidades*Historico$Preciocierre[i]

    Historico$Ganancia[i]<-(Historico$Preciocierre[i]-Historico$Precioapertura[i])*Historico$Unidades[i]
    Historico$Ganancia_Acumulada[i]<- Historico$Ganancia[i]+Historico$Ganancia_Acumulada[i-1]
    Historico$Mensaje[i] <- "Señal de compra ejecutada"
  
    }else{
    Historico$Unidades[i]   <- 0
    
    Historico$unidades_a[i] <- Historico$unidades_a[i-1]+Historico$Unidades[i]
    
    
    
    Historico$Flotante[i]<- Historico$Preciocierre[i]*Historico$Unidades[i]
    Historico$Capital[i] <- Historico$Capital[i-1]
    Historico$Ganancia[i]<-0
    Historico$Ganancia_Acumulada[i]<- Historico$Ganancia_Acumulada[i-1]
    Historico$Mensaje[i] <- "No hubo capital"
    }}
    else{
      Historico$Unidades[i]   <- 0
      
      Historico$unidades_a[i] <- Historico$unidades_a[i-1]+Historico$Unidades[i]
      
      
      
      Historico$Flotante[i]<- Historico$Preciocierre[i]*Historico$Unidades[i]
      Historico$Capital[i] <- Historico$Capital[i-1] 
      Historico$Ganancia[i]<-0
      Historico$Ganancia_Acumulada[i]<- Historico$Ganancia_Acumulada[i-1]
      Historico$Mensaje[i] <- "No hubo capital"
    }}
  else{
    if(Historico$Capital[i-1] > 0){  
      if(Historico$Capital[i-1]>Runidades*Historico$Precioapertura[i]){# Si Capital minimo
        
        Historico$Unidades[i]   <- Runidades
        
        Historico$unidades_a[i] <- Historico$unidades_a[i-1]+Historico$Unidades[i]
        
        
        Historico$Flotante[i]<- Historico$Preciocierre[i]*Historico$Unidades[i]
        Historico$Capital[i] <- Historico$Capital[i-1]-Runidades*Historico$Preciocierre[i]
        Historico$Ganancia[i]<-(Historico$Precioapertura[i]-Historico$Preciocierre[i])*Historico$Unidades[i]
        Historico$Ganancia_Acumulada[i]<- Historico$Ganancia[i]+Historico$Ganancia_Acumulada[i-1]
        Historico$Mensaje[i] <- "Señal de venta ejecutada"
      }else{
        Historico$Unidades[i]   <- 0
        
        Historico$unidades_a[i] <- Historico$unidades_a[i-1]+Historico$Unidades[i]
        
        
        
        Historico$Flotante[i]<- Historico$Preciocierre[i]*Historico$Unidades[i]
        Historico$Capital[i] <- Historico$Capital[i-1]
        Historico$Ganancia[i]<-0
        Historico$Ganancia_Acumulada[i]<- Historico$Ganancia_Acumulada[i-1]
        Historico$Mensaje[i] <- "No hubo capital"
      }}
    else{
      Historico$Unidades[i]   <- 0
      
      Historico$unidades_a[i] <- Historico$unidades_a[i-1]+Historico$Unidades[i]
      
      
      
      Historico$Flotante[i]<- Historico$Preciocierre[i]*Historico$Unidades[i]
      Historico$Capital[i] <- Historico$Capital[i-1] 
      Historico$Ganancia[i]<-0
      Historico$Ganancia_Acumulada[i]<- Historico$Ganancia_Acumulada[i-1]
      Historico$Mensaje[i] <- "No hubo capital"
    }}
}



plot_ly(x = Historico[,1], type="candlestick", open = Historico$Precioapertura, close=Historico$Preciocierre, high = Precios_Oanda$High, low=Precios_Oanda$Low) %>%
  layout(tittle = "Basic Candlestick Chart")
 
plot_ly(x = Historico[,1], y = Historico[,4], type = 'scatter', mode = 'lines', name = 'EMA',
            
            line = list(color = 'blue'), hoverinfo = "text", text = ~paste('EMA', Historico[,4]))  %>%
 
  layout(title = "EMA VS PRECIO DE CIERRE",
         
         xaxis = list(title = "Fechas", showgrid = T),
         
         yaxis = list(title = "Precios"), 
         
         legend = list(orientation = 'h', y = -0.25, x = 0.5))     
      
      
  #quantconnect
      