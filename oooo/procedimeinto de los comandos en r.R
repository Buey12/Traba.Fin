#Regresión logística (Comparación de dos modelos logísticos univariantes)
-------------------------------------------
#lectura bibliotecas
  library(vcd)
  library(ggplot2)
  library(readxl)
------------------------
#Data frame de los datos que tenemos en excel y apertura de los mismos
   
  datos <- data.frame(traba_F)
  View(datos)
------------------------
    #Procedimiento de la Regresión logistica 
#1. 
    datos <- read_excel("traba.F.xlsx")
  variabley <-datos$origen
  variablex <-datos$altura
  
#2.
  
  data <- data.frame(variabley, variablex)

#3.
  modelo_logistico <- glm(variabley ~ variablex, data = data, family = "binomial")
  
#4.
  summary(modelo_logistico)

#5. 
  predict(object = modelo_logistico, newdata = data.frame(variablex = 10000))
  
#6.
  confint(object = modelo_logistico, level = 0.95 )

#7.
  ggplot(data, aes(variablex, fill = factor(variabley))) + 
    geom_histogram(binwidth = 1, position = 'dodge') + 
    labs(title = 'Diferencia entre la variable de origen 1 y 0', 
         x = 'Altura', 
         y = 'Frecuencia')
#8.
  dif_residuos <- as.numeric(modelo_logistico$null.deviance - modelo_logistico$deviance)
  
  print(dif_residuos)

#9.
  df <- as.numeric(modelo_logistico$df.null - modelo_logistico$df.residual)
  
  print(df)

#10.
  p_value <- pchisq(q = dif_residuos, df = df, lower.tail = FALSE)
  
  print(p_value)
  
#11.
  paste("Diferencia de residuos:", round(dif_residuos, 4))
  
  paste("Grados de libertad:", df)
  
  paste("p-value:", p_value)

#12.
  predicciones <- ifelse(test = modelo_logistico$fitted.values > 0.5, yes = 1, no = 0)
  
  print(predicciones)
  
#13.
  matriz_confusion <- table(variabley, predicciones,
                            dnn = c("observaciones", "predicciones"))
  matriz_confusion

#14.
  mosaic(matriz_confusion, shade = T, colorize = T,
         gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
  
  
  
  