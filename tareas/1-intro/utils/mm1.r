library(tidyverse)

mm1 <- function(lambda_a, lambda_s, n){
  # Simular los tiempos de llegada y de servicio
  tiempo_llegada <- cumsum(rexp(n, 1/lambda_a))
  tiempo_servicio <- rexp(n, 1/lambda_s)
  
  # Tiempos en los que cada persona sale del sistema
  tiempo_salida<- c(tiempo_llegada[1]+tiempo_servicio[1], rep(0, n-1))
  for(i in 2:n)
    tiempo_salida[i] <- tiempo_servicio[i] + pmax(tiempo_llegada[i], tiempo_salida[i-1])
  
  #Data frame de eventos en el sistema por intervalo de tiempo
  eventos <- data_frame(tiempo_final = c(tiempo_llegada, tiempo_salida),
                        trigger_cambio = as.factor(c(rep('llegada', n), rep('salida', n)))) %>%
    arrange(tiempo_final) %>%
    mutate(tiempo_inicial = ifelse(is.na(lag(tiempo_final)), 0, lag(tiempo_final)),
           delta_personas_sistema = ifelse(trigger_cambio=='llegada', 1, -1),
           personas_en_sistema = lag(cumsum(delta_personas_sistema)),
           personas_en_sistema = ifelse(is.na(personas_en_sistema),0,personas_en_sistema),
           personas_en_fila = pmax(0, personas_en_sistema - 1)) %>%
    select(tiempo_inicial, tiempo_final, personas_en_sistema, personas_en_fila, trigger_cambio, -delta_personas_sistema)
  
  # ESTADÍSTICA 1: Tiempo total de ejecución del sistema
  tiempo_simulacion <- max(tiempo_salida)
  
  # ESTADÍSTICA 2: Tiempo promedio que cada persona espera para ser atendida
  tiempo_fila <- tiempo_salida - (tiempo_llegada + tiempo_servicio)
  espera_promedio <- sum(tiempo_fila)/tiempo_simulacion
  
  # ESTADÍSTICA 3: Número promedio de clientes esperando en la fila
 longitud_promedio <- sum(tiempo_fila)/n
  
  # ESTADÍSTICA 4: Ocupación promedio del servidor
  uso_promedio <- eventos %>%
    filter(personas_en_sistema>0) %>%
    summarise(op = sum(tiempo_final-tiempo_inicial)/tiempo_simulacion) %>%
    pull()
  
  # ESTADÍSTICA 5: Tiempo total promedio en el sistema
  tiempo_total_promedio <- sum(tiempo_fila+tiempo_servicio)/n
  
  # ESTADÍSTICA 6: Máxima longitud de la cola
  max_longitud_cola <- eventos %>%
    pull(personas_en_fila) %>%
    max()
  
  #ESTADÍSTICA 7: Máxima espera en cola
  max_tiempo_cola <- max(tiempo_fila)

  return(list(tiempo_simulacion=tiempo_simulacion, 
              espera_promedio=espera_promedio, 
              longitud_promedio=longitud_promedio, 
              uso_promedio=uso_promedio,
              tiempo_total_promedio=tiempo_total_promedio, 
              max_longitud_cola=max_longitud_cola,
              max_tiempo_cola=max_tiempo_cola))
}
