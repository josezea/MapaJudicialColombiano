library(tabulizer)
library(dplyr)
library(tidyr)
library(here)
library(writexl)
library(readxl)

setwd("..")
ruta <- here::here()
# setwd(ruta)
setwd("datos")

df_divipola <- read_excel("Geoportal del DANE - Codificación Divipola.xlsx")
df_divipola <- df_divipola %>% select(`Código Departamento`,	`Código Municipio`,	
                                      nom_departamento = `Nombre Departamento`, nom_mpio = `Nombre Municipio`)
df_divipola <- unique(df_divipola)


t_pg1 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 1, 
                        area =  list(c(74.37044, 152.55702, 767.86357, 
                                       470.81680)),
                        guess = FALSE)[[1]]
names(t_pg1) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")
t_pg1$nom_distrito <- ifelse(t_pg1$nom_distrito == "", NA, t_pg1$nom_distrito)
t_pg1$nom_circuito  <- ifelse(t_pg1$nom_circuito  == "", NA, t_pg1$nom_circuito )
t_pg1 <- t_pg1 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)

t_pg1 <- t_pg1 %>% dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")



t_pg2a <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                         output = "data.frame",
                         pages = 2, 
                         area =  list(c(26.48705 , 22.62484 , 109.83476 , 
                                        437.11955)),
                         guess = FALSE)[[1]]

t_pg2a <- data.frame(num_distrito = 1, nom_distrito = "ANTIOQUIA", 
                     num_circuito = c(31,t_pg2a$X31),
                     nom_circuito = c("URRAO", t_pg2a$URRAO), 
                     num_mpio = c(1, t_pg2a$X1), nom_mpio = c("URRAO",t_pg2a$URRAO.1))
t_pg2a$nom_circuito <- ifelse(t_pg2a$nom_circuito == "", NA, t_pg2a$nom_circuito)

t_pg2a <- t_pg2a %>% tidyr::fill(num_circuito , nom_circuito)



t_pg2 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 2, 
                        area =  list(c(102.6127 , 160.4502, 759.8159, 
                                       444.3370)),
                        guess = FALSE)[[1]]
names(t_pg2) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")

t_pg2 <- t_pg2[,1:6]
t_pg2$nom_distrito <- ifelse(t_pg2$nom_distrito == "", NA, t_pg2$nom_distrito)
t_pg2$nom_circuito  <- ifelse(t_pg2$nom_circuito  == "", NA, t_pg2$nom_circuito)

t_pg2$num_distrito[1] <- 2
t_pg2$nom_distrito[1] <- "ARAUCA"
t_pg2 <- t_pg2 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)

t_pg2 <- t_pg2 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")


t_pg2 <- rbind(t_pg2a, t_pg2)



t_pg3a <- data.frame(num_distrito = 7, nom_distrito = "BUCARAMANGA",
                     num_circuito = 4, 
                     nom_circuito = "SAN VICENTE CHUCURI",
                     num_mpio = 1:2,
                     nom_mpio = c("SAN VICENTE CHUCURI", "EL CARMEN"))

t_pg3 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 3, 
                        area =  list(c(68.17162 , 147.82071 , 686.49270 , 
                                       460.70954)),
                        guess = FALSE)[[1]]
names(t_pg3) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")

t_pg3 <- t_pg3[,1:6]
t_pg3$nom_distrito <- ifelse(t_pg3$nom_distrito == "", NA, t_pg3$nom_distrito)
t_pg3$nom_circuito  <- ifelse(t_pg3$nom_circuito  == "", NA, t_pg3$nom_circuito )
t_pg3$num_distrito[1] <- 8
t_pg3$nom_distrito[1] <- "BUGA"

t_pg3 <- t_pg3 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)

t_pg3 <- rbind(t_pg3a, t_pg3)

t_pg3 <- t_pg3 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")



t_pg4 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 4, 
                        area =  list(c(29.96076 , 158.25034, 745.54584  , 
                                       474.61571)),
                        guess = FALSE)[[1]]
names(t_pg4) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")


t_pg4 <- t_pg4[,1:6]
t_pg4$nom_distrito <- ifelse(t_pg4$nom_distrito == "", NA, t_pg4$nom_distrito)
t_pg4$nom_circuito  <- ifelse(t_pg4$nom_circuito  == "", NA, t_pg4$nom_circuito )
t_pg4$num_distrito[1] <- 11
t_pg4$nom_distrito[1] <- "CUCUTA"
t_pg4 <- t_pg4 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)

t_pg4 <- t_pg4 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")



t_pg5a <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                         output = "data.frame",
                         pages = 5, 
                         area =  list(c(16.06591  , 234.73427 , 273.12074  , 
                                        443.32683 )),
                         guess = FALSE)[[1]]

t_pg5a <- data.frame(num_distrito = 12, nom_distrito = "CUNDINAMARCA",
                     num_circuito = c(13, t_pg5a$X13 ),
                     nom_circuito = c("SOACHA", t_pg5a$SOACHA),
                     num_mpio = c(1, t_pg5a$X1), nom_mpio = c("SOACHA",t_pg5a$SOACHA.1))
t_pg5a$nom_circuito <- ifelse(t_pg5a$nom_circuito == "", NA, t_pg5a$nom_circuito)

t_pg5a <- t_pg5a %>% tidyr::fill(num_circuito , nom_circuito)




t_pg5 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 5, 
                        area =  list(c(266.1733 , 165.2034 , 717.7561 , 
                                       457.2330)),
                        guess = FALSE)[[1]]
names(t_pg5) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")

t_pg5 <- t_pg5[,1:6]
t_pg5$nom_distrito <- ifelse(t_pg5$nom_distrito == "", NA, t_pg5$nom_distrito)
t_pg5$nom_circuito  <- ifelse(t_pg5$nom_circuito  == "", NA, t_pg5$nom_circuito )
t_pg5$num_distrito[1] <- 13
t_pg5$nom_distrito[1] <- "FLORENCIA"
t_pg5 <- t_pg5 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg5 <- rbind(t_pg5a, t_pg5)

t_pg5 <- t_pg5 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")




t_pg6 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 6, 
                        area =  list(c(33.43448, 147.82071, 672.59785, 
                                       474.61571)),
                        guess = FALSE)[[1]]
names(t_pg6) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")


t_pg6 <- t_pg6[,1:6]
t_pg6$nom_distrito <- ifelse(t_pg6$nom_distrito == "", NA, t_pg6$nom_distrito)
t_pg6$nom_circuito  <- ifelse(t_pg6$nom_circuito  == "", NA, t_pg6$nom_circuito )

t_pg6$num_distrito[1] <- 15
t_pg6$nom_distrito[1] <- "MANIZALES"
t_pg6 <- t_pg6 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg6 <- t_pg6 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")
t_pg6$num_mpio <- as.numeric(t_pg6$num_mpio)




t_pg7 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 7, 
                        area =  list(c(29.96076, 154.77379, 742.07213 , 
                                       467.66262)),
                        guess = FALSE)[[1]]
names(t_pg7) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")


t_pg7 <- t_pg7[,1:6]
t_pg7$nom_distrito <- ifelse(t_pg7$nom_distrito == "", NA, t_pg7$nom_distrito)
t_pg7$nom_circuito  <- ifelse(t_pg7$nom_circuito  == "", NA, t_pg7$nom_circuito )

t_pg7$num_distrito[1] <- 19
t_pg7$nom_distrito[1] <- "NEIVA"
t_pg7 <- t_pg7 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg7 <- t_pg7 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")




t_pg8a <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                         output = "data.frame",
                         pages = 8, 
                         area =  list(c(23.01334, 231.25773, 151.54075  , 
                                        450.27991)),
                         guess = FALSE)[[1]]

t_pg8a <- data.frame(num_distrito = 21, nom_distrito = "PASTO",
                     num_circuito = c(7, t_pg8a$X7),
                     nom_circuito = c("TUMACO", t_pg8a$TUMACO),
                     num_mpio = c(1, t_pg8a$X1), nom_mpio = c("TUMACO",t_pg8a$TUMACO.1))
t_pg8a$nom_circuito <- ifelse(t_pg8a$nom_circuito == "", NA, t_pg8a$nom_circuito)
t_pg8a <- t_pg8a %>% tidyr::fill(num_circuito , nom_circuito)




t_pg8 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 8, 
                        area =  list(c(144.5933, 154.7738, 755.9670, 
                                       453.7565)),
                        guess = FALSE)[[1]]
names(t_pg8) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")

t_pg8 <- t_pg8[,1:6]
t_pg8$nom_distrito <- ifelse(t_pg8$nom_distrito == "", NA, t_pg8$nom_distrito)
t_pg8$nom_circuito  <- ifelse(t_pg8$nom_circuito  == "", NA, t_pg8$nom_circuito )
t_pg8$num_distrito[1] <- 22
t_pg8$nom_distrito[1] <- "PEREIRA"
t_pg8 <- t_pg8 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg8 <- t_pg8 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")
t_pg8 <- rbind(t_pg8a, t_pg8)




t_pg9 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                        output = "data.frame",
                        pages = 9, 
                        area =  list(c(33.43448 , 158.25034 , 721.22984, 
                                       471.13917 )),
                        guess = FALSE)[[1]]
names(t_pg9) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")


t_pg9 <- t_pg9[,1:6]
t_pg9$nom_distrito <- ifelse(t_pg9$nom_distrito == "", NA, t_pg9$nom_distrito)
t_pg9$nom_circuito  <- ifelse(t_pg9$nom_circuito  == "", NA, t_pg9$nom_circuito )

t_pg9$num_distrito[1] <- 25
t_pg9$nom_distrito[1] <- "RIOHACHA"
t_pg9 <- t_pg9 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg9 <- t_pg9 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")





t_pg10 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                         output = "data.frame",
                         pages = 10, 
                         area =  list(c(26.48705, 147.82071, 703.86127, 
                                        457.23300)),
                         guess = FALSE)[[1]]
names(t_pg10) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")


t_pg10 <- t_pg10[,1:6]
t_pg10$nom_distrito <- ifelse(t_pg10$nom_distrito == "", NA, t_pg10$nom_distrito)
t_pg10$nom_circuito  <- ifelse(t_pg10$nom_circuito  == "", NA, t_pg10$nom_circuito )

t_pg10$num_distrito[1] <- 28
t_pg10$nom_distrito[1] <- "STA ROSA DE VITERBO"
t_pg10 <- t_pg10 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)

t_pg10 <- t_pg10 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")





t_pg11a <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                          output = "data.frame",
                          pages = 11, 
                          area =  list(c(33.43448, 227.78119, 373.85845   , 
                                         467.66262 )),
                          guess = FALSE)[[1]]

t_pg11a <- data.frame(num_distrito = 30, nom_distrito = "TUNJA",
                      num_circuito = c(2, t_pg11a$X2),
                      nom_circuito = c("CHIQUINQUIRA", t_pg11a$CHIQUINQUIRA),
                      num_mpio = c(1, t_pg11a$X1), 
                      nom_mpio = c("CHIQUINQUIRA", t_pg11a$CHIQUINQUIRA.1))
t_pg11a$nom_circuito <- ifelse(t_pg11a$nom_circuito == "", NA, t_pg11a$nom_circuito)
t_pg11a <- t_pg11a %>% tidyr::fill(num_circuito, nom_circuito)




t_pg11 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                         output = "data.frame",
                         pages = 11, 
                         area =  list(c(370.3847 , 161.7269 , 759.4407 , 
                                        488.5219)),
                         guess = FALSE)[[1]]
names(t_pg11) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")

t_pg11 <- t_pg11[,1:6]
t_pg11$nom_distrito <- ifelse(t_pg11$nom_distrito == "", NA, t_pg11$nom_distrito)
t_pg11$nom_circuito  <- ifelse(t_pg11$nom_circuito  == "", NA, t_pg11$nom_circuito )
t_pg11$num_distrito[1] <- 31
t_pg11$nom_distrito[1] <- "VALLEDUPAR"
t_pg11 <- t_pg11 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg11 <- t_pg11 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")
t_pg11 <- rbind(t_pg11a, t_pg11)



t_pg12a <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                          output = "data.frame",
                          pages = 12, 
                          area =  list(c(40.38191, 245.16390 , 165.43561    , 
                                         471.13917)),
                          guess = FALSE)[[1]]

t_pg12a <- data.frame(num_distrito = 33, nom_distrito = "VILLAVICENCIO",
                      num_circuito = c(5, t_pg12a$X5),
                      nom_circuito = c("PTO CARREÑO", t_pg12a[,2]),
                      num_mpio = c(1, t_pg12a$X1),
                      nom_mpio = c("PTO CARREÑO", t_pg12a[,4]))
t_pg12a$nom_circuito <- ifelse(t_pg12a$nom_circuito == "", NA, t_pg12a$nom_circuito)
t_pg12a <- t_pg12a %>% tidyr::fill(num_circuito, nom_circuito)



t_pg12 <- extract_tables("MAPA JUDICIAL Detallado.pdf",
                         output = "data.frame",
                         pages = 12, 
                         area =  list(c(161.9619, 161.7269, 311.3316  , 
                                        460.7095)),
                         guess = FALSE)[[1]]
names(t_pg12) <- c("num_distrito", "nom_distrito", "num_circuito", "nom_circuito", "num_mpio", "nom_mpio")

t_pg12 <- t_pg12[,1:6]
t_pg12$nom_distrito <- ifelse(t_pg12$nom_distrito == "", NA, t_pg12$nom_distrito)
t_pg12$nom_circuito  <- ifelse(t_pg12$nom_circuito  == "", NA, t_pg12$nom_circuito )
t_pg12$num_distrito[1] <- 33
t_pg12$nom_distrito[1] <- "YOPAL"
t_pg12 <- t_pg12 %>% tidyr::fill(num_circuito, nom_circuito) %>%
  tidyr::fill(num_distrito, nom_distrito)
t_pg12 <- t_pg12 %>% 
  dplyr::filter(nom_circuito != "CIRCUITOS JUDICIALES")
t_pg12 <- rbind(t_pg12a, t_pg12)



rm(t_pg2a); rm(t_pg3a); rm(t_pg5a); 
rm(t_pg8a); rm(t_pg11a);  rm(t_pg12a)



df <- bind_rows(t_pg1, t_pg2, t_pg3, t_pg4, t_pg5, t_pg6, t_pg7, t_pg8, t_pg9, t_pg10, t_pg11, t_pg12)




df$nom_distrito <- gsub("Ã‘", "Ñ", df$nom_distrito)
df$nom_distrito <- gsub("Ã\u0081", "Á", df$nom_distrito)
df$nom_distrito <- gsub("Ã‰", "É", df$nom_distrito)
df$nom_distrito <- gsub("Ã\u008d", "Í", df$nom_distrito)
df$nom_distrito <- gsub("Ã“", "Ó", df$nom_distrito)
df$nom_distrito <- gsub("Ãœ", "Ü", df$nom_distrito)

df$nom_circuito <- gsub("Ã‘", "Ñ", df$nom_circuito)
df$nom_circuito <- gsub("Ã\u0081", "Á", df$nom_circuito)
df$nom_circuito <- gsub("Ã‰", "É", df$nom_circuito)
df$nom_circuito <- gsub("Ã\u008d", "Í", df$nom_circuito)
df$nom_circuito <- gsub("Ã“", "Ó", df$nom_circuito)
df$nom_circuito <- gsub("Ãœ", "Ü", df$nom_circuito)

df$nom_mpio <- gsub("Ã‘", "Ñ", df$nom_mpio)
df$nom_mpio <- gsub("Ã\u0081", "Á", df$nom_mpio)
df$nom_mpio <- gsub("Ã‰", "É", df$nom_mpio)
df$nom_mpio <- gsub("Ã\u008d", "Í", df$nom_mpio)
df$nom_mpio <- gsub("Ã“", "Ó", df$nom_mpio)
df$nom_mpio <- gsub("Ãœ", "Ü", df$nom_mpio)

mapa_judicial <- df %>% select(nom_distrito, nom_circuito, nom_mpio)


rm(t_pg1); rm(t_pg2); rm(t_pg3); 
rm(t_pg4); rm(t_pg5);  rm(t_pg6)
rm(t_pg7); rm(t_pg8); rm(t_pg9); 
rm(t_pg10); rm(t_pg11);  rm(t_pg12)
rm(df)

# Arreglar el nombre del departamento

df_distCirc <- mapa_judicial %>% group_by(nom_distrito, nom_circuito) %>% 
               summarise(temp = n()) %>% arrange(nom_circuito)

nom_departamento  <- c("ANTIOQUIA", "META", "CESAR", "CALDAS", "ANTIOQUIA", "ANTIOQUIA", 
                                      "ANTIOQUIA", "CALDAS", "ANTIOQUIA", "RISARALDA", "ARAUCA", "QUINDÍO", 
                                      "CÓRDOBA", "CHOCÓ", "NARIÑO", "SANTANDER", "ATLÁNTICO", 
                                      "CAQUETÁ", "RISARALDA", "ANTIOQUIA", "BOGOTÁ, D. C.", 
                       "ANTIOQUIA",
                                      "CAUCA", "SANTANDER", "VALLE DEL CAUCA", "VALLE DEL CAUCA",
                                      "QUINDÍO", 
                                      "ANTIOQUIA", "VALLE DEL CAUCA", "CAUCA", "CUNDINAMARCA",
                                      "BOLÍVAR", "BOLÍVAR", 
                                      "VALLE DEL CAUCA", "ANTIOQUIA", "CÓRDOBA", 
                                      "TOLIMA", "SANTANDER", "CÓRDOBA", 
                                      "CÓRDOBA", "BOYACÁ", "CESAR", "CUNDINAMARCA", "MAGDALENA", 
                                      "SANTANDER", "ANTIOQUIA", "ANTIOQUIA", "SUCRE",
                                      "NORTE DE SANTANDER", "ANTIOQUIA", 
                                      "NARIÑO", "RISARALDA", "BOYACÁ", 
                                      "ANTIOQUIA", "MAGDALENA", 
                                      "BOYACÁ", "ANTIOQUIA", "TOLIMA", "CUNDINAMARCA", "CAQUETÁ", 
                                      "ANTIOQUIA", "TOLIMA", "ANTIOQUIA", "MAGDALENA", "CUNDINAMARCA",
                                      "CUNDINAMARCA", 
                                      "CUNDINAMARCA", "BOYACÁ", 
                                      "HUILA", "CUNDINAMARCA", "ANTIOQUIA", "META", 
                                      "CUNDINAMARCA", "TOLIMA", "CAUCA", "BOYACÁ", 
                                      "TOLIMA", "TOLIMA", "GUAINÍA", 
                                      "NARIÑO", "ANTIOQUIA", "CHOCÓ", "ANTIOQUIA", "ANTIOQUIA", "ANTIOQUIA", 
                                      "CALDAS", "CUNDINAMARCA", "CUNDINAMARCA", 
                                      "HUILA", "NARIÑO", "RISARALDA", 
                                      "TOLIMA", "AMAZONAS", "TOLIMA", "CÓRDOBA", "NORTE DE SANTANDER",
                                       "BOLÍVAR", 
                                      "LA GUAJIRA", "SANTANDER", "CALDAS", "CALDAS", "ANTIOQUIA", 
                                      "ANTIOQUIA", 
                                      "TOLIMA", "BOYACÁ", "VAUPÉS", "PUTUMAYO",
                                       "BOLÍVAR", "BOYACÁ", 
                                      "CÓRDOBA", "CÓRDOBA", "CASANARE", "HUILA", 
                                      "NORTE DE SANTANDER", "CASANARE", 
                                      "CUNDINAMARCA", "VALLE DEL CAUCA", "NORTE DE SANTANDER", 
                                        "NARIÑO", "CAUCA", "CASANARE", 
                                      "BOYACÁ", "CALDAS", "RISARALDA", "HUILA", "MAGDALENA", 
                                      "CÓRDOBA", "MAGDALENA", "CAUCA", "VICHADA", "META", 
                                      "SANTANDER", "PUTUMAYO", "ANTIOQUIA", "BOYACÁ", 
                                      "CAQUETÁ", "CAUCA", "TOLIMA", "CHOCÓ", "RISARALDA", 
                                      "BOYACÁ", "LA GUAJIRA", "ANTIOQUIA", 
                                      "CALDAS", "CHOCÓ",  "VALLE DEL CAUCA", 
                                      "ATLÁNTICO", "CÓRDOBA", "CALDAS", "NARIÑO",
                                      "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA", 
                                      "SANTANDER", "GUAVIARE", "LA GUAJIRA", "SUCRE", 
                                      "META", "ANTIOQUIA", "SANTANDER", 
                                      "ANTIOQUIA", "MAGDALENA",
                                      "ANTIOQUIA", "CAUCA", 
                                      "ANTIOQUIA", "ARAUCA", "ANTIOQUIA", "VALLE DEL CAUCA",
                                      "PUTUMAYO", "CAUCA", 
                                      "BOLÍVAR", "SUCRE", "SUCRE", "CUNDINAMARCA",
                                      "BOYACÁ", "BOYACÁ", "SANTANDER", 
                                      "BOYACÁ", "ATLÁNTICO", "ANTIOQUIA", "ANTIOQUIA", 
                                      "RISARALDA", 
                                      "ANTIOQUIA", "BOYACÁ", "SUCRE", "ANTIOQUIA", 
                                      "ANTIOQUIA", "VALLE DEL CAUCA", "NARIÑO", 
                                      "BOYACÁ", "NARIÑO", "BOLÍVAR", 
                                      "ANTIOQUIA", "CUNDINAMARCA", "ANTIOQUIA", "CESAR",
                                      "SANTANDER", "LA GUAJIRA", 
                                      "META", "CUNDINAMARCA", "ANTIOQUIA", 
                                      "ANTIOQUIA", "CASANARE", "CUNDINAMARCA")


df_distCirc$nom_departamento <- nom_departamento


 
mapa_judicial <- left_join(mapa_judicial, df_distCirc, by = c("nom_distrito", "nom_circuito"))
mapa_judicial$temp <- NULL
mapa_judicial <- mapa_judicial %>% select(nom_distrito, nom_circuito, nom_departamento, nom_mpio)



mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "CUBARÁ (BOY)"] <- "BOYACÁ"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "CÁCHIRA (NS)"] <- "NORTE DE SANTANDER"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "LA ESPERANZA (NS)"] <- "NORTE DE SANTANDER"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "SAN JOSE DEL PALMAR (CH)"] <- "CHOCÓ"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "GONZALEZ (CESAR)"] <- "CESAR"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "RIO DE ORO (CESAR)"] <- "CESAR"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "BELTRAN (CUND)"] <- "CUNDINAMARCA"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "GUAYABETAL (CUND)"] <- "CUNDINAMARCA"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "PTO SALGAR (CUND)"] <- "CUNDINAMARCA"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "PIAMONTE (CAUCA)"] <- "CAUCA"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "MEDINA (CUN)"] <- "CUNDINAMARCA"
mapa_judicial$nom_departamento[mapa_judicial$nom_mpio == "PARATEBUENO (CUND)"] <- "CUNDINAMARCA"


mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "CUBARÁ (BOY)"] <- "CUBARÁ"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "CÁCHIRA (NS)"] <- "CÁCHIRA (NS)"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "LA ESPERANZA (NS)"] <- "LA ESPERANZA (NS)"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "SAN JOSE DEL PALMAR (CH)"] <- "SAN JOSE DEL PALMAR"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "GONZALEZ (CESAR)"] <- "GONZALEZ"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "RIO DE ORO (CESAR)"] <- "RIO DE ORO"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "BELTRAN (CUND)"] <- "BELTRAN"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "GUAYABETAL (CUND)"] <- "GUAYABETAL"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "PTO SALGAR (CUND)"] <- "PTO SALGAR"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "PIAMONTE (CAUCA)"] <- "PIAMONTE"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "MEDINA (CUN)"] <- "MEDINA"
mapa_judicial$nom_mpio[mapa_judicial$nom_mpio == "PARATEBUENO (CUND)"] <- "PARATEBUENO"


mapa_judicial <- mapa_judicial %>% left_join(unique(df_divipola[c("Código Departamento", 
                                                                  "nom_departamento")]), 
                                             by = "nom_departamento")
names(mapa_judicial)[names(mapa_judicial) == "Código Departamento"] <- "cdg_dpto" 



mapa_judicial[mapa_judicial$nom_mpio == "LA CALERA",]$nom_departamento <- "CUNDINAMARCA"
mapa_judicial[mapa_judicial$nom_mpio == "LA CALERA",]$cdg_dpto <- 25

mapa_judicial[mapa_judicial$nom_mpio == "PALESTINA",]$nom_departamento <- "CALDAS"
mapa_judicial[mapa_judicial$nom_mpio == "PALESTINA",]$cdg_dpto <- 17

mapa_judicial[mapa_judicial$nom_mpio == "CHINCHINA",]$nom_departamento <- "CALDAS"
mapa_judicial[mapa_judicial$nom_mpio == "CHINCHINA",]$cdg_dpto <- 17

mapa_judicial[mapa_judicial$nom_mpio == "PAJARITO",]$nom_departamento <- "BOYACÁ"
mapa_judicial[mapa_judicial$nom_mpio == "PAJARITO",]$cdg_dpto <- 15

mapa_judicial[mapa_judicial$nom_mpio == "PTO CONCORDIA",]$nom_departamento <- "META"
mapa_judicial[mapa_judicial$nom_mpio == "PTO CONCORDIA",]$cdg_dpto <- 50
mapa_judicial[mapa_judicial$nom_mpio == "PTO CONCORDIA",]$nom_mpio <- "PUERTO CONCORDIA"

setwd(ruta)
setwd("scripts")
source("f_busquedaMpioDivipola.r", encoding = "UTF-8")

# Busqueda de las cadenas de texto en cada municipio del nombre de la DIVIPOLA:
vctr_dpto <- sort(unique(mapa_judicial$cdg_dpto))
lst_nomMpioArreglado <- vector(mode = "list", length = length(vctr_dpto))
lst_nomMpioOrigMapaJud <- vector(mode = "list", length = length(vctr_dpto))
names(lst_nomMpioArreglado) <- vctr_dpto 
names(lst_nomMpioOrigMapaJud) <- vctr_dpto 


for(i in seq_along(vctr_dpto)){
  
  vctr_mpio_dptoDivipola <- sort(df_divipola$nom_mpio[df_divipola$`Código Departamento` == vctr_dpto[i]])
  vctr_mpio_MapaJudicial <- sort(mapa_judicial$nom_mpio[mapa_judicial$cdg_dpto == vctr_dpto[i]])
  
  lst_nomMpioOrigMapaJud[[i]] <-  vctr_mpio_MapaJudicial
  lst_nomMpioArreglado[[i]] <-  f_busquedaMun(vctrMpio = vctr_mpio_MapaJudicial,
                                              vctrDivipolaMpio = vctr_mpio_dptoDivipola)
}
# Los nombres de los municipios según la DIVIPOA
vctr_rep <- unlist(lapply(lst_nomMpioOrigMapaJud, function(x) length(x)))
df_dptoMapaJudDivip <- data.frame(cdg_dpto = rep(vctr_dpto, vctr_rep), 
                                  nomMpioOrigMapaJud = as.character(unlist(lst_nomMpioOrigMapaJud)), 
                                  nomMpioArreglado = as.character(unlist(lst_nomMpioArreglado)))


# Se identifican los municipios que no se imputan adecuadamente:
# a <- df_dptoMapaJudDivip[is.na(df_dptoMapaJudDivip$nomMpioArreglado),]

df_nomArreglados <- tibble::tribble(
  ~cdg_dpto,        ~nomMpioOrigMapaJud,                                          ~nomMpioArregladoR,
  "05",                  "BOLIVAR",                                           "CIUDAD BOLÍVAR",
  "05",                 "EL PEÑOL",                                                    "PEÑOL",
  "05",                "EL RETIRO",                                                   "RETIRO",
  "05",                 "NECLOQUI",                                                 "NECOCLÍ",
  "05",               "PTO BERRIO",                                            "PUERTO BERRÍO",
  "05",                 "PTO NARE",                                              "PUERTO NARE",
  "13",                "CARTAGENA",                                      "CARTAGENA DE INDIAS",
  "13",                   "MOMPOX",                                     "SANTA CRUZ DE MOMPOX",
  "15",                 "CUIENAGA",                                                  "CIÉNEGA",
  "15",                   "GUICAN",                                      "GÜICÁN DE LA SIERRA",
  "19",                 "COCONUCO",                                                   "PURACÉ",
  "19",          "PAEZ BELALCAZAR",                                                     "PÁEZ",
  "19",           "PATIA EL BORDO",                                                    "PATÍA",
  "19",                 "PIENDAMO",                                         "PIENDAMÓ - TUNÍA",
  "19",                 "STA ROSA",                                               "SANTA ROSA",
  "20",          "LA PAZ (ROBLES)",                                                   "LA PAZ",
  "20",                  "MANAURE",                                 "MANAURE BALCÓN DEL CESAR",
  "23",           "PTO LIBERTADOR",                                        "PUERTO LIBERTADOR",
  "23",                 "PURISIMA",                                "PURÍSIMA DE LA CONCEPCIÓN",
  "25", "S ANTONIO DEL TEQUENDAMA",                               "SAN ANTONIO DEL TEQUENDAMA",
  "25",                    "UBATE",                              "VILLA DE SAN DIEGO DE UBATÉ",
  "27",          "BELEN DE BAJIRA",                                                 "RIOSUCIO",
  "27",      "CANTON DE SAN PABLO",                                  "EL CANTÓN DEL SAN PABLO",
  "27",      "LITORAL DE SAN JUAN",                                  "EL LITORAL DEL SAN JUAN",
  "41",                "ARGENTINA",                                                "LA ARGENTINA",
  "44",          "JAGUA DEL PILAR",                                       "LA JAGUA DEL PILAR",
  "50",                "PTO LOPEZ",                                             "PUERTO LÓPEZ",
  "52",           "ALBAN SAN JOSE",                                                    "ALBÁN",
  "52",                  "CUASPUD",                                        "CUASPUD CARLOSAMA",
  "52",              "MAGUI PAYAN",                                                    "MAGÜÍ",
  "52",     "STA BARBARA ISCUANDE",                                            "SANTA BÁRBARA",
  "52",                   "TUMACO",                                     "SAN ANDRÉS DE TUMACO",
  "54",                  "TEOREMA",                                                  "TEORAMA",
  "66",        "STA ROSA DE CABAL",                                      "SANTA ROSA DE CABAL",
  "68",                "EL CARMEN",                                     "EL CARMEN DE CHUCURÍ",
  "68",                   "ENSISO",                                                   "ENCISO",
  "68",              "JORDAN SUBE",                                                   "JORDÁN",
  "68",                "PTO PARRA",                                             "PUERTO PARRA",
  "68",              "STA BARBARA",                                            "SANTA BÁRBARA",
  "68",       "STA ELENA DEL OPON",                                    "SANTA HELENA DEL OPÓN",
  "70",                "TOLUVIEJO",                                    "SAN JOSÉ DE TOLUVIEJO",
  "73",         "GUAYABAL (ARMERO",                                                   "ARMERO",
  "73",                "MARIQUITA",                               "SAN SEBASTIÁN DE MARIQUITA",
  "73",               "STA ISABEL",                                             "SANTA ISABEL",
  "76",                     "BUGA",                                      "GUADALAJARA DE BUGA",
  "86",              "PTO CAICEDO",                                           "PUERTO CAICEDO",
  "86",               "PTO GUZMAN",                                            "PUERTO GUZMÁN",
  "86",             "PTO LEGIZAMO",                                         "PUERTO LEGUÍZAMO",
  "88",          "ARCH SAN ANDRES",                                         "SAN ANDRÉS",
  "99",              "PTO CARREÑO",                                           "PUERTO CARREÑO",
  "99",               "SANTA RITA",                                                 "CUMARIBO",
  "99",              "STA ROSALIA",                                            "SANTA ROSALÍA"
)


# Se arreglan esos municipios
df_dptoMapaJudDivip <- left_join(df_dptoMapaJudDivip, df_nomArreglados, by = c("cdg_dpto", "nomMpioOrigMapaJud"))
df_dptoMapaJudDivip$nomMpioArreglado <- ifelse(is.na(df_dptoMapaJudDivip$nomMpioArreglado),
                                               df_dptoMapaJudDivip$nomMpioArregladoR, df_dptoMapaJudDivip$nomMpioArreglado)
df_dptoMapaJudDivip$nomMpioArregladoR <- NULL
names(df_dptoMapaJudDivip)[names(df_dptoMapaJudDivip) == "nomMpioArreglado" ] <- "nom_mpio"
names(df_dptoMapaJudDivip)[names(df_dptoMapaJudDivip) == "cdg_dpto" ] <- "Código Departamento"

DF_DPTOMJDIVIPOLA <- left_join(df_dptoMapaJudDivip, df_divipola, by = c("Código Departamento", "nom_mpio"))
DF_DPTOMJDIVIPOLA <- select(DF_DPTOMJDIVIPOLA,`Código Departamento`, `Código Municipio`, nom_departamento, nom_mpio, nomMpioOrigMapaJud )

#DF_DPTOMJDIVIPOLA$dup <- as.numeric(duplicated(DF_DPTOMJDIVIPOLA$`Código Municipio`) | #duplicated(DF_DPTOMJDIVIPOLA$`Código Municipio`, fromLast =T))

# Manualmente se arreglan los últimos errores
# Arreglos manuales
# PALESTINA HUILA
pos1 <- which(DF_DPTOMJDIVIPOLA$nomMpioOrigMapaJud == "PALESTINA")[1]
DF_DPTOMJDIVIPOLA$`Código Departamento`[pos1] <- "41"
DF_DPTOMJDIVIPOLA$`Código Municipio`[pos1] <- "41530"
DF_DPTOMJDIVIPOLA$nom_departamento[pos1] <- "HUILA" 

# BELEN DE BAJIRÁ
# Este municipio no está en la Divipola
pos2 <- which(DF_DPTOMJDIVIPOLA$nomMpioOrigMapaJud == "BELEN DE BAJIRA")[1]
DF_DPTOMJDIVIPOLA$`Código Departamento`[pos2] <- "27"
DF_DPTOMJDIVIPOLA$`Código Municipio`[pos2] <- "27086"
DF_DPTOMJDIVIPOLA$nom_departamento[pos2] <- "CHOCÓ" 

# EL TABLON NARIÑO
pos3 <- which(DF_DPTOMJDIVIPOLA$nomMpioOrigMapaJud == "EL TABLON")[1]
DF_DPTOMJDIVIPOLA$`Código Departamento`[pos3] <- "52"
DF_DPTOMJDIVIPOLA$`Código Municipio`[pos3] <- "52258"
DF_DPTOMJDIVIPOLA$nom_departamento[pos3] <- "NARIÑO" 
DF_DPTOMJDIVIPOLA$nom_mpio[pos3] <- "EL TABLÓN DE GÓMEZ" 

# STA CRUZ NARIÑO
pos4 <- which(DF_DPTOMJDIVIPOLA$nomMpioOrigMapaJud == "STA CRUZ")[1]
DF_DPTOMJDIVIPOLA$`Código Departamento`[pos4] <- "52"
DF_DPTOMJDIVIPOLA$`Código Municipio`[pos4] <- "52699"
DF_DPTOMJDIVIPOLA$nom_departamento[pos4] <- "NARIÑO" 
DF_DPTOMJDIVIPOLA$nom_mpio[pos4] <- "SANTACRUZ" 

# SINCE SUCRE
pos5 <- which(DF_DPTOMJDIVIPOLA$nomMpioOrigMapaJud == "SINCE")[1]
DF_DPTOMJDIVIPOLA$`Código Departamento`[pos5] <- "70"
DF_DPTOMJDIVIPOLA$`Código Municipio`[pos5] <- "70742"
DF_DPTOMJDIVIPOLA$nom_departamento[pos5] <- "SUCRE" 
DF_DPTOMJDIVIPOLA$nom_mpio[pos5] <- "SAN LUIS DE SINCÉ" 

#DF_DPTOMJDIVIPOLA <- DF_DPTOMJDIVIPOLA[!duplicated(DF_DPTOMJDIVIPOLA$`Código Municipio`),]

# prueba$nomMpioOrigMapaJud[prueba$nom_mpio == "PURACÉ"] <- "COCONUCO - PURACE"
# prueba$nomMpioOrigMapaJud[prueba$nom_mpio == "CUMARIBO"] <- "SANTA RITA - CUMARIBO"
DF_DPTOMJDIVIPOLA$dup <- as.numeric(duplicated(DF_DPTOMJDIVIPOLA$`Código Municipio`) | duplicated(DF_DPTOMJDIVIPOLA$`Código Municipio`, fromLast = T))
DF_DPTOMJDIVIPOLA$dup <- NULL

mapa_judicial$nom_departamento <- NULL
names(mapa_judicial)[names(mapa_judicial) == "cdg_dpto"] <- "Código Departamento"
names(mapa_judicial)[names(mapa_judicial) == "nom_mpio"] <- "nomMpioOrigMapaJud"

mapa_judicial$dup <- as.numeric(duplicated(mapa_judicial$nomMpioOrigMapaJud ) | duplicated(mapa_judicial$nomMpioOrigMapaJud , fromLast = T))
mapa_judicial <- arrange(mapa_judicial, desc(dup), nomMpioOrigMapaJud)
mapa_judicial$`Código Departamento`[mapa_judicial$nomMpioOrigMapaJud == "PALESTINA" & 
                                      mapa_judicial$nom_distrito == "NEIVA"] <- 41
mapa_judicial$dup <- NULL


# intersect(names(DF_DPTOMJDIVIPOLA), names(mapa_judicial))

MAPAJUDICIAL <- mapa_judicial %>% left_join(DF_DPTOMJDIVIPOLA, by = c("Código Departamento", "nomMpioOrigMapaJud"))
MAPAJUDICIAL <- MAPAJUDICIAL[c("nom_distrito", "nom_circuito", "Código Departamento", 
                               "Código Municipio", "nom_departamento", "nom_mpio", "nomMpioOrigMapaJud")]
#table(duplicated(prueba$`Código Municipio`))
MAPAJUDICIAL$dup <- as.numeric(duplicated(MAPAJUDICIAL$`Código Municipio`) | duplicated(MAPAJUDICIAL$`Código Municipio`, fromLast = T))


MAPAJUDICIAL <- MAPAJUDICIAL %>% filter(!(nomMpioOrigMapaJud %in% c("SANTA RITA", "COCONUCO")))
MAPAJUDICIAL$nomMpioOrigMapaJud[MAPAJUDICIAL$nom_mpio == "PURACÉ"] <- "COCONUCO - PURACE"
MAPAJUDICIAL$nomMpioOrigMapaJud[MAPAJUDICIAL$nom_mpio == "CUMARIBO"] <- "SANTA RITA - CUMARIBO"
MAPAJUDICIAL$dup <- NULL
MAPAJUDICIAL <- MAPAJUDICIAL %>% arrange(nom_distrito, nom_circuito, `Código Municipio` )



setwd("..")
setwd("salidas")
write_xlsx(MAPAJUDICIAL, "MAPAJUDICIAL.xlsx")
saveRDS(MAPAJUDICIAL, "MAPAJUDICIAL.rds")
