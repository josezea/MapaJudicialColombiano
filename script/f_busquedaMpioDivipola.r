

f_busquedaMun <- function(vctrMpio, vctrDivipolaMpio, umbral = 0.15) {
library(stringi)
vctr_Mpio <- stri_trans_general(str = vctrMpio, 
                                     id = "Latin-ASCII")

vctr_divipolaMpio <- stri_trans_general(str = vctrDivipolaMpio, 
                                              id = "Latin-ASCII")

# Cuantificar el número de municipios que se va a buscar en la DIVIPOLA
n_mpios <- length(vctr_Mpio)  

ls_mpio_encontrado <- vector(mode = "list", length = n_mpios)
ls_distanciasMenores <- vector(mode = "list", length = n_mpios)

for(i in 1:n_mpios){
mpio_buscado <- vctr_Mpio[i]

distancias  <- stringdist::stringdist(mpio_buscado, method = "jw",
                                           b = vctr_divipolaMpio)
ls_distanciasMenores[[i]]  <- min(distancias)
ls_mpio_encontrado[[i]] <- ifelse(min(distancias) <= umbral & 
                                    length(min(distancias)) == 1,
                                  vctrDivipolaMpio[which(distancias == min(distancias))],
                          NA)
}
# salida <- list(distanciaMun = ls_distanciasMenores,
#                municpio_econtrado = unlist(ls_mpio_encontrado))
salida <-  unlist(ls_mpio_encontrado)
return(salida)
}


# datos <- mapa_judicial %>% filter(nom_departamento == nom_dptos[1])
# mpios_divipola <- df_divipola %>% filter(nom_departamento == nom_dptos[1])


# vctrMpio <- c("ABEJORRAL", "AMAGA", "AMALFI", "ANORI", "ANDES", "BETANIA", 
#               "HISPANIA", "JARDIN", "APARTADO", "CAREPA", "CHIGORODO", "MURINDO", 
#               "MUTATA", "VIGIA DEL FUERTE", "BOLIVAR", "SALGAR", "CAUCASIA", 
#               "CACERES", "NECHI", "TARAZA", "CISNEROS", "CAROLINA", "GOMEZ PLATA", 
#               "GUADALUPE", "SAN ROQUE", "SANTO DOMINGO", "CONCORDIA", "BETULIA", 
#               "DABEIBA", "PEQUE", "URAMITA", "EL BAGRE", "ZARAGOZA", "SANTUARIO", 
#               "COCORNA", "GRANADA", "PUERTO TRIUNFO", "SAN FRANCISCO", "SAN LUIS", 
#               "FREDONIA", "VENECIA", "FRONTINO", "ABRIAQUI", "CAÑASGORDAS", 
#               "ITUANGO", "SAN ANDRES DE CUERQUIA", "JERICO", "PUEBLO RICO", 
#               "TARSO", "LA CEJA", "LA UNION", "EL RETIRO", "MARINILLA", "GUATAPÉ", 
#               "EL PEÑOL", "SAN CARLOS", "SAN RAFAEL", "PTO BERRIO", "CARACOLI", 
#               "MACEO", "PTO NARE", "YONDÓ", "RIONEGRO", "ALJENADRIA", "CARMEN DE VIBORAL", 
#               "CONCEPCION", "GUARNE", "SAN VICENTE", "SAN PEDRO DE LOS MILAGROS", 
#               "BELMIRA", "ENTRERRIOS", "SANTA BARBARA", "LA PINTADA", "MONTEBELLO", 
#               "SANTAFE DE ANTIOQUIA", "ANZÁ", "BURITICA", "CAICEDO", "EBEJICO", 
#               "GIRALDO", "SAN JERONIMO", "STA ROSA DE OSOS", "DON MATIAS", 
#               "SAN JOSE DE LA MONTAÑA", "TOLEDO", "SEGOVIA", "REMEDIOS", "SONSON", 
#               "ARGELIA", "NARIÑO", "SOPETRAN", "LIBORINA", "OLAYA", "SABANALARGA", 
#               "TAMESIS", "CARAMANTA", "VALPARAISO", "TITIRIBI", "TURBO", "ARBOLETES", 
#               "NECLOQUI", "SAN JUAN DE URABA", "SAN PEDRO DE URABA", "URRAO", 
#               "YARUMAL", "ANGOSTURA", "BRICEÑO", "CAMPAMENTO", "VALDIVIA", 
#               "YOLOMBO", "VEGACHÍ", "YALÍ", "MEDELLIN", "BELLO", "CALDAS", 
#               "ANGELOPOLIS", "ENVIGADO", "SABANETA", "GIRARDOTA", "BARBOSA", 
#               "COPACABANA", "ITAGÜI", "ARMENIA", "HELICONIA", "LA ESTRELLA"
# )
# 
# vctrDivipolaMpio <- c("MEDELLÍN", "ABEJORRAL", "ABRIAQUÍ", "ALEJANDRÍA", "AMAGÁ", 
#                       "AMALFI", "ANDES", "ANGELÓPOLIS", "ANGOSTURA", "ANORÍ", "SANTA FÉ DE ANTIOQUIA", 
#                       "ANZÁ", "APARTADÓ", "ARBOLETES", "ARGELIA", "ARMENIA", "BARBOSA", 
#                       "BELMIRA", "BELLO", "BETANIA", "BETULIA", "CIUDAD BOLÍVAR", "BRICEÑO", 
#                       "BURITICÁ", "CÁCERES", "CAICEDO", "CALDAS", "CAMPAMENTO", "CAÑASGORDAS", 
#                       "CARACOLÍ", "CARAMANTA", "CAREPA", "EL CARMEN DE VIBORAL", "CAROLINA", 
#                       "CAUCASIA", "CHIGORODÓ", "CISNEROS", "COCORNÁ", "CONCEPCIÓN", 
#                       "CONCORDIA", "COPACABANA", "DABEIBA", "DONMATÍAS", "EBÉJICO", 
#                       "EL BAGRE", "ENTRERRÍOS", "ENVIGADO", "FREDONIA", "FRONTINO", 
#                       "GIRALDO", "GIRARDOTA", "GÓMEZ PLATA", "GRANADA", "GUADALUPE", 
#                       "GUARNE", "GUATAPÉ", "HELICONIA", "HISPANIA", "ITAGÜÍ", "ITUANGO", 
#                       "JARDÍN", "JERICÓ", "LA CEJA", "LA ESTRELLA", "LA PINTADA", "LA UNIÓN", 
#                       "LIBORINA", "MACEO", "MARINILLA", "MONTEBELLO", "MURINDÓ", "MUTATÁ", 
#                       "NARIÑO", "NECOCLÍ", "NECHÍ", "OLAYA", "PEÑOL", "PEQUE", "PUEBLORRICO", 
#                       "PUERTO BERRÍO", "PUERTO NARE", "PUERTO TRIUNFO", "REMEDIOS", 
#                       "RETIRO", "RIONEGRO", "SABANALARGA", "SABANETA", "SALGAR", "SAN ANDRÉS DE CUERQUÍA", 
#                       "SAN CARLOS", "SAN FRANCISCO", "SAN JERÓNIMO", "SAN JOSÉ DE LA MONTAÑA", 
#                       "SAN JUAN DE URABÁ", "SAN LUIS", "SAN PEDRO DE LOS MILAGROS", 
#                       "SAN PEDRO DE URABÁ", "SAN RAFAEL", "SAN ROQUE", "SAN VICENTE FERRER", 
#                       "SANTA BÁRBARA", "SANTA ROSA DE OSOS", "SANTO DOMINGO", "EL SANTUARIO", 
#                       "SEGOVIA", "SONSÓN", "SOPETRÁN", "TÁMESIS", "TARAZÁ", "TARSO", 
#                       "TITIRIBÍ", "TOLEDO", "TURBO", "URAMITA", "URRAO", "VALDIVIA", 
#                       "VALPARAÍSO", "VEGACHÍ", "VENECIA", "VIGÍA DEL FUERTE", "YALÍ", 
#                       "YARUMAL", "YOLOMBÓ", "YONDÓ", "ZARAGOZA")
# 
# 
# 
# datos$nom_mpioDivipola <- f_busquedaMun(vctrMpio = datos$nom_mpio, 
#                                         vctrDivipolaMpio = mpios_divipola$nom_mpio, 0.1)

