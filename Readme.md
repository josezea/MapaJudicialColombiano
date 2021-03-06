Mapa Judicial Colombiano
================

El mapa judicial colombiana está disponible en
<https://www.ramajudicial.gov.co/web/informacion/mapa-judicial-de-colombia>

-   Sin embargo, esta información no está dispuesta en una hoja de
    cálculo estructurada y además no se dispone el código de división
    política adminsitrativa (DIVIPOLA,
    <https://geoportal.dane.gov.co/geovisores/territorio/consulta-divipola-division-politico-administrativa-de-colombia/>)
    de los municipios y departamentos para poder integrar con otros
    sistemas de información

Se leerá cada uno de las hojas del pdf y se estructura la información,
para localizar el área se usa una aplicación en shiny que permiten
delimitar los rangos:

``` r
locate_areas("MAPA JUDICIAL Detallado.pdf", pages = 1) 
```

-   Se llega a cabo una lectura programática del archivo en pdf del mapa
    judicial utilizando el paquete tabulizer de y posteriormente se
    integra la información de los circuitos con la de la DIVIPOLA. Este
    proceso implica usar algoritmos de similitud de caracteres,
    expresiones regulares y tratamiento manual para corregir algunas
    inconsistencias resultantes al aplicar los algoritmos:

-   Al mapa judiicial colombiano se le corrigen los nombres de los
    municipios, se le agrega los códigos de la división política
    administrativa. El mama judicial resulta con un total hay 1102
    municipios que corresponden a los 1122 municipios del país sin los
    18 de zonas no municipalizadas, por otro lado COCONUCO y PURACE
    corrresponden al municipio de PURACÉ, SANTA RITA y CUMARIBO
    corresponden al municipio de CUMARIBO y se incluye los códigos de la
    DIVIPOLA de BELEN DE BAJIRA que es un municipio nuevo en el Chocó.

-   Los archivos pueden consultarse en la carpeta salidas del
    repositorios en R y en excel (MAPAJUDICIAL.xlsx y MAPAJUDICIAL.R).
