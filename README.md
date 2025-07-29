
RANDOM FOREST PARA CORRECCIÓN DE PRECIPITACIÓN ANUAL DE GCM SD Y BC DISPONIBLES EN:

Vasquez, Nicolas; Mendoza, Pablo A., 2024, "Statistically downscaled and bias corrected CMIP6 models for Continental Chile under scenario SSP5-8.5", https://doi.org/10.7910/DVN/O3YBOT, Harvard Dataverse, V1

1. Se obtienen las estaciones de precipitación a partir de la base de datos del CR2 (CR2, 2019) para las regiones I a III. Se seleccionan estaciones con código DGA que cuenten con más de 20 años de datos.
Se estudia el período comprendido entre los años hidrológicos 1970 y 2020, dividido en subperíodos de 10 años hidrológicos.

2. Se obtiene un PCA a partir de P_anual_GCM según año hidrológico de cada GCM en estaciones

![Análisis de PCA](https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PCA.png)

3. Se obtienen las componentes principales que explican la proporción de varianza acumulada requerida.
  
4. Se obtienen la latitud, longitud, altitud, distancia a la costa, pendiente y aspecto para cada estación.
  
6. Se ajusta un modelo Random Forest (RF) usando las primeras 10 componentes principales y los atributos descritos anteriormente. Se prueban condiciones desfavorables para evaluar la capacidad de generalización espaciotemporal de los modelos RF ajustados.

8. En la Figura se muestra un modelo RF ajustado segun el 40% de las estaciones del periodo P1 (1970-1980). Se ajusta el modelo RF segun los atributos y variables mencionadas y luego se evalua la prediccion segun el coeficiente de determinacion de los anuales y
el porcentaje de sesgo entre las precipitaciones anuales observadas y las corregidas. Las estaciones de testeo para cada periodo no se usaron en el entrenamiento para evaluar la robustes del modelo en espacios (estaciones) y tiempos (periodos) distintos a los usados en entrenamiento (generalizacion espaciotemporal).








