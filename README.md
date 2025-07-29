
RANDOM FOREST PARA CORRECCIÓN DE PRECIPITACIÓN ANUAL DE GCM SD Y BC. GCM DISPONIBLES EN:

Vasquez, Nicolas; Mendoza, Pablo A., 2024, "Statistically downscaled and bias corrected CMIP6 models for Continental Chile under scenario SSP5-8.5", https://doi.org/10.7910/DVN/O3YBOT, Harvard Dataverse, V1.

1. Se obtuvieron las estaciones de precipitación a partir de la base de datos del CR2 (CR2, 2019) para las regiones I a III. Se seleccionaron estaciones con código DGA que cuenten con más de 20 años de datos.
Se estudio el período comprendido entre los años hidrológicos 1970 y 2020, dividido en subperíodos de 10 años hidrológicos.

2. Se obtuvo un PCA a partir de P_anual_GCM según año hidrológico de cada GCM en estaciones. La precipitación anual GCM se obtuvo con la latitud y longitud de cada estacion y el pixel que contiene a la estación (ver **Figura 1**).

![Figura 1 - PCA](https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PCA.png)

3. Se asignaron a cada estación las componentes principales que explican la proporción de varianza acumulada requerida. Para este caso se consideraron 10 componentes principales.
  
4. Se obtuvo la latitud, longitud, altitud, distancia a la costa, pendiente y aspecto para cada estación.

5. Se ajusta un modelo Random Forest (RF) usando las primeras 10 componentes principales y los atributos descritos anteriormente. Se prueban condiciones desfavorables para evaluar la capacidad de generalización espaciotemporal de los modelos RF ajustados.

6. En la figura se muestra un modelo RF ajustado según el 40 % de las estaciones del período P1 (1970–1980). Se ajusta el modelo RF según los atributos y variables mencionadas y luego se evalúa la predicción según el coeficiente de determinación de los anuales y el porcentaje de sesgo entre las precipitaciones anuales observadas y las corregidas. Las estaciones de testeo para cada período no se usaron en el entrenamiento para evaluar la robustez del modelo en espacios (estaciones) y tiempos (períodos) distintos a los usados en entrenamiento (generalización espaciotemporal). Se muestra un GCM sin corrección para las estaciones de testeo.

![Panel_scatterplot1 ](https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PANEL_SCATTERPLOT1.png)

7. Se observa en la figura que para una validación espacial en P1 se obtienen porcentajes de sesgo y coeficientes de determinacion muy buenos. Para el resto de los periodos (validacion espaciotemporal) se observa que empeoran estas metricas pero ambas en un rango aceptable. Se muestra para los periodos P2, P3, P4 y P5 un porcentaje de sesgo menor que el GCM de referencia.

8. Para probar el modelo ajustado a distintos periodos se consideran las mismas estaciones






