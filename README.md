
RANDOM FOREST PARA CORRECCIÓN DE PRECIPITACIÓN ANUAL DE GCM SD Y BC. GCM DISPONIBLES EN:

Vasquez, Nicolas; Mendoza, Pablo A., 2024, "Statistically downscaled and bias corrected CMIP6 models for Continental Chile under scenario SSP5-8.5", https://doi.org/10.7910/DVN/O3YBOT, Harvard Dataverse, V1.

1. Se obtuvieron las estaciones de precipitación a partir de la base de datos del CR2 (CR2, 2019) para las regiones I a III. Se seleccionaron estaciones con código DGA que cuenten con más de 20 años de datos.
Se estudio el período comprendido entre los años hidrológicos 1970 y 2020, dividido en subperíodos de 10 años hidrológicos.

2. Se obtuvo un PCA que se muestra en la **Figura 1** partir de P_GCM_anual según año hidrológico de cada GCM en estaciones. La precipitación anual GCM se obtuvo con la latitud y longitud de cada estacion y el pixel que contiene a la estación.

![Figura 1 - PCA](https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PCA.png)
**Figura 1**: PCA para variable P_GCM_anual y la varianza explicada acomulada por componente.

3. Se asignaron a cada estación las componentes principales que explican la proporción de varianza acumulada requerida. Para este caso se consideraron 10 componentes principales.
  
4. Se obtuvo la latitud, longitud, altitud, distancia a la costa, pendiente y aspecto para cada estación.

5. Se ajusto un modelo Random Forest (RF) usando las primeras 10 componentes principales y los atributos descritos anteriormente. Se probaron condiciones desfavorables para evaluar la capacidad de generalización espaciotemporal de los modelos RF ajustados.

6. En la **Figura 2** se muestra un modelo RF ajustado según el 40 % de las estaciones del período P1 (1970–1980). Se ajusto el modelo RF según los atributos y variables mencionadas y luego se evaluo la predicción según el coeficiente de determinación de los anuales y el porcentaje de sesgo entre las precipitaciones anuales observadas y las corregidas. Las estaciones de testeo para cada período no se usaron en el entrenamiento para evaluar la robustez del modelo en espacios (estaciones) y tiempos (períodos) distintos a los usados en entrenamiento (generalización espaciotemporal). Se muestra un GCM sin corrección para las estaciones de testeo.

![Panel_scatterplot1 ](https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PANEL_SCATTERPLOT1.png)
**Figura 2**: Panel de graficos de dispersion para P_GCM_anual sin correccion con RF y para P_GCM_anual_corr. P1 validacion espacial (1970-1980). P2-P5 validacion espaciotemporal (1980 - 2020). 


7. Se observa en la figura que para una validación espacial en P1 se obtienen porcentajes de sesgo y coeficientes de determinacion muy buenos. Para el resto de los periodos (validacion espaciotemporal) se observa que empeoran estas metricas pero ambas en un rango aceptable. Se muestra para los periodos P2, P3, P4 y P5 un porcentaje de sesgo menor que el GCM de referencia.

8. Para probar el modelo ajustado a distintos periodos de entrenamiento se consideran las mismas estaciones y se ajusta un RF para cada periodo y se evalua en los demas sin considerar el periodo de entrenamiento. Para probar el efecto de distintas estaciones en entrenamiento se generaron 1000 iteraciones del experimento anterior pero cada 






