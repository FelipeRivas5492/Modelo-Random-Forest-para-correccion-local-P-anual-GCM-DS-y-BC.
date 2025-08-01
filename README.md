RANDOM FOREST PARA CORRECCIÓN LOCAL DE PRECIPITACIÓN ANUAL DE GCM SD Y BC. GCM DISPONIBLES EN:

Vasquez, Nicolas; Mendoza, Pablo A., 2024, "Statistically downscaled and bias corrected CMIP6 models for Continental Chile under scenario SSP5-8.5", https://doi.org/10.7910/DVN/O3YBOT, Harvard Dataverse, V1.

<p align="justify">
1. Se obtuvieron las estaciones de precipitación a partir de la base de datos del CR2 (CR2, 2019) para las regiones I a III. Se seleccionaron estaciones con código DGA que cuenten con más de 20 años de datos.  
Se estudió el período comprendido entre los años hidrológicos 1970 y 2020, dividido en subperíodos de 10 años hidrológicos.
</p>

<p align="justify">
2. Se obtuvo un PCA que se muestra en la <strong>Figura 1</strong> a partir de P_GCM_anual según año hidrológico de cada GCM en estaciones. La precipitación anual GCM se obtuvo con la latitud y longitud de cada estación y el píxel que contiene a la estación.
</p>

<div align="center">
  <img src="https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PCA.png" alt="Figura 1 - PCA">
</div>
<p><strong>Figura 1</strong>: PCA para variable P_GCM_anual y la varianza explicada acomulada por componente.</p>

<p align="justify">
3. Se asignaron a cada estación las componentes principales que explican la proporción de varianza acumulada requerida. Para este caso se consideraron 10 componentes principales.
</p>

<p align="justify">
4. Se obtuvo la latitud, longitud, altitud, distancia a la costa, pendiente y aspecto para cada estación con un DEM.
</p>

<p align="justify">
5. Se ajustó un modelo Random Forest (RF) usando las primeras 10 componentes principales y los atributos descritos anteriormente. Se probaron condiciones desfavorables para validar la capacidad de generalización espaciotemporal de los modelos RF ajustados y se usaron las métricas de coeficiente de determinación y porcentaje de sesgo entre observados y corregidos (R2 y PBIAS) para evaluar esta capacidad.
</p>

<p align="justify">
6. En la <strong>Figura 2</strong> se muestra un modelo RF ajustado según el 40 % de las estaciones del período P1 (1970–1980). Se ajustó el modelo RF según los atributos y variables mencionadas y luego se evaluó la predicción según el coeficiente de determinación de los anuales y el porcentaje de sesgo entre las precipitaciones anuales observadas y las corregidas. Las estaciones de testeo para cada período no se usaron en el entrenamiento para evaluar la robustez del modelo en espacios (estaciones) y tiempos (períodos) distintos a los usados en entrenamiento (generalización espaciotemporal). Se muestra un GCM sin corrección para las estaciones de testeo.
</p>

<div align="center">
  <img src="https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PANEL_SCATTERPLOT1.png" alt="Panel_scatterplot1">
</div>
<p><strong>Figura 2</strong>: Panel de gráficos de dispersión para P_GCM_anual sin corrección con RF y para P_GCM_anual_corr. P1: validación espacial (1970–1980). P2–P5: validación espaciotemporal (1980–2020).</p>

<p align="justify">
7. Se observa en la figura que, para una validación espacial en P1, se obtienen porcentajes de sesgo y coeficientes de determinación muy buenos. Para el resto de los períodos (validación espaciotemporal), se observa que empeoran estas métricas, pero ambas en un rango aceptable. Se muestra que para los períodos P2, P3, P4 y P5 el porcentaje de sesgo es menor que el del GCM de referencia.
</p>

<p align="justify">
8. Para probar la generalización espaciotemporal para distintos set de entrenamiento se considera un experimento de 1000 iteraciones. Donde cada iteración es un set de entrenamiento distinto generado a partir de una semilla aleatoria y la extracción del 40% de las estaciones. Para cada iteración se entrena en P1 y se prueba en las estaciones de testeo desde P1 a P5. El segundo experimento es igual pero en cada iteración se entrena en cada periodo y se prueba en los demás.
</p>

<div align="center">
  <img src="https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PANEL_SCATTERPLOT2.png" alt="Panel_scatterplot2">
</div>
<p><strong>Figura 3</strong>: Panel de gráficos de dispersión para experimentos 1, 2 y un GCM de referencia sin corrección</p>

<p align="justify">
9. Con <strong>Figura 2</strong> y <strong>Figura 3</strong> es posible desprender, a partir de los modelos ajustados y las validaciones generadas, la capacidad de generalización espaciotemporal de un modelo RF para corregir sesgos en la magnitud de las precipitaciones anuales en tiempos y espacios no entrenados. En todas las validaciones se observa la utilidad del modelo en la reducción del porcentaje de sesgo de las precipitaciones anuales. En los experimentos de la Figura 3 es posible observar un PBIAS mejor que un GCM SD y BC para muchas iteraciones para condiciones desfavorables de prueba de los modelos RF ajustados.
  
</p>

<p align="justify">
10. En la <strong>Figura 4</strong> se grafica la serie temporal de precipitaciones anuales, con y sin corrección para la estación Visviri. Se muestran los resultados de cuando la estación participa en el entrenamiento y de cuando no. Con ello, puede observarse que a pesar de la reducción en el PBIAS mostrada en las <strong>Figuras 2</strong> y <strong>3</strong>, el modelo ajustado sin la estación no logra alcanzar un valor adecuado de R<sup>2</sup>. Estos gráficos muestran una reducción del PBIAS, pero sin una mejora significativa en la capacidad explicativa del modelo RF (R<sup>2</sup>). Esta situación sugiere que el modelo podría estar sobreajustado al conjunto de entrenamiento o que, con las variables seleccionadas, no es posible explicar de forma adecuada la variabilidad temporal observada en los datos. Por lo tanto, a pesar de existir una reducción en el PBIAS, el modelo no explicó adecuadamente la variabilidad temporal de los datos dada una estación cualquiera que no participo del entrenamiento.
</p>


<div align="center">
  <img src="https://raw.githubusercontent.com/FelipeRivas5492/RF_corrPP/main/PLOT_VISVIRI.png" alt="PLOT_VISVIRI">
</div>
<p><strong>Figura 4</strong>: Panel series temporales anuales y media y dispersión movil para la estación Visviri. Izq: Visviri no participa en entrenamiento. Der: Visviri participa </p>












