# Ayuda - PsyML Report Generator

## Como usar esta aplicacion

### 1. Generar el archivo JSON

Primero, ejecute su analisis con PsyML capturando el verbose:

```r
library(PsyML)

# Ejecutar analisis capturando output
resultado <- easy_ml_capture(
  data = mis_datos,
  target = "mi_variable",
  task = "classification"
)

# Exportar como JSON
export_verbose_json(resultado, "mi_analisis.json")
```

### 2. Subir el archivo JSON

- Haga clic en "Buscar..." para seleccionar su archivo JSON
- El archivo debe haber sido generado con `export_verbose_json()`

### 3. Configurar el reporte

- **Titulo**: El titulo que aparecera en el reporte
- **Autor(es)**: Nombre de los autores (opcional)
- **Tipo de reporte**:
  - *Cientifico*: Formato estilo articulo cientifico con secciones de Metodo, Resultados y Referencias
  - *Resumen ejecutivo*: Version corta con los puntos clave
  - *Completo*: Incluye todo el verbose como anexo

### 4. Seleccionar secciones

Elija que secciones incluir en el reporte:

- **Metodo**: Descripcion general del analisis
- **Participantes**: Informacion sobre la muestra
- **Analisis de datos**: Detalles tecnicos del modelado
- **Metricas**: Resultados de rendimiento del modelo
- **Importancia de variables**: Ranking de variables mas influyentes
- **Interpretacion**: Explicacion de los resultados
- **Referencias**: Bibliografia en formato APA

### 5. Generar y descargar

1. Haga clic en "Generar Reporte" para crear el documento
2. Revise la vista previa
3. Haga clic en "Descargar Reporte" para obtener el archivo

## Formatos disponibles

- **Word (.docx)**: Ideal para editar y agregar contenido
- **HTML**: Para publicacion web
- **PDF**: Para documentos finales

## Consejos

- El reporte generado es un punto de partida. Se recomienda revisar y personalizar el contenido.
- Las referencias estan en formato APA 7.
- Puede agregar sus propias referencias y citas despues de generar el documento.

## Soporte

Para reportar problemas o sugerencias, visite:
https://github.com/jventural/PsyML/issues
