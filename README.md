# Monitor de respuestas · Encuesta JusMer, Ola 1

Aplicación Shiny para seguir el trabajo de campo de la Ola 1 de la encuesta del proyecto Fondecyt N° 1250518 *Market Justice and Deservingness of Social Welfare* ([jus-mer/survey-jusmer](https://github.com/jus-mer/survey-jusmer), carpeta `surveys/ola1`). Muestra el avance del campo y los descriptivos básicos de las variables principales a medida que entran las respuestas.

La app es independiente de la encuesta: **solo lee** la tabla de respuestas y nunca escribe en ella.

![Resumen del monitor](docs/capturas/resumen.png)


## Qué muestra

| Pestaña | Contenido |
|---|---|
| **Resumen** | Encuestas completas y avance respecto de la meta, sesiones iniciadas, tasa de término, completas del día, duración mediana, sesiones por día, curva acumulada, abandono según el último bloque con respuestas (por forma A/B), formas y dispositivos. |
| **Muestra** | Sexo, edad y nivel educativo contra el marco de cuotas; nivel educativo, ingreso, estatus subjetivo, posición política, situación económica, identidad de género, sistema de salud y tipo de colegio. |
| **Escalas** | Barras apiladas y tabla (N, media, DE, % alto, % No sé, % Prefiero no responder) para Justicia de mercado, Lucro, Merecimiento, Meritocracia e Inseguridad económica. |
| **Conjoint y decisiones** | Reparto del monto en las seis tareas, consistencia de la tarea repetida (práctica y tarea 6 con lados invertidos), decisiones de pago (`dec_1` a `dec_3`) y prioridades de inversión pública. |
| **Calidad** | Chequeo de atención, comprensión de la tarea conjoint, respuestas muy rápidas, respuesta idéntica en la batería de merecimiento, exceso de No sé / No responde, duración e ítems con más No sé y Prefiero no responder. |
| **Explorador** | Frecuencias de cualquier variable del cuestionario, con sus etiquetas. |

La barra lateral permite filtrar por casos (solo completas o todas las que aceptaron), forma de la encuesta y fechas, y ajustar la meta. La app vuelve a leer los datos cada 60 segundos y también con el botón **Actualizar ahora**.

### Definiciones

- **Inicio del conteo**: solo se consideran las sesiones iniciadas desde `INICIO_CONTEO_DEFAULT` (`R/config.R`, hora de Chile), para dejar fuera las pruebas previas al lanzamiento. Se cambia con la variable de entorno `MONITOR_DESDE` (p. ej. `2026-10-28` o `2026-10-28 15:00`); con `MONITOR_DESDE=todas` se cuentan todas.

- **Completa**: aceptó el consentimiento y respondió alguna de las preguntas finales (`salud_prov`, `educ_prov`). Se definen en `VARS_FIN` (`R/config.R`).
- **Abandono**: para las sesiones incompletas, el bloque más avanzado con alguna respuesta, según el orden de módulos de su forma (A: Meritocracia, Actitudes, Merecimiento; B: Merecimiento, Actitudes, Meritocracia).
- **Sliders con "Prefiero no responder"**: si la casilla `<id>_nr` está marcada, el valor del slider no se considera.
- **Tarea repetida**: la tarea 6 muestra los perfiles de la práctica con los lados invertidos; una respuesta consistente cumple práctica + tarea 6 ≈ $2.000.000 (tolerancia de $200.000).
- **Respuestas muy rápidas**: duración menor a un tercio de la mediana.
- Los umbrales de los indicadores de calidad (`UMBRALES_CALIDAD`) son referenciales y pueden ajustarse.
- El **marco de cuotas** (`MARCO_CUOTAS`) se copió de `output/monitoreo_cuotas/app.R` del pre-piloto: conviene confirmar que corresponde al diseño de la Ola 1.

## Estructura

```
monitor-jusmer/
├── app.R               interfaz y servidor
├── R/
│   ├── config.R        bloques, módulos, etiquetas breves, cuotas, umbrales
│   ├── preguntas.R     lectura de config/questions.yml
│   ├── datos.R         fuente de datos: demo o base
│   ├── simular.R       generador de respuestas simuladas
│   ├── procesar.R      variables derivadas y descriptivos
│   └── graficos.R      gráficos (ggplot2)
├── config/
│   └── questions.yml   copia de surveys/ola1/_survey/questions.yml
├── docs/capturas/      capturas de pantalla
└── .env.example        plantilla de credenciales (el .env real no se sube)
```

Si cambia el cuestionario: copiar de nuevo `surveys/ola1/_survey/questions.yml` a `config/questions.yml` y ajustar las listas de `R/config.R` (bloques, módulos y etiquetas breves).

