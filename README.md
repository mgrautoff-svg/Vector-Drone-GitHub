# Vector-Drone-GitHub

**Plataforma de análisis estratégico sobre conflicto armado, capacidades militares irregulares y transformación de la guerra en Colombia**, construida sobre datos verificados por la **Fundación Ideas para la Paz (FIP)** y fuentes oficiales, incluyendo información del **Comando General de las Fuerzas Militares**.

Este repositorio no es solo un repositorio de código. Es un **instrumento de producción de análisis estratégico de alto nivel**, orientado a convertir datos complejos en **narrativas analíticas, escenarios prospectivos y diagnósticos útiles para el debate público y la toma de decisiones**.

Aquí se documenta, se reproduce y se respalda empíricamente el tipo de análisis que se publica en espacios como **La Silla Vacía** y otros medios de discusión estratégica.

---

## 🧭 ¿Cuál es el valor agregado de este proyecto?

La mayoría del debate público sobre seguridad se queda en:
- Eventos aislados
- Titulares coyunturales
- Reacciones tácticas

Este proyecto hace algo distinto:

- Integra **múltiples indicadores** en una sola arquitectura analítica coherente  
- Convierte bases de datos operativas en **paneles estratégicos comparables en el tiempo**  
- Permite **medir capacidades**, no solo contar eventos  
- Traduce datos en:
  - Escenarios de crecimiento o contención de la amenaza
  - Medidas de concentración y dispersión del riesgo
  - Índices sintéticos de riesgo estratégico
  - Diagnósticos sobre adaptación, innovación y gobernanza criminal

En otras palabras:  
👉 **No describe solo qué pasó. Permite analizar hacia dónde va el conflicto y por qué.**

---

## 📊 Datos y trazabilidad

El pipeline trabaja sobre:

- Datos consolidados y verificados por la **FIP**
- Información agregada proveniente de fuentes oficiales, incluyendo el **Comando General de las Fuerzas Militares**
- Registros de indicadores de:
  - Seguridad y violencia
  - Dinámicas de grupos armados
  - Impacto humanitario
  - Capacidades emergentes (por ejemplo, uso de drones)

Todo el proceso está diseñado para que:

- Los datos fuente estén separados de los datos derivados  
- Cada transformación sea **reproducible y auditable**  
- Los resultados puedan ser **citados, verificados y actualizados** cuando haya nuevos cortes de información

---

## 📰 Relación con análisis publicados

Este repositorio funciona como **soporte empírico y metodológico** de análisis estratégicos publicados en medios de debate público y político, como **La Silla Vacía**.

Cuando se publica una columna o análisis:

- El artículo presenta la **interpretación estratégica**
- Este repositorio conserva:
  - Los datos usados
  - Los modelos aplicados
  - Las tablas y visualizaciones que sustentan el argumento

De esta forma, el análisis público no es solo opinativo: queda **anclado en evidencia reproducible**.

---

## 🧠 ¿Por qué esto importa?

Porque la discusión sobre seguridad en Colombia suele reaccionar **tarde y mal**:

- Se responde al síntoma, no a la tendencia
- Se persigue el evento, no la capacidad
- Se subestima la velocidad de adaptación del adversario

Este proyecto permite:

- Detectar **cambios estructurales antes de que se vuelvan crisis**
- Evaluar **costos estratégicos de no hacer nada**
- Comparar **escenarios de política pública con métricas claras**
- Pasar del discurso reactivo a la **planificación basada en evidencia**

---

## 🧩 ¿Qué hace el pipeline?

1. **Ingesta y validación de datos**
2. **Construcción de paneles estratégicos comparables**
3. **Ejecución de modelos de análisis estructural**, incluyendo:
   - P2: Escenarios de crecimiento de capacidades
   - P3: Descomposición del crecimiento de la amenaza
   - P4: Concentración y dispersión del riesgo (HHI)
   - P5: Clasificación estratégica de indicadores
   - P8: Índice sintético de riesgo estratégico (“Red Year”)
4. **Generación de tablas, figuras y reportes ejecutivos**

---

## 📁 Estructura del proyecto

```text
scripts/        # Código del pipeline
data/           # Datos (raw, verified, derived)
outputs/        # Resultados, figuras, reportes
docs/           # Documentos editoriales y notas analíticas
checks/         # Contratos de datos y validaciones
