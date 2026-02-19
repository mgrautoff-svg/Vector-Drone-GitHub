# Arquitectura de la Guerra Irregular en Colombia
**Plataforma de Analítica Estratégica para Capacidades Militares Irregulares y Transformación del Conflicto en Colombia**

---

## Resumen Ejecutivo

Este repositorio alberga una **infraestructura de analítica estratégica** diseñada para analizar la evolución de capacidades militares irregulares, los procesos de adaptación organizacional y la transformación del conflicto armado en Colombia. Está construido sobre **datos verificados** de la **Fundación Ideas para la Paz (FIP)** y fuentes oficiales, incluyendo información publicada por el **Comando General de las Fuerzas Militares**.

El objetivo de este proyecto no es la descripción coyuntural. Es la **producción sistemática de inteligencia estratégica**: transformar datos operativos heterogéneos en **indicadores comparables, modelos de escenarios y productos analíticos útiles para la toma de decisiones**.

Esta infraestructura respalda el tipo de análisis publicado en espacios de debate estratégico como *La Silla Vacía* y está diseñada para escalar hacia informes de política pública, evaluaciones estratégicas y ejercicios de planeación por escenarios.

---

## Justificación Estratégica

El debate público sobre seguridad y conflicto suele estar dominado por:

- Narrativas centradas en eventos aislados  
- Interpretaciones tácticas de corto plazo  
- Indicadores fragmentados y no comparables  

Este pipeline responde a una carencia estructural: **la ausencia de marcos analíticos acumulativos, comparables en el tiempo y orientados a escenarios**.

En concreto, permite:

- Integrar **múltiples indicadores** en una arquitectura analítica coherente  
- Convertir bases de datos operativas en **paneles estratégicos longitudinales**  
- Medir **capacidades y adaptación organizacional**, no solo contar eventos  
- Traducir datos en:
  - Escenarios de crecimiento o contención de la amenaza  
  - Métricas de concentración y dispersión del riesgo  
  - Índices sintéticos de riesgo estratégico  
  - Diagnósticos sobre gobernanza, innovación y aprendizaje organizacional en estructuras armadas  

En otras palabras:

> Este marco está diseñado para apoyar evaluaciones estratégicas prospectivas, no solo descripciones retrospectivas.

---

## Gobernanza de Datos y Trazabilidad

El pipeline se construye sobre:

- Datos verificados de la **Fundación Ideas para la Paz (FIP)**  
- Fuentes oficiales del Estado colombiano  
- Publicaciones del **Comando General de las Fuerzas Militares**  
- Procesos completamente reproducibles de ingesta, transformación, modelado y visualización  

Cada salida (tablas, figuras, escenarios, índices) es **trazable** a:

- Un script específico  
- Una fuente de datos definida  
- Una decisión metodológica explícita  

Esto garantiza:

- Auditabilidad técnica  
- Reproducibilidad de resultados  
- Actualización sistemática de los diagnósticos estratégicos  

---

## Productos Analíticos

El repositorio soporta, entre otros:

- Modelos de escenarios sobre crecimiento y contención de capacidades irregulares  
- Análisis de concentración y fragmentación del poder organizacional armado  
- Índices compuestos de riesgo estratégico  
- Modelos de gobernanza y adaptación organizacional  
- Visualizaciones estratégicas para uso analítico de alto nivel  
- Respaldo empírico para columnas, informes y documentos de política pública  

---

## Publicaciones y Productos de Política Pública

Esta infraestructura respalda análisis publicados en medios de debate estratégico.

- **El giro de los drones en el conflicto colombiano** — *La Silla Vacía*, 2026  
  (enlace a añadir tras publicación)

A medida que se publiquen nuevos artículos, informes o notas de política, los datos y modelos subyacentes serán incorporados aquí para asegurar transparencia, reproducibilidad y acumulación de capacidad analítica.

---

## Estructura del Repositorio

- `scripts/` — Pipeline analítico (ingesta, transformación, modelos, visualización)  
- `data/` — Datos crudos, verificados y derivados  
- `outputs/` — Tablas, figuras y productos analíticos  
- `docs/` — Informes renderizados y notas técnicas  
- `checks/` — Validación de datos y control de calidad  
- `logs/` — Registros de ejecución  

---

## Posicionamiento Estratégico

Este proyecto opera en la intersección entre:

- Analítica de datos  
- Inteligencia estratégica  
- Economía del conflicto  
- Estudios de seguridad y defensa  
- Narrativa estratégica basada en evidencia  

Su propósito no es la visualización por sí misma.  
Es la **producción de insumos analíticos de calidad decisional**.

---

## Uso Público y Gobernanza del Proyecto

Este repositorio funciona como:

- Espina dorsal técnica de análisis estratégicos publicados  
- Archivo reproducible de productos analíticos  
- Plataforma de desarrollo para modelación prospectiva por escenarios  

Cada nuevo producto público tendrá aquí su respaldo empírico y metodológico correspondiente.

---

## Fuentes, trazabilidad y método (How this is sourced)

Este proyecto se construye sobre un principio central: **todo resultado debe ser trazable a datos verificables y a decisiones metodológicas explícitas**.

### Fuentes de datos

La infraestructura integra:

- Bases consolidadas y verificadas por la **Fundación Ideas para la Paz (FIP)**  
- Información pública de entidades del Estado colombiano, incluyendo el **Comando General de las Fuerzas Militares**  
- Registros administrativos y series de indicadores de seguridad, conflicto e impacto humanitario  

Estas fuentes se organizan en tres capas:

- `data/raw/` → Insumos originales  
- `data/verified/` → Datos depurados y validados (capa FIP / fuentes oficiales)  
- `data/derived/` → Paneles, variables e indicadores construidos por el pipeline  

### Trazabilidad analítica

Cada producto analítico (tablas, figuras, escenarios, índices):

- Está generado por un **script específico** en `scripts/`  
- Usa **una versión identificable de los datos** en `data/`  
- Responde a **supuestos metodológicos explícitos** documentados en el código  
- Puede ser **reproducido de extremo a extremo** ejecutando el pipeline completo  

Esto permite:

- Auditoría técnica independiente  
- Reproducción de resultados  
- Actualización sistemática cuando hay nuevos cortes de datos  
- Discusión sustantiva sobre supuestos, no solo sobre conclusiones  

### Qué hace diferente a esta arquitectura

A diferencia de enfoques centrados en eventos o conteos aislados, este pipeline:

- Construye **paneles comparables en el tiempo**  
- Modela **capacidades y adaptación organizacional**, no solo incidentes  
- Produce **escenarios prospectivos** y métricas de riesgo estratégico  
- Vincula datos, modelos y narrativa estratégica en una sola arquitectura reproducible  

