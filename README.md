# DataMigra

DataMigra es una plataforma pública del Núcleo Milenio Migra para consultar, visualizar y reutilizar estadísticas oficiales sobre migración en Chile. Este repositorio reúne la versión compartible de los principales productos publicados por fuente de datos.

## Qué contiene esta carpeta

```text
plataformaMIGRA/
├── README.md
├── code/
├── figures/
└── tables/
```

- `code/`:
  scripts en R organizados por fuente. Cada archivo documenta el flujo general de trabajo desde datos originales hasta figuras y tablas publicables.
- `figures/`:
  figuras finales en formato PNG, ordenadas según la secuencia editorial visible en DataMigra.
- `tables/`:
  tablas espejo en CSV para replicar rápidamente muchas de las figuras publicadas.

## Fuentes incluidas

- `casen`: Encuesta de Caracterización Socioeconómica Nacional.
- `snm`: registros del Servicio Nacional de Migraciones.
- `epe_ine_snm`: estimación de población extranjera residente elaborada por INE y SNM.
- `censo`: Censo de Población y Vivienda.
- `mineduc`: matrícula escolar y educación parvularia obtenidas vía Transparencia.
- `ene`: Encuesta Nacional de Empleo.
- `epf`: Encuesta de Presupuestos Familiares.
- `pdi`: sección en preparación; todavía no cuenta con un script público.

## Cómo navegar la estructura

### `code/`

Cada fuente tiene un script principal:

- `code/casen.R`
- `code/snm.R`
- `code/epe_ine_snm.R`
- `code/censo.R`
- `code/mineduc.R`
- `code/ene.R`
- `code/epf.R`

Los scripts documentan el flujo conceptual `raw -> interim -> final -> figures/tables`:

1. lectura de datos originales;
2. procesamiento hacia `data/interim/`;
3. construcción de paneles en `data/final/`;
4. generación de tablas espejo;
5. generación de figuras;
6. exportación de productos.

Algunos de esos insumos no se distribuyen dentro de esta carpeta pública. Por eso, los scripts documentan claramente qué esperan como entrada, aunque no siempre puedan ejecutarse de forma autónoma solo con el contenido aquí disponible.

### `figures/`

Dentro de cada fuente, las figuras están en formato PNG y siguen una numeración simple:

- `figura_01_...`
- `figura_02_...`
- `figura_03_...`

La numeración coincide con el orden de lectura visible en la web de DataMigra para esa fuente.

### `tables/`

Cuando una figura tiene una tabla base simple que permite replicarla rápidamente, esa tabla se publica en CSV con el mismo número y nombre corto de la figura correspondiente.

Ejemplo:

- `figures/ene/figura_01_participacion_laboral_2012_2025.png`
- `tables/ene/figura_01_participacion_laboral_2012_2025.csv`

En algunos casos no existe una tabla espejo directa. Esto ocurre sobre todo en mapas u otras visualizaciones cuya construcción final requiere pasos espaciales o de diseño adicionales.

## Reutilización

Esta carpeta permite dos usos principales:

- descargar figuras finales ya listas para presentación o docencia;
- reutilizar tablas espejo para replicar resultados sin reconstruir todo el pipeline.

Si necesita rehacer completamente una figura, revise primero el script de la fuente correspondiente y luego contraste con la tabla espejo disponible.

## Cita sugerida

Al reutilizar una figura, tabla o código de esta carpeta, se recomienda citar:

1. DataMigra, Núcleo Milenio Migra;
2. la fuente estadística o administrativa original;
3. el año del dato utilizado;
4. el nombre del archivo reutilizado, cuando corresponda.

## Advertencias metodológicas generales

- Las figuras publicadas priorizan comparabilidad, legibilidad y uso público.
- Las tablas espejo no reemplazan necesariamente todo el procesamiento original.
- Algunas fuentes presentan cambios metodológicos entre años; esos ajustes se documentan en los scripts.
- Las carpetas `data/raw/`, `data/interim/` y `data/final/` forman parte del flujo interno de trabajo del proyecto y no se incluyen como parte de esta carpeta pública.
