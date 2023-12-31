# Plataforma MIGRA
Repositorio con el fin de evidenciar a través de estadísticas, la situación de migrantes o extranjeros/as, y compararlos con la situación de Nativos (Chilenos/as).
Para cumplir este objetivo, recopilamos diferentes bases de datos (CASEN, ENE, .... )
## CASEN
### Figura 1: Extranjeros en el tiempo por género.
![Figura 1](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_1.png)
`clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear

drop if mujer==.

gen mujer_ext=.
replace mujer_ext=1 if mujer==1 & extranjero==1

gen hombre_ext=.
replace hombre_ext=1 if mujer==0 & extranjero==1 



collapse (sum) mujer_ext hombre_ext, by(año)

binscatter2 (hombre_ext) (mujer_ext año),  graphregion(fcolor(white) lcolor(white) ///
ifcolor(white) ilcolor(white)) xtitle("Año Encuesta CASEN") ytitle("Cantidad de Migrantes")  linetype(connect) title("Cantidad de Migrantes por Género en el Tiempo") ///
name(fig1a, replace) legend(label(1 "Hombres") label(2 "Mujeres")) ///
legend(pos(10) ring(0) col(1) order(1 2) ) xlabel(2009 2011 2013 2015 2017 2020 2022)
graph export "D:\plataforma_migra\fig_casen\fig_1.png", as(png)  replace`
### Figura 2: Educación Superior.
![Figura 2](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_2.png)
### Figura 3: Hacinamiento.
![Figura 3](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_3.png)
### Figura 4: Horas de Trabajo.
![Figura 4](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_4.png)
### Figura 5: Porcentaje de Personas Contratadas.
![Figura 5](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_5.png)
### Figura 6: Salario.
![Figura 6](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_6.png)
### Figura 7: Ruralidad.
![Figura 7](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_7.png)
### Figura 8: Pirámides Poblacionales.
![Figura 8](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_8.png)

