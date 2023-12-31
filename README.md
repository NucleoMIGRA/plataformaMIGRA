# Plataforma MIGRA
Repositorio con el fin de evidenciar a través de estadísticas, la situación de migrantes o extranjeros/as, y compararlos con la situación de Nativos (Chilenos/as).
Para cumplir este objetivo, recopilamos diferentes bases de datos (CASEN, ENE, .... )
## CASEN
### Figura 1: Extranjeros en el tiempo por género.
![Figura 1](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_1.png)
```
clear
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
graph export "D:\plataforma_migra\fig_casen\fig_1.png", as(png)  replace
```

### Figura 2: Educación Superior.
![Figura 2](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_2.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear

drop if mujer==.

gen ext_sup=.
replace ext_sup=1 if educ_sup==1 & extranjero==1
replace ext_sup=0 if educ_sup==0 & extranjero==1

gen nacional_sup =.
replace nacional_sup=1 if educ_sup==1 & extranjero==0 
replace nacional_sup=0 if educ_sup==0 & extranjero==0

collapse (mean) nacional_sup ext_sup, by(año)

 
 graph bar  nacional_sup ext_sup, over(año)  legend(label(2 "Migrantes") label(1 "Nativos")) ///
 ytitle("Porcentaje Con Educación Superior") b1title("Año Encuesta CASEN") ///
 title("Porcentaje de Personas con Educación Superior por Origen") ///
 legend(pos(10) ring(0) col(1) order(1 2) )
 
graph export "D:\plataforma_migra\fig_casen\fig_2.png", as(png)  replace
```
### Figura 3: Hacinamiento.
![Figura 3](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_3.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear

drop if mujer==.
drop if hacinamiento==4
drop if hacinamiento==9
drop if hacinamiento==-88
drop if hacinamiento==1


*****************************************************************************

label define extranjero_lbl 1 "Extranjero" 0 "Nativo"

label values extranjero extranjero_lbl


graph bar (count), over(hacinamiento) over(extranjero, label(angle(45))) over(año) asyvars stack percentages ///
 ytitle("Porcentaje") b1title("Año Encuesta CASEN") ///
 title("Porcentaje de Nivel de Hacinamiento por Origen") ///
 legend(label(1 "Medio Hacinamiento") label(2 "Hacinamiento Crítico") pos(6) ring(1) col(1) order(1 2 3) )
 
 graph export "D:\plataforma_migra\fig_casen\fig_3.png", as(png)  replace
```
### Figura 4: Horas de Trabajo.
![Figura 4](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_4.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear
gen horas_ext=.
replace horas_ext=horas_trabajo if extranjero==1
gen horas_nac=.
replace horas_nac=horas_trabajo if extranjero==0

collapse (mean) horas_trabajo horas_ext horas_nac , by(año)
drop if año==2020

binscatter2 (horas_nac) (horas_ext año),  graphregion(fcolor(white) lcolor(white) ///
ifcolor(white) ilcolor(white)) xtitle("Año Encuesta CASEN") ytitle("Horas")  linetype(connect) title("Horas Promedio de Trabajo Semanales por Origen") ///
name(fig4a, replace) legend(label(2 "Extranjeros") label(1 "Nativos")) ///
legend(pos(3) ring(0) col(1) order(1 2) ) xlabel(2009 2011 2013 2015 2017  2022)
graph export "D:\plataforma_migra\fig_casen\fig_4.png", as(png) replace
```
### Figura 5: Porcentaje de Personas Contratadas.
![Figura 5](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_5.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear

gen contra_ext=.
replace contra_ext=contrato if extranjero==1
gen contra_nac=.
replace contra_nac=contrato if extranjero==0

collapse (mean) contra_ext  contra_nac , by(año)
drop if año==2020

graph bar  contra_nac contra_ext, over(año)  legend(label(2 "Migrantes") label(1 "Nativos")) ///
 ytitle("Porcentaje") b1title("Año Encuesta CASEN") ///
 title("Porcentaje de Personas con Contrato de Trabajo por Origen") ///
 legend(pos(10) ring(0) col(1) order(1 2) )
 graph export "D:\plataforma_migra\fig_casen\fig_5.png", as(png) replace
```
### Figura 6: Salario.
![Figura 6](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_6.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear
drop if ing_del_trabajo>5000000
gen salario=(ing_act_prin*inflacion)/(4*horas_trabajo)

gen salario_ext=.
replace salario_ext=salario if extranjero==1
gen salario_nac=.
replace salario_nac=salario if extranjero==0

collapse (mean) salario_ext  salario_nac , by(año)
drop if año==2020

binscatter2 (salario_nac) (salario_ext año), graphregion(fcolor(white) lcolor(white) ///
ifcolor(white) ilcolor(white)) xtitle("Año Encuesta CASEN") ytitle("Salario Promedio")  linetype(connect) title("Salario Promedio por Origen") ///
name(fig6a, replace) legend(label(2 "Extranjeros") label(1 "Nativos")) ///
legend(pos(3) ring(0) col(1) order(1 2) ) xlabel(2009 2011 2013 2015 2017  2022) 

 graph export "D:\plataforma_migra\fig_casen\fig_6.png", as(png)  replace
```
### Figura 7: Ruralidad.
![Figura 7](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_7.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear

drop if mujer==.
drop if año==2022

gen ext_rur=.
replace ext_rur=1 if urbano==0 & extranjero==1
replace ext_rur=0 if urbano==1 & extranjero==1

gen nac_rur=.
replace nac_rur=1 if urbano==0 & extranjero==0
replace nac_rur=0 if urbano==1 & extranjero==0

collapse (mean) nac_rur ext_rur, by(año)
gen nac_rur_por=nac_rur*100
gen ext_rur_por= ext_rur*100
 
 graph bar  nac_rur_por ext_rur_por, over(año)  legend(label(2 "Migrantes") label(1 "Nativos")) ///
 ytitle("Porcentaje") b1title("Año Encuesta CASEN") ///
 title("Porcentaje de Personas Viviendo en Zonas Rurales") ///
 legend(pos(1) ring(0) col(1) order(1 2) )
 
graph export "D:\plataforma_migra\fig_casen\fig_7.png", as(png)  replace
```
### Figura 8: Pirámides Poblacionales.
![Figura 8](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_8.png)
```
*****************************GRAFICO 8******************************************
****8. PIRAMIDE DEMOGRAFICA****
*8.1 PARA TODO CHILE
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear
keep if año==2022
replace edad=90 if edad>90
gen grupos_edad=5*int(edad/5)
label var grupos_edad "Edad (tramos de 5 años)"
label define rangos_g  0 "0-5" 5 "5-10" 10 "10-15" 15 "15-20" 20 "20-25" 25 "25-30" ///
30 "30-35" 35 "35-40" 40 "40-45" 45 "45-50" 50 "50-55" 55 "55-60" 60 "60-65" ///
65 "65-70" 70 "70-75" 75 "75-80" 80 "80-85" 85 "85-90" 90 "90 +"
label values grupos_edad rangos_g
gen pobtot=1
egen totpob=sum(pobtot)
egen pop5=sum(pobtot), by(grupos_edad mujer)

gen cero=0

gen Hombres=-100*pop5/totpob if mujer==0
gen Mujeres=100*pop5/totpob if mujer==1



twoway ///
(bar Hombres grupos_edad  , horizontal barwidth(5) ) ///
(bar Mujeres grupos_edad  , horizontal barwidth(5) )  ///
(scatter grupos_edad cero, msymbol(i) mlabel(grupos_edad) mlabstyle(p1) mlabcolor(black) mlabsize(*.5) xlabel(-4 "4" -2 "2" 0 "0" 2 "2" 4 "4") legend(order(1 "Hombres" 2 "Mujeres"))), ///
title("Pirámide Poblacional, Chile 2022") ///
xtitle("Porcentaje de Población") 

****8. PIRAMIDE DEMOGRAFICA****
*8.2 PARA EXTRANJEROS
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear
keep if año==2022
keep if extranjero==1
replace edad=90 if edad>90
gen grupos_edad=5*int(edad/5)
label var grupos_edad "Edad (tramos de 5 años)"
label define rangos_g  0 "0-5" 5 "5-10" 10 "10-15" 15 "15-20" 20 "20-25" 25 "25-30" ///
30 "30-35" 35 "35-40" 40 "40-45" 45 "45-50" 50 "50-55" 55 "55-60" 60 "60-65" ///
65 "65-70" 70 "70-75" 75 "75-80" 80 "80-85" 85 "85-90" 90 "90 +"
label values grupos_edad rangos_g
gen pobtot=1
egen totpob=sum(pobtot)
egen pop5=sum(pobtot), by(grupos_edad mujer)

gen cero=0

gen Hombres=-100*pop5/totpob if mujer==0
gen Mujeres=100*pop5/totpob if mujer==1



twoway ///
(bar Hombres grupos_edad  , horizontal barwidth(5) ) ///
(bar Mujeres grupos_edad  , horizontal barwidth(5) )  ///
(scatter grupos_edad cero, msymbol(i) mlabel(grupos_edad) mlabstyle(p1) mlabcolor(black) mlabsize(*.5) xlabel(-8 "8" -6"6"  -4 "4" -2 "2" 0 "0" 2 "2" 4 "4" 6 "6" 8 "8") legend(order(1 "Hombres" 2 "Mujeres"))), ///
title("Pirámide Poblacional de Extranjeros, Chile 2022") ///
xtitle("Porcentaje de Población") saving(extranjero, replace)


****8. PIRAMIDE DEMOGRAFICA****
*8.2 PARA NATIVOS
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear
keep if año==2022
keep if extranjero==0
replace edad=90 if edad>90
gen grupos_edad=5*int(edad/5)
label var grupos_edad "Edad (tramos de 5 años)"
label define rangos_g  0 "0-5" 5 "5-10" 10 "10-15" 15 "15-20" 20 "20-25" 25 "25-30" ///
30 "30-35" 35 "35-40" 40 "40-45" 45 "45-50" 50 "50-55" 55 "55-60" 60 "60-65" ///
65 "65-70" 70 "70-75" 75 "75-80" 80 "80-85" 85 "85-90" 90 "90 +"
label values grupos_edad rangos_g
gen pobtot=1
egen totpob=sum(pobtot)
egen pop5=sum(pobtot), by(grupos_edad mujer)

gen cero=0

gen Hombres=-100*pop5/totpob if mujer==0
gen Mujeres=100*pop5/totpob if mujer==1



twoway ///
(bar Hombres grupos_edad  , horizontal barwidth(5) ) ///
(bar Mujeres grupos_edad  , horizontal barwidth(5) )  ///
(scatter grupos_edad cero, msymbol(i) mlabel(grupos_edad) mlabstyle(p1) mlabcolor(black) mlabsize(*.5) xlabel(-8 "8" -6"6"  -4 "4" -2 "2" 0 "0" 2 "2" 4 "4" 6 "6" 8 "8") legend(order(1 "Hombres" 2 "Mujeres"))), ///
title("Pirámide Poblacional de Nativos, Chile 2022") ///
xtitle("Porcentaje de Población") saving(nativo, replace)

gr combine extranjero.gph nativo.gph

graph export "D:\plataforma_migra\fig_casen\fig_8.png", as(png)  replace
```


### Figura 9: Años de Escolaridad.
![Figura 9](https://github.com/NucleoMIGRA/plataformaMIGRA/blob/main/Figuras/fig_9.png)
```
clear
set more off

set scheme modern, perm
use "D:\plataforma_migra\casen_panel\casen_panel_2009_2022", clear

gen esc_ext=.
replace esc_ext=esc if extranjero==1
gen esc_nac=.
replace esc_nac=esc if extranjero==0

collapse (mean) esc_ext esc_nac , by(año)

binscatter2 (esc_nac) (esc_ext año),  graphregion(fcolor(white) lcolor(white) ///
ifcolor(white) ilcolor(white)) xtitle("Año Encuesta CASEN") ytitle("Años")  linetype(connect) title("Años Promedio de Escolaridad por Origen") ///
name(fig9, replace) legend(label(2 "Extranjeros") label(1 "Nativos")) ///
legend(pos(11) ring(0) col(1) order(1 2) ) xlabel(2009 2011 2013 2015 2017 2020 2022)
graph export "D:\plataforma_migra\fig_casen\fig_9.png", as(png)  replace

```
