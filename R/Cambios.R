---
  title: "Indicadores de migración interna"
subtitle: "Comparativo microdatos del censo y la muestra"
author: "CONAPO"
output:
  html_document:
  highlight: tango
theme: flatly
toc: yes
toc_depth: 2
toc_float:
  collapsed: yes
---
  
  \usepackage{color}

```{=html}
<style>
  code.r{
    font-size: 10px;
  }
pre {
  font-size: 12px
}
</style>
  
  <style>
  body {
    text-align: justify;
    font-style: normal;
    font-family: "Montserrat Medium";
    font-size: 12px
  }
h1.title {
  font-size: 40px;
  color: #000D3B;
}
h1 {
  color: #B6854D;
}
h2 {
  color: #172984;
}
h3 {
  color: #172984;
}
</style>
  ```

```{=html}
<style>
  .nav>li>a {
    position: relative;
    display: block;
    padding: 10px 15px;
    color: #0A2687;
  }
.nav-pills>li.active>a, .nav-pills>li.active>a:hover, .nav-pills>li.active>a:focus {
  color: #ffffff;
    background-color: #09C2BC;
}
</style>
  ```

```{r, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, cache = FALSE, cache.lazy = FALSE, 
                      eval = FALSE, class.source = "fold-show")
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
options(digits = 2, encoding = "UTF8")
```   


```{r, echo=FALSE}
rm(list = ls())
```

```{r, echo=FALSE}
setwd(here::here())
```


```{r, echo=FALSE}
#Font Stlye
require(showtext)
library(extrafont)
# activar showtext
# SE cargan las funtes del sitema Windows (Pero tarda en correr)
#ttf_import('C:/Users/dvill/AppData/Local/Microsoft/Windows/Fonts/')
#fonts()
#extrafont::loadfonts(device = "win")
#font_import()
windowsFonts()
```


```{r, echo = FALSE}
# Librerías que se usaron en el documCVE_ENTo
require(Cairo)
require(haven)
require(Hmisc) # %nin%
require(dplyr)
require(survey)
require(srvyr)
require(stringr)
require(sna)
require(expss)
require(knitr)
require(kableExtra)
require(sjlabelled)
require(gt)
require(ggplot2)
require(ggpubr)
require(janitor)
require(tibble)
require(tidyr)
require(reshape2)
require(openxlsx)
require(doMC)
registerDoMC(cores = 60)
```



**Cuestionario Ampliado del Censo de Población y Vivienda 2020**
  
  El cuestionario ampliado se guarda en un un archivo `.RData`.

```{r, eval = FALSE}
data <- read_sav("~/Personas_Censo 2020.SAV")
save(data, 
     file = paste0(here::here(), "/Bases/Censo_Personas_2020.RData"))
```

Se seleccionan las variables que se desean conservar para la realización de este documento y se guarda en un archivo `.RData` para practicidad del manejo de datos.

**Posibles variables que se pueden contemplar en la migración reciente**
  
  -   `EDAD`   
-   `SEXO`  
-   `AFRODES`\   
-   `HLENGUA`\   
-   `QDIALECT_INALI`    
-   `PERTE_INDIGENA`\  
-   `NIVACAD`   
-   `ALFABET`   
-   `CAUSA_MIG`\   
-   `SITUA_CONYUGAL`\   
-   `CONACT`\  
-   `HIJOS_NAC_VIVOS`  

La variable `mydata` contiene **15 015 683 observaciones** y **12 variables**.

```{r, eval = FALSE}
load(paste0(here::here(), "/Bases/Censo_Personas_2020.RData"))

mydata <- data %>%
  select(CVE_ENT, ENT, MUN, CVE_MUN, LOC50K, COBERTURA, CLAVIVP, NUMPER, ENT_PAIS_NAC,
         ENT_PAIS_RES_5A, MUN_RES_5A, CVE_MUN_RES15, EDAD, SEXO, AFRODES, 
         HLENGUA, QDIALECT_INALI, PERTE_INDIGENA, NIVACAD, ALFABET, ESCOLARI, ESCOACUM,
         CONACT, SITTRA, CAUSA_MIG, SITUA_CONYUGAL, CONACT, HIJOS_NAC_VIVOS, 
         FACTOR, ESTRATO, UPM)

save(mydata, file = paste0(here::here(), "/Bases/01_Migracion interna_2020.RData"))
```


✔️A partir de aquí se pueden correr los códidos 👇.   
Se carga el archivo `Migracion interna_2020.RData`.   

```{r}
load(file = paste0(here::here(), "/Bases/01_Migracion interna_2020.RData"))

# Para fines prácticos se genera un ponderador de uno 
muestra_2020 <- mydata %>%
  select(CVE_ENT, ENT, MUN, CVE_MUN, LOC50K, COBERTURA, ENT_PAIS_NAC, ENT_PAIS_RES_5A, MUN_RES_5A, CVE_MUN_RES15, EDAD, COBERTURA, FACTOR, ESTRATO, UPM) %>%
  mutate(M = 1)  %>%
  mutate(NOM_ENT = as.factor(.$CVE_ENT)) %>%
  ungroup()
```

**Claves de entidades y municipios**
  
  Se genera un vector con el nombre de las entidades llamado `estados` para facilitar los filtros en el documento.\  
Se genera un vector con las abreviaturas de las entidades llamado `ent` para fines prácticos.\      
Se genera un vector con las claves de los municipios, pero es importante hacer notar que tres municipios no entraron el muestreo del Cuestionario Ampliado.      

```{r}
# Claves de los estados
estados <- sjlabelled::get_labels(muestra_2020$CVE_ENT)
nom_estados <- c( "Aguascalientes", "Baja California" ,"Baja California Sur", "Campeche", "Coahuila de Zaragoza",
                  "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
                  "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",  "México", 
                  "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", 
                  "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", 
                  "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala","Veracruz de Ignacio de la Llave", 
                  "Yucatán", "Zacatecas")
est <- c("AGS", "BC", "BCS", "CAMP", "COAH", "COL", "CHIS", "CHIH", "CDMX", "DGO", "GTO", "GRO", "HGO",
         "JAL", "MEX", "MICH", "MOR", "NAY", "NL", "OAX", "PUE", "QRO", "QROO", "SLP","SIN","SON", "TAB", 
         "TAMS", "TLX", "VER", "YUC", "ZAC")

# Claves de los municipios
MUN <- readRDS(paste0(here::here(), "/Bases/municipios_2020.RDS"))
nom_municipios <- sjlabelled::get_labels(MUN$NOM_MUN) %>% as.factor()
municipios <- sjlabelled::get_labels(MUN$CVE_MUN) %>% as.factor()

# Claves de los estados
ENT <- readRDS(file = paste0(here::here(), "/Bases/entidad_2020.RDS"))
```


**Población de 5 años y más**   
  
  Se identifica a la población de 5 años y más.   

`filter(EDAD >= 5 & EDAD != 999)'.`   

```{r}
Pob.5ymas <- muestra_2020 %>%
  as.data.frame() %>%
  mutate(EDAD = as.numeric(.$EDAD)) %>%
  subset(EDAD >= 5 & EDAD <= 130) %>%
  filter(ENT_PAIS_RES_5A %in% estados)  # Filtro del lugar de residencia dentro del país. 
```

```{r, echo = FALSE}
# Población de 5 años y más
tabla <- Pob.5ymas %>%
  summarise(Pob_5ymas = sum(.$FACTOR))
rm(Pob.5ymas)

as.integer(tabla) %>%  
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ prettyNum(., big.mark = " ", preserve.width = "none"))) %>%
  gt() %>% 
  tab_header(title = "Población de 5 años y más") %>%
  tab_options(heading.title.font.size = 12, 
              heading.align = "center",
              heading.subtitle.font.size = 10,
              table.align = "center",
              column_labels.font.weight = "bold",
              table.font.names = 'montserrat',
              table.font.size = 8) %>%  
  as_raw_html()
```


# Nivel estatal {.tabset .tabset-pills}

## Población Total {.tabset .tabset-pills}

Estructura de la población por estado. Utilizando REDATAM.  

````
```{r}`r ''`
TABLE DR_RESHAB
FREQ CVE_ENT
TITLE "RESIDENCIA HABITUAL"
OUTPUTFILE XLS "Output\Residencia_Habitual a nivel estatal_2020.xlsx" OVERWRITE
```
````

```{r}
options(survey.lonely.psu = "adjust")

MC <- muestra_2020 %>%
  as.data.frame() %>%
  select(FACTOR, ESTRATO, UPM, CVE_ENT) %>%
  mutate(probs = 1/.$FACTOR) %>%
  mutate(Pob.Total = 1) 

svy_design <- MC  %>%
  svydesign(data = ., ids = ~UPM, strata = ~ESTRATO, 
            #weights = ~FACTOR, 
            nest = TRUE,
            probs = ~probs)

## Población de total
Total_est <- svyby(~Pob.Total,
                   by = ~M,
                   svy_design,
                   svytotal,
                   vartype = c("se","cv"),
                   drop.empty.groups = TRUE) %>%
  cbind(confint(object = ., level = 0.90))

Total_est %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ prettyNum(., big.mark = " ", preserve.width = "none")))

## Población total en el estado 
est <- svyby(~Pob.Total,
             by = ~CVE_ENT,
             svy_design,
             svytotal,
             vartype = c("se","cv"),
             drop.empty.groups = TRUE) %>%
  cbind(confint(object = ., level = 0.90))

est %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ prettyNum(., big.mark = " ", preserve.width = "none")))

# Se guardan los resultados
wb <- createWorkbook()
addWorksheet(wb, "Pob.Total (Estados)")
writeData(wb, 1, est, colNames = TRUE, rowNames = TRUE)
saveWorkbook(wb, 
             file = paste0(here::here(), "/Output/Poblacion Total (Estados).xlsx"), 
             overwrite = TRUE)
```

## Población 5 años y más {.tabset .tabset-pills}

Estructura de la población de 5 años y más. Utilizando REDATAM.  

````
```{r}`r ''`
TABLE DR_RESHAB
FREQ CVE_ENT
FILTER EDAD > 4 AND EDAD <= 130
TITLE "RESIDENCIA HABITUAL"
OUTPUTFILE XLS "Output\Poblacion 5 años y mas a nivel estatal_2020.xlsx" OVERWRITE
```
````
```{r}
options(survey.lonely.psu = "adjust")

MC <- muestra_2020 %>%
  as.data.frame() %>%
  select(FACTOR, ESTRATO, UPM, CVE_ENT, EDAD) %>%
  mutate(probs = 1/.$FACTOR) %>%
  mutate(EDAD = as.numeric(.$EDAD), 
         M = 1) %>%
  mutate(Pob.5ymas = case_when(.$EDAD >= 5 & .$EDAD <= 130 ~ 1,
                               TRUE ~  0)) 

svy_design <- MC  %>%
  svydesign(data = ., ids = ~UPM, strata = ~ESTRATO, 
            #weights = ~FACTOR, 
            nest = TRUE,
            probs = ~probs)

## Población de 5 años y más
Total_est <- svyby(~Pob.5ymas,
                   by = ~M,
                   svy_design,
                   svytotal,
                   vartype = c("se","cv"),
                   drop.empty.groups = TRUE) %>%
  cbind(confint(object = ., level = 0.90))

Total_est %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ prettyNum(., big.mark = " ", preserve.width = "none")))


## Población de 5 años y más en el estado 
est <- svyby(~Pob.5ymas,
             by = ~CVE_ENT,
             svy_design,
             svytotal,
             vartype = c("se","cv"),
             drop.empty.groups = TRUE) %>%
  cbind(confint(object = ., level = 0.90))

est %>%
  as.data.frame() %>%
  mutate(across(where(is.numeric), ~ prettyNum(., big.mark = " ", preserve.width = "none")))

# Se guardan los resultados
wb <- createWorkbook()
addWorksheet(wb, "Pob.5ymas (Total)")
addWorksheet(wb, "Pob.5ymas (Estados)")
writeData(wb, 1, est, colNames = TRUE, rowNames = TRUE)
writeData(wb, 1, est, colNames = TRUE, rowNames = TRUE)

saveWorkbook(wb, 
             file = paste0(here::here(), "/Output/Pob.5ymas (Estados).xlsx"), 
             overwrite = TRUE)
```

## Migración nacimiento {.tabset .tabset-pills}

Se utiliza la paquetería `survey` para poder trabajar con la muestra del cuestionario ampliado, en la cual se selecciona a la población de 5 años y más.

### Muestreo Complejo (Opción 1)


```{r, eval = FALSE}
require(srvyr)
options(survey.lonely.psu="adjust")
options(survey.lonely.psu="certainty")
options("survey.ultimate.cluster" = FALSE)

MC <- muestra_2020 %>%
  as.data.frame() %>%
  select(FACTOR, ESTRATO, UPM, CVE_ENT, ENT_PAIS_RES_5A, EDAD) %>%
  mutate(EDAD = as.numeric(.$EDAD)) %>%
  mutate(ENT_PAIS_RES_5A = case_when(.$ENT_PAIS_RES_5A %in% estados ~.$ENT_PAIS_RES_5A,
                                     .$ENT_PAIS_RES_5A %nin% estados ~ "888", #Residencia en otro país
                                     .$ENT_PAIS_RES_5A %in% "997" ~ "997",
                                     .$ENT_PAIS_RES_5A %in% "998" ~ "998",
                                     .$ENT_PAIS_RES_5A %in% "997" ~ "999")) %>% 
  mutate(I_RES_5A = case_when((.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% estados ~ .$ENT_PAIS_RES_5A,
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "888" ~ "888",
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "997" ~ "997",
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "998" ~ "998",
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "999" ~ "999")) %>%
  mutate(M = 1) 

svy_design <- MC  %>%
  svydesign(data = ., ids = ~UPM, strata = ~ESTRATO, #weights = ~FACTOR, 
            nest = TRUE, probs = probs)
#clusterExport(cl, c("MC"))
#svy_design <- parLapply(cl, 1:length(cl), function(i) {
# svydesign(data = MC[i:nrow(MC), ], ids = ~UPM, strata = ~ESTRATO, weights = ~FACTOR, nest = TRUE)
#})
probs <- svy_design$prob
svy_design <- MC  %>%
  svydesign(data = ., ids = ~UPM, strata = ~ESTRATO, #weights = ~FACTOR, 
            nest = TRUE, probs = probs)

#p <- as.svrepdesign(svy_design, type="bootstrap", replicates=100)

#options("survey.ultimate.cluster" = FALSE)
#p <- as.svrepdesign(svy_design)

require(survey)
tabla <- svytotal(~M + CVE_ENT, svy_design)

library(fastsurvey)
fastsurvey::svytotal(~M,
                     multistage_design)

p <- MC  %>%
  svrepdesign(data = mydata, ids = ~UPM, strata = ~ESTRATO, weights = ~FACTOR, nest = TRUE) 
POB_TOT_est <- svytotal(~M,
                        MC)
## Población total en el estado 
POB_TOT_est <- svyby(~M,
                     by = ~CVE_ENT,
                     svy_design,
                     svytotal,
                     vartype = c("se","cv", "ci"),
                     level = 0.90
                     #drop.empty.groups = TRUE 
                     #na.rm.by = TRUE, 
                     #multicore=getOption("survey.multicore")
) #desvest / %CV / Coef. de variación

confint(POB_TOT_est, level = 0.90, df =Inf)
```


#### Muestreo Complejo (Opción 2)

```{r}
MC <- muestra_2020 %>%
  as.data.frame() %>%
  select(FACTOR, ESTRATO, UPM, CVE_ENT, ENT_PAIS_RES_5A, EDAD) %>%
  mutate(EDAD = as.numeric(.$EDAD)) %>%
  mutate(ENT_PAIS_RES_5A = case_when(.$ENT_PAIS_RES_5A %in% estados ~.$ENT_PAIS_RES_5A,
                                     .$ENT_PAIS_RES_5A %nin% estados ~ "888", #Residencia en otro país
                                     .$ENT_PAIS_RES_5A %in% "997" ~ "997",
                                     .$ENT_PAIS_RES_5A %in% "998" ~ "998",
                                     .$ENT_PAIS_RES_5A %in% "997" ~ "999")) %>% 
  mutate(I_RES_5A = case_when((.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% estados ~ .$ENT_PAIS_RES_5A,
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "888" ~ "888",
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "997" ~ "997",
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "998" ~ "998",
                              (.$EDAD >= 5 & .$EDAD <= 130) & ENT_PAIS_RES_5A %in% "999" ~ "999")) %>%
  mutate(CVE_ENTS = case_when(.$ENT_PAIS_RES_5A %in% estados ~.$ENT_PAIS_RES_5A)) %>%
  mutate(M = 1) %>%
  mutate(RESHAB = 1,
         RESREC = case_when(EDAD >= 5 & EDAD <= 130 ~ 1,
                            TRUE ~ NA),
         MIG = case_when(.$CVE_ENT !=.$ENT_PAIS_RES_5A  & (EDAD >= 5 & EDAD <= 130) ~ 1,
                         TRUE ~ NA),
         NOMIGRA = case_when(.$CVE_ENT ==.$ENT_PAIS_RES_5A & (EDAD >= 5 & EDAD <= 130) ~ 1,
                             TRUE ~ NA)) 
MC <- MC %>%
  as.data.frame() %>%
  select(FACTOR, ESTRATO, UPM, CVE_ENT, ENT_PAIS_RES_5A, RESHAB, RESREC,  MIG, NOMIGRA) %>%
  melt(., id = c("UPM", "ESTRATO", "FACTOR", "RESHAB", "RESREC", "MIG", "NOMIGRA")) %>%
  srvyr::as_survey_design(ids = UPM, strata = ESTRATO, weights = FACTOR, nest = TRUE)


p <- MC %>%
  group_by(value) %>%
  dplyr::summarise(RESHAB = survey_total(ifelse(variable == "CVE_ENT", RESHAB, 0), vartype = c("se", "cv", "ci"), level = 0.90, na.rm = TRUE),
                   RESREC = survey_total(ifelse(variable == "CVE_ENT", RESREC, 0), vartype = c("se", "cv", "ci"), level = 0.90, na.rm = TRUE),
                   INMIG = survey_total(ifelse(variable == "CVE_ENT", MIG, 0), vartype = c("se", "cv", "ci"), level = 0.90, na.rm = TRUE), 
                   EMIG = survey_total(ifelse(variable == "CVE_ENT5" & value %in% estados, MIG, 0), vartype = c("se", "cv", "ci"), level = 0.90, na.rm = TRUE)) %>%
  ungroup()  %>%
  mutate(MIGNETA = INMIG - EMIG, 
         MIGBRUTA = INMIG + EMIG,
         TASAINMI = ((INMIG/ 5) /((RESHAB + RESREC) / 2))*1000,
         TASAEMI = ((EMIG/ 5) /((RESHAB + RESREC) / 2))*1000, 
         TASAMIG = TASAINMI - TASAEMI,
         EFICACIA = MIGNETA - MIGBRUTA) 
```


# Nivel municipal {.tabset .tabset-pills}

## Población Total {.tabset .tabset-pills}

## Población 5 años y más {.tabset .tabset-pills}

## Migración reciente {.tabset .tabset-pills}


Se utiliza la paquetería `survey` para poder trabajar con la muestra del cuestionario ampliado, en la cual se selecciona a la población de 5 años y más.

### Muestreo Complejo (Opción 1)

````
```{r}`r ''`
TABLE DR_RESHAB
FREQ CVE_ENT
FILTER EDAD > 4 AND EDAD <= 130
TITLE "RESIDENCIA HABITUAL"
OUTPUTFILE XLS "Output\Residencia_Habitual_2020.xlsx" OVERWRITE
```
````

# Población Total

```{r}
Pob.Total <- read.xlsx(paste0(here::here(), "/Output/Indicadores de migracion interna 2020.xlsx"), startRow = 1, skipEmptyCols = TRUE,
                       sheet = "Población Total (Entidad)") 
```


```{r}
tabla <- Pob.Total %>%
  arrange(Pob_Total_Muestra) %>%
  droplevels() 

ent <- tabla %>% pull(CVE_ENT)

p <- tabla %>%
  ggplot(aes(x = order(factor(CVE_ENT, levels = ent)))) + 
  geom_line(aes(y = Pob_Total_Muestra, color = "LINE1")) +
  geom_ribbon(aes(ymin = Pob_Total_Muestra_low, ymax = Pob_Total_Muestra_upp), alpha = 0.2) + 
  geom_line(aes(y = Pob_Total_Censo, color = "LINE2")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Montserrat Medium", size = 20), 
        axis.text = element_text(family = "Montserrat Medium", size = 10), 
        axis.title = element_text(family = "Montserrat Medium", size = 12),
        legend.text = element_text(family = "Montserrat Medium"),
        legend.title = element_text(family = "Montserrat Medium")) + 
  scale_colour_manual(labels = c("Muestra", "Censo"),
                      values = c("LINE1" = "blue", "LINE2" = "red")) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  labs(title = "Población Total",
       y = "Población de Total",
       x = "Estados", 
       color = "", 
       fill = "")

p
path = paste0(here::here(), "/Graficos/Pob_Total a nivel estatal.pdf") 
ggexport(p, width = 9, height = 7, dpi = 400, filename = path, device = "cairo_pdf")      
```


# Población Total

```{r}
P.Nacimiento <- read.xlsx(paste0(here::here(), "/Output/Indicadores de migracion interna 2020.xlsx"), startRow = 6, skipEmptyCols = TRUE,
                          sheet = "Lugar de Nacimiento") 

P.Nacimiento <- P.Nacimiento %>%
  as.data.frame() %>%
  filter(NOM_ENT %nin% "Total")
names(P.Nacimiento)
```


## Residentes

```{r}
tabla <- P.Nacimiento %>%
  arrange(Residentes) %>%
  droplevels() 

ent <- tabla %>% pull(CVE_ENT)

p <- tabla %>%
  ggplot(aes(x = order(factor(CVE_ENT, levels = ent)))) + 
  geom_line(aes(y = Residentes, color = "LINE1")) +
  geom_ribbon(aes(ymin = Residentes_low, ymax = Residentes_upp), alpha = 0.2) + 
  geom_line(aes(y = Residentes_Censo, color = "LINE2")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Montserrat Medium", size = 20), 
        plot.subtitle = element_text(family = "Montserrat Medium", size = 15), 
        axis.text = element_text(family = "Montserrat Medium", size = 10), 
        axis.text.x = element_text(family = "Montserrat Medium", size = 8, angle = 90, hjust = 1), 
        axis.title = element_text(family = "Montserrat Medium", size = 12),
        legend.text = element_text(family = "Montserrat Medium"),
        legend.title = element_text(family = "Montserrat Medium")) + 
  scale_colour_manual(labels = c("Muestra", "Censo"),
                      values = c("LINE1" = "blue", "LINE2" = "red")) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  scale_x_discrete(limits = nom_estados) + 
  labs(title = "Población Nacimiento",
       subtitle = "Residentes en el estado",
       y = "",
       x = "", 
       color = "", 
       fill = "")

p
path = paste0(here::here(), "/Graficos/MNAC (Residentes) a nivel estatal.pdf") 
ggexport(p, width = 9, height = 7, dpi = 400, filename = path, device = "cairo_pdf")      
```


## Inmigrantes

```{r}
tabla <- P.Nacimiento %>%
  arrange(Inmigrantes) %>%
  droplevels() 

ent <- tabla %>% pull(CVE_ENT)
nom_ent <- tabla %>% pull(NOM_ENT)

p <- tabla %>%
  ggplot(aes(x = order(factor(CVE_ENT, levels = ent)))) + 
  geom_line(aes(y =Inmigrantes, color = "LINE1")) +
  geom_ribbon(aes(ymin = Inmigrantes_low, ymax = Inmigrantes_upp), alpha = 0.2) + 
  geom_line(aes(y = Inmigrantes_Censo, color = "LINE2")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Montserrat Medium", size = 20), 
        plot.subtitle = element_text(family = "Montserrat Medium", size = 15), 
        axis.text = element_text(family = "Montserrat Medium", size = 10), 
        axis.text.x = element_text(family = "Montserrat Medium", size = 8, angle = 90, hjust = 1), 
        axis.title = element_text(family = "Montserrat Medium", size = 12),
        legend.text = element_text(family = "Montserrat Medium"),
        legend.title = element_text(family = "Montserrat Medium")) + 
  scale_colour_manual(labels = c("Muestra", "Censo"),
                      values = c("LINE1" = "blue", "LINE2" = "red")) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  scale_x_discrete(limits = nom_estados) + 
  labs(title = "Población Nacimiento",
       subtitle = "Nacidos en otro estado (Inmigrantes)",
       y = "",
       x = "", 
       color = "", 
       fill = "")

p
path = paste0(here::here(), "/Graficos/MNAC (Inmigrantes) a nivel estatal.pdf") 
ggexport(p, width = 9, height = 7, dpi = 400, filename = path, device = "cairo_pdf")      
```


## Emigrantes

```{r}
tabla <- P.Nacimiento %>%
  arrange(Emigrantes) %>%
  droplevels() 

ent <- tabla %>% pull(CVE_ENT)
nom_ent <- tabla %>% pull(NOM_ENT)

p <- tabla %>%
  ggplot(aes(x = order(factor(CVE_ENT, levels = ent)))) + 
  geom_line(aes(y = Emigrantes, color = "LINE1")) +
  geom_ribbon(aes(ymin = Emigrantes_low, ymax = Emigrantes_upp), alpha = 0.2) + 
  geom_line(aes(y = Emigrantes_Censo, color = "LINE2")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Montserrat Medium", size = 20), 
        plot.subtitle = element_text(family = "Montserrat Medium", size = 15), 
        axis.text = element_text(family = "Montserrat Medium", size = 10), 
        axis.text.x = element_text(family = "Montserrat Medium", size = 8, angle = 90, hjust = 1), 
        axis.title = element_text(family = "Montserrat Medium", size = 12),
        legend.text = element_text(family = "Montserrat Medium"),
        legend.title = element_text(family = "Montserrat Medium")) + 
  scale_colour_manual(labels = c("Muestra", "Censo"),
                      values = c("LINE1" = "blue", "LINE2" = "red")) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  scale_x_discrete(limits = nom_estados) + 
  labs(title = "Población Nacimiento",
       subtitle = "Nacidos en otro estado (Emigrantes)",
       y = "",
       x = "", 
       color = "", 
       fill = "")

p
path = paste0(here::here(), "/Graficos/MNAC (Emigrantes) a nivel estatal.pdf") 
ggexport(p, width = 9, height = 7, dpi = 400, filename = path, device = "cairo_pdf")      
```


## Tasa de Inmigrantes

```{r}
T.MNac <- read.xlsx(paste0(here::here(), "/Output/Indicadores de migracion interna 2020.xlsx"), startRow = 6, skipEmptyCols = TRUE,
                    sheet = "T. MNac") 

T.MNac <- T.MNac %>%
  as.data.frame() %>%
  filter(NOM_ENT %nin% "Total")
names(T.MNac)
str(T.MNac)
```


```{r}
tabla <- T.MNac %>%
  arrange(Tasa_Inmigracion_Censo) %>%
  droplevels() 

ent <- tabla %>% pull(CVE_ENT)
nom_ent <- tabla %>% pull(NOM_ENT)

p <- tabla %>%
  ggplot(aes(x = order(factor(CVE_ENT, levels = ent)))) + 
  geom_text(aes(y = Tasa_Inmigracion, label = round(Tasa_Inmigracion, 1)), size = 2, vjust = -1.5) +
  geom_text(aes(y = Tasa_Inmigracion_Censo, label = round(Tasa_Inmigracion_Censo, 1)), size = 2, vjust = 1.5, color = "#808080") +
  geom_line(aes(y = Tasa_Inmigracion, color = "LINE1")) +
  geom_ribbon(aes(ymin = Tasa_Inmigracion_low, ymax = Tasa_Inmigracion_upp), alpha = 0.2) + 
  geom_line(aes(y = Tasa_Inmigracion_Censo, color = "LINE2")) +
  theme_minimal() + 
  theme(plot.title = element_text(family = "Montserrat Medium", size = 20), 
        plot.subtitle = element_text(family = "Montserrat Medium", size = 15), 
        axis.text = element_text(family = "Montserrat Medium", size = 10), 
        axis.text.x = element_text(family = "Montserrat Medium", size = 8, angle = 90, hjust = 1), 
        axis.title = element_text(family = "Montserrat Medium", size = 12),
        legend.text = element_text(family = "Montserrat Medium"),
        legend.title = element_text(family = "Montserrat Medium")) + 
  scale_colour_manual(labels = c("Muestra", "Censo"),
                      values = c("LINE1" = "blue", "LINE2" = "red")) +
  #scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  scale_x_discrete(limits = nom_ent) + 
  labs(title = "Tasa de Inmigración",
       subtitle = "Nacidos en otro estado (Inmigrantes)",
       y = "",
       x = "", 
       color = "", 
       fill = "")

p
path = paste0(here::here(), "/Graficos/MNAC (Tasa de inmigracion) a nivel estatal.pdf") 
ggexport(p, width = 9, height = 7, dpi = 400, filename = path, device = "cairo_pdf")      
```



# Población Municipal

```{r}
MRes5 <- read.xlsx(paste0(here::here(), "/Output/Indicadores de migracion interna 2020.xlsx"), startRow = 6, skipEmptyCols = TRUE,
                   sheet = "Lugar de residencia (Municipio)") 

MRes5 <- MRes5  %>%
  as.data.frame() %>%
  filter(NOM_MUN %nin% "Total")
names(MRes5)
```


## Residentes

```{r}
tabla <- MRes5 %>%
  arrange(Inmigrantes_Muestra) %>%
  filter(MUN_Muestra %in% "Municipio muestreado") %>%
  droplevels() %>%
  slice(1:200) 

mun <- tabla %>% pull(CVE_MUN)

p <- tabla %>%
  ggplot(aes(x = order(factor(CVE_MUN, levels = mun)))) + 
  geom_line(aes(y = Inmigrantes_Muestra, color = "LINE1")) +
  geom_ribbon(aes(ymin = Inmigrantes_Muestra_low, ymax = Inmigrantes_Muestra_upp), alpha = 0.2) + 
  geom_line(aes(y = Inmigrantes_Censo, color = "LINE2")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Montserrat Medium", size = 20), 
        plot.subtitle = element_text(family = "Montserrat Medium", size = 15), 
        axis.text = element_text(family = "Montserrat Medium", size = 10), 
        axis.text.x = element_text(family = "Montserrat Medium", size = 8, angle = 90, hjust = 1), 
        axis.title = element_text(family = "Montserrat Medium", size = 12),
        legend.text = element_text(family = "Montserrat Medium"),
        legend.title = element_text(family = "Montserrat Medium")) + 
  scale_colour_manual(labels = c("Muestra", "Censo"),
                      values = c("LINE1" = "blue", "LINE2" = "red")) +
  #scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  # scale_x_discrete(limits = mun) + 
  labs(title = "Migración reciente 2015 - 2020",
       subtitle = "Inmigrantes (Residencia en otro estado)",
       y = "",
       x = "", 
       color = "", 
       fill = "")

p
path = paste0(here::here(), "/Graficos/MNAC (Residentes) a nivel municipal.pdf") 
ggexport(p, width = 9, height = 7, dpi = 400, filename = path, device = "cairo_pdf")      
```

