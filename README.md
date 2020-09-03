# Resumption-of-businsess
Evaluate the risk of resumption of business for the states of New York, New Jersey, and Connecticut via a pre-symptomatic and asymptomatic transmission model of COVID-19

## 1. Data 
### 1) Abstract
Our data source consists of cumulative confirmed cases for New York, New Jersey, Connecticut, and California from March 13 to May 24 which were collected from New York Times. Those data acted as training data to create our proposed models. Also, the cumulative cases for those four states from May 25 to June 6 as test data were used to validate the models. These data were available in their offical websites. This enabled us to evaluate the risk of resumption of business for the states of New York, New Jersey and Connecticut using California as benchmark of resumption of businsess in the real world. 

### 2) Availability
The data to reproduce our results are available.

### 3) Description
The data incorporte 4 `.RData` files and 1 `.csv` file.
- The cumulative confirmed cases in the states of the United States were recorded and collected in a single `.csv` file (`us-states.csv`) 
- The cumulative confirmed cases in each of states were complied in each of four `.rda` files (`CT_dat.RData`; `Ca_dat.RData`;`NJ_dat.RData`;`NY_dat.RData`)

### 4) Permissions
The data were orignially collected by the authors.

----
## 2. Code
### 1) Abstract
The codes incorported all the scripts to reproduce the analysis in the paper. 

### 2) Reporducibility
- The different classes of counties were obtained by runing `clustering.R`.
- The singificant contributing factors related to increaning the mortiliay of COVID-19 were identified in different classes of counties by runing `var_glm.R`.


----
## 3. Paper

- https://www.medrxiv.org/content/10.1101/2020.05.18.20105
