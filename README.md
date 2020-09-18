# Resumption-of-business
Evaluate the risk of resumption of business for the states of New York, New Jersey, and Connecticut via a pre-symptomatic and asymptomatic transmission model of COVID-19

## 1. Data 
### 1) Abstract
Our data source consists of cumulative confirmed cases for New York, New Jersey, Connecticut, and California from March 13 to May 24 which were collected from New York Times. Those data acted as training data to create our proposed models. Also, the cumulative cases for those four states from May 25 to June 6 as test data were used to validate the models. These data were available in their offical websites. This enabled us to evaluate the risk of resumption of business for the states of New York, New Jersey and Connecticut using California as benchmark of resumption of businsess in the real world. 

### 2) Availability
The data to reproduce our results are available.

### 3) Description
The data incorporte 4 `.RData` files.
- The cumulative confirmed cases in each of four states were complied in each of four `.RData` files (`CT_dat.RData`; `CA_dat.RData`;`NJ_dat.RData`;`NY_dat.RData`)

### 4) Permissions
The data were orignially collected by the authors.

----
## 2. Code
### 1) Abstract
The codes incorported all the scripts to reproduce the analysis in the paper. 

### 2) Reporducibility
- The estimation of Susceptible individuals (S), unidentified infectious (I), self-healing without being confirmed (H), and confirmed cases for the Equation (1) in the paper by runing `Modeling_function.R`.
- The simulation of resumption of busniess by runing `Resumption.R`.
- The evaluation of resumptiong of busniess for each of four states (New York, New Jersey, Connecticut, and California) inadividually by running `NY.R`;`NJ.R`;`CT.R`; and `CA.R`.

----
## 3. Paper

- https://www.medrxiv.org/content/10.1101/2020.05.16.20103747v5
