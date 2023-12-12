# plotforme
An **automated** plotting package that will generate tailored plots with the options of using Ente's theme or a user specified theme.

All you need to know are the dataframe and variable names! The functions will figure out the most suitable plot for you.

The user **does not** need to know the class of the input variables, the functions will determine the class automatically. 


#### R installation Instructions
```
install.packages("devtools")
library(devtools)
devtools::install_github("entekang/plotforme")
library(plotforme)
```
#### Example for visualizing one variable
```
onevar(diamonds, price)
```

#### Example for visualizing two variables
```
twovar(diamonds, price, color)
```

#### Example for visualizing three variables
```
threevar(diamonds, price, color, cut)
```
