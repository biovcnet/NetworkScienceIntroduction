# NetworkScienceIntroduction

This is some working code that pulls data from Tara Oceans and performs some analysis, generates some plots and data that can be output to cytoscape.

You need the following to get it to work:

R and RStudio

the following packages installed

```{r}
install.packages(tidyverse) # for plotting and wrangling data

install.packages(otuSummary)
install.packages(reshape2) # has the melt function, which I use to wrangle data
install.packages(psych) # for calculating regular correlations with p values
```


You also need SpiekEasi, which requires `install.packages(devtools)`
install.packages(SpiecEasi) # Has sparcc and also does clr transforms

```{r}
#library(devtools)
#install_github("zdk123/SpiecEasi")
```