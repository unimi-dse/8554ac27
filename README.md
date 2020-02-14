# Computing the Financial Fair Play Break-Even Requirement on a 3-year monitoring period 
## by Silvia Fratarcangeli
**Install**
```{r}
library(devtools)
library(plotly)
library(dplyr)
library(ggplot2)
library(shiny)
```



**Uploading and formatting table**
```{r}
upload()
frmtb(df, vct_date)
```

**Creating distinct and Relevant dataframes**
```{r}
rel_var(df,vct_var)
df1$AggrX <- trnsfR(df1)
df2$AggrY <- trnsfR(df2)
```
WARNING: for subject matters, assign two distinct dataframes, e.g. RR=Relevant Revenues and RE=Relevant Expenses to `rel_var(df,vct_var)` so as to make BE(defined as RR+RE) computations easier.

**Computing break-even numeric result for 3-years monitoring period and printing as logical the final evaluation according to threshold established by the UEFA regulations**
```{r}
sum_be(df1,df2)
be_judgf(b)
```

The break-even result for the 3-years monitoring period was `be <-sum_be(df1,df2)`. The evaluation to be taken into account from the UEFA Commission was `be_judgf(be)`.

**Creating a vector with yearly break-even resuts and plotting the results, together with others variable of interest, into an interactive bars graph using plotly library**
```{r}
rel_var(df,vct_var)
z <-c(beyR(df2,df2,x,y))
grph(z)
```

**Running the functions included in the FFP package in the correct order so to obtain the final results printed as logical, together with the interactive bars graph**
```{r}
completefunc()
```

**Launching the ShinyApp interface. The page will plot bar graphs on a 3-years comparison scale, for any relevant account we choose from the sidebar.**
```{r}
shinyapp_asr(server,ui)
```

