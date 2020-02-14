# R PACKAGE - FFP

#' Upload Function
#' @description The function upload a file suitable for the package in a csv format.
#' @return As csv file, the data we want to use for peculiar package purposes.
#' @export
#' @import devtools
#' @examples DATA <- upload()
upload <- function(){
  data <- read.csv(system.file("extdata","DATAR.csv",package= "FFP"))
  return(data)
}




#' Suitable Format Function
#' @description The function retrieves the data we import, attributing a date vector to the rows.
#'
#' @param d The dataframe of interest for the package.
#' @param vct_date A vector with dates to attribute to the rows of the dataframe.
#'
#' @return A new dataframe , obtained by eliminating the first column and denominating rows with the dates we chose.
#' @export
#' @import devtools
#' @examples tab <- frmtb(data,c('30/06/2016','30/06/2017','30/06/2018'))
frmtb <- function(d,vct_date) {
  tab <- as.data.frame(d)
  tab[1] <- NULL
  row.names(tab) <- as.Date((vct_date),format="%d/%m/%Y")

  return(tab)

}




#' Default Relevant Variables Function
#' @description The function creates a new dataframe with selected columns retrived by vct_var.
#' @param df A dataframe of our interest.
#' @param vct_var The vector with (ordered) columns we want to extract from the original dataframe.
#'
#' @return A new dataframe with the columns we assigned to the vct_var.
#' @export
#' @import devtools
#' @examples RR <-rel_var(tab,c(2,3,5,6,7) RE <- rel_var(tab,c(10,12,13,14,15))
rel_var <- function(df,vct_var){
  df_mat <- as.matrix(df)
  mat_rel <- df_mat[,vct_var]
  df_rel <- as.data.frame(mat_rel)
  return(df_rel)
}




#' Columns Transfer Function
#' @description The function adds columns with row sums to our dataframes of interest.
#' @param df The dataframe we obtained from rel_vaRR() and rel_vaRE().
#'
#' @return A new columns to be add to RR and RE whose values are retrived as the aggregate rows sum of all of the columns.
#' @export
#' @import devtools
#' @examples RR$AggrRev <- trnsfR(RR) RE$AggrExp <- trnsfR(RE)
trnsfR <- function(df) {
  mat_df <- as.matrix(df)
  num_var <- t(apply(mat_df,1,as.numeric))
  rel_x <- (apply(num_var,1,sum))
  return(
    c(as.numeric(rel_x))
  )
}




#' Break-Even Calculator
#' @description The function print the result of the sum between Aggregate Revenues and Aggregate Expenses (i.e. "break-even requirement" under the UEFA Financial Fair Play Regulation 2019), computed for the 3-years monitoring period.
#' @param df1 The dataframe we obtained from rel_vaRR(), including the column obtained by running the trnsfR() function.
#' @param df2 The dataframe we obtained from rel_vaRE(), including the column obtained by running the trnsfR() function.
#'
#' @return A numeric value, indicating the break-even value for the 3-years monitoring period under inspection.
#' @export
#' @import devtools
#' @examples be <- sum_be(RR,RE)
sum_be <- function(df1,df2) {
  output1 <- sum(as.numeric(df1[,6]))
  output2 <- sum(as.numeric(df2[,6]))
  return(
    b <- as.numeric(output1+output2)
  )
}




#' Break-Even Result Evaluator
#' @description The function prints as logical the evaluation of the numeric break-even results: 'SURPLUS' if positive, 'ACCEPTABLE' if falling within the limits established by the UEFA Commission, 'SANCTION' if negative exceeding €30million threshold.
#' @param b A numeric value we obtained from sum_be().
#'
#' @return As logical, an evaluation on the numeric break-even value we have as argument of the function, following the UEFA regulations, according to which a deficit of more than €30million on a 3-years monitoring period cannot be accepted by the Commission.
#' @export
#' @import devtools
#' @examples
be_judgf<- function(b) {
  if (b > 0) {
    print('SURPLUS')
  } else if (b >= -30000000){
    print('ACCEPTABLE')
  }
  else {
    print('SANCTION')
  }
}




#' Yearly Break-Even Function
#' @description The function creates a vector by applying a sum by row within different columns with aggregate values of interest (for break-even purposes) of different dataframes.
#' @param df1 The dataframe we obtained from rel_vaRR(), including the column obtained by running the trnsfR() function.
#' @param df2 The dataframe we obtained from rel_vaRE(), including the column obtained by running the trnsfR() function.
#' @param x Number of the column with Aggregate Values in df1.
#' @param y Number of the column with Aggregate Values in df2.
#'
#' @return A vector, whose elements for yearly Break-even (difference by rows of two Aggregate Values columns)
#' @export
#' @import devtools
#' @examples z <- c(beyR(df1,df2,6,6))
beyR <- function(df1,df2,x,y) {
  u <- as.vector(rel_var(df1,c(x)))
  w <- as.vector(rel_var(df2,c(y)))
  return(
    t(as.vector(u+w))
  )
}




#' Plotly Bars Graph Generator
#' @description The function generates an interactive plotly bars graph. On the x-bar we put the rownames as dates of the dataframe of our interest. On the first y-bar we insert the aggregate values of Relevant Expenses ordered by year. On the second y-bar we insert the aggregate values of Relevant Revenues by year. On the third y-bar we insert the vector we obtain by running the beyR() function.
#' @param z A vector with yearly Break-even values.
#'
#' @return An interactive bars graph with 3 parameters of interest.
#' @export
#' @import devtools plotly dplyr
#' @examples
grph <- function(z){
  plot_ly() %>%
    add_bars(
    x = c("30-06-2016", "30-06-2017", "30-06-2018"),
    y = c(-212375000,-204107000,-213397765),
    base = 0,
    marker = list(
      color = 'green'
    ),
    name = 'Relevant Expenses'
  ) %>%
  add_bars(
    x = c("30-06-2016", "30-06-2017", "30-06-2018"),
    y = c(266764000,242312000,277266291),
    base = 0,
    marker = list(
      color = 'orange'
    ),
    name = 'Relevant Revenues'
  ) %>%
  add_bars(
    x = c("30-06-2016", "30-06-2017", "30-06-2018"),
    y = c(z),
    base = 0,
    marker = list(
      color = 'red'
    ),
    name = 'Yearly BE'
  )
}




#' Default Ordered Function
#' @description The function runs the all the function implemented in the FFP.R package in the logical order.
#' @return The function returns the evaluation as logical of the break-even result for the 3-years  monitoring period, together with the interactive bars graph.
#' @export
#' @import devtools plotly dplyr
#' @examples
completefunc <- function(){
  data <- upload()
  tab <- frmtb(data,c('30/06/2016','30/06/2017','30/06/2018'))
  RR <- rel_var(tab,c(2,3,5,6,7))
  RE <- rel_var(tab,c(10,12,13,14,15))
  RR$AggrRev <- trnsfR(RR)
  RE$AggrExp <- trnsfR(RE)
  rel_var(RR,c(6))
  rel_var(RE,c(6))
  b <- sum_be(RR,RE)
  be_judgf(b)
  z <- c(beyR(RR,RE,6,6))
  return(
    grph(z)
  )
}




#' Relevant Accounts ShinyAppAsr
#'
#' @param server Default parameter by shinyApp interface
#' @param ui Default parameter by shinyApp interface
#'
#' @return An interactive page with bar graphs and selection of relevant accounts through a sidebar panel.
#' @export
#' @import shiny ggplot2
#' @examples
shinyapp_asr <- function(server,ui){
  data <- upload()
  tab <- frmtb(data,c('30/06/2016','30/06/2017','30/06/2018'))
  RR_RE <- rel_var(tab,c(2,3,5,6,7,10,12,13,14,15))
  server <- function(input,output) {
    output$RR_REplot <- renderPlot({
      ggplot(RR_RE)+ geom_bar(mapping=aes(x= c("2016","2017","2018"), y=RR_RE[,input$RR_RE]*0.00001), stat= "identity", color=c('dark red','orange','black'),fill=c('dark red','orange','black'),width= 0.7)+ labs(title= 'ASR 3-years financial review',x='Financial Years',y='Account Balance')
    })
  }

  ui <- fluidPage(
    titlePanel("Relevant Figures for BE computation"),
    sidebarLayout(
      sidebarPanel(
        selectInput("RR_RE","Relevant Accounts:",
                    choices=colnames(RR_RE)),
        hr(),
        helpText("Data from ASR Financial Statements 2016-2018 (expressed in € thousand).")

      ),
      mainPanel(
        plotOutput("RR_REplot")
      )
    )

  )
  return(
    shinyApp(ui=ui,server=server)
  )
}




