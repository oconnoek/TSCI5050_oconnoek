#'---
#' title: "TSCI 5050: Simulating a Data Set"
#' author: 'Erin Stewart, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;nrows <- 200;seed <- 22#replicate same random numbers- set.seed 
prob_missing=c(.99,.01)
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(ggplot2); # visualisation
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(broom); # allows to give clean dataset
library(dplyr); #add dplyr library

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};

# Import Data ----
datafile0 <- "Data/R test data.xlsx"
Dat0 <- import(datafile0)
Dat0[1,] #Take the 1st row of the data set
Dat0[1,2] #Row 1, Col 2
Dat0[1,2:5] #Row 1, Cols 2 through 5
Dat0[1,c(2,5)] #Row 1, Cols 2 and 5 #C command is helpful for non-sequential data
Dat0[c(1,1),]#Row 1 twice, All Cols
Dat0[rep(1,6),] #Row 1, repeated 6 times, All Cols 
#Rep command is helpful for repeating large amounts of data as the c command must be manually input

set.seed(seed) #Use this func to replicate the same vector of random numbers; seed was defined on line 25
rnorm(nrows, mean=900, sd=250) #Create a single column vector of random values; nrows was defined on line 25

#Modify Data----
#Create a new data frame from Dat 0 with row 1 repeated nrows times for all columns
#Redefine Dat1 by replacing the values in specified columns with random values
  #Across performs a specific action y given a condition x. 
    #In this case, if a value is numeric, then replace it with a random value with the given mean and sd
    #n() represents the number of rows in the current block of data
    #.x is a placeholder for the column name - this syntax only works within mutate, transmute, and summarize

Dat1 <- Dat0[rep(1,nrows),] %>% 
        mutate(across(where(is.numeric),~rnorm(n(), mean=.x, sd=1+.x/12))
        , ID= sprintf("EX-%04d",sample(1:1000,n())) # Zero pad to 4 places with prefix 'EX-'
        ,`Specimen ID`= sprintf("%03d-%03d-%04d-%d"#`These quotes are needed for variable names that include a space`
              ,sample(1:100,n(), replace = TRUE)
              ,sample(1:100,n(),replace = TRUE)
              ,sample(1:1000,n(),replace = TRUE)
              ,sample(1:9,n(),replace = TRUE))
        , 'IHG'= sample(c(I, II, III, IV),n(),replace = TRUE) 
        , PIN = seq_len(n())
        , `CD4 ABS`=round(rnorm(n(), mean=900, sd=250))
        , `CD8 ABS`=round(rnorm(n(), mean=500, sd=20))
        , `CD4/8 Ratio`=(`CD4 ABS`/`CD8 ABS`)
        , WBC=rnorm(n(), mean=4.9, sd=.26)
        , RBC=rnorm(n(), mean=8.7, sd=.24)
        , across(everything(),~ifelse(sample(1:0,n(),replace=TRUE,prob = prob_missing),.x, NA))
        , Date=as.Date(Date,"%m/%d/%Y")-sample(0:2,n(),replace=TRUE) 
        )

#sprintf("name = %s, age = %d, percentile = %f %%", "Ciera", 30, 98.5)
# s = string, d = integer, f = fraction

#Generate a Date column
#as.Date("11/30/2024","%m/%d/%Y") #Used to define format of date
#class(as.Date("11/30/2024","%m/%d/%Y")) #What class of data is this? May use function to confirm that you've created a date.
#is(as.Date("11/30/2024","%m/%d/%Y"),"Date") ## Alternatively, you can ask is argument X class Y? Returns TRUE or FALSE.
#as.Date("11/30/2024","%m/%d/%Y")-sample(0:2,size=3)

SummaryDat1 <-summarize(group_by(Dat1,Date),`CD4 ABS`=mean(`CD4 ABS`),`CD8 ABS`=mean(`CD8 ABS`))

#Write Data ----
export(Dat1,"Data/Simulated Data.xlsx")
#....scatter plot, visualize:
select(Dat1,!any_of(c("ID","Specimen ID","PIN","VISIT","Notes")))%>% ggpairs

