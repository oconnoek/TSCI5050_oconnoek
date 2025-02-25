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
DataFile0 <-"Data/R test data.xlsx"
Dat0 <- import(DataFile0)
Dat0[1,]#first row 
Dat0[1,2:5]#first row, second column thru 5th column
Dat0[1, c(2,2)]#first row, second column prepeat 2x
Dat0[c(1,1)]#column 1 twice
rep(sqrt(3), 5)#repeat value, 5 times
Dat0[rep(1,6),]#expand to 6 rows; need comma after
Dat1 <- Dat0[rep(1,nrows),]
mutate(Dat1)
#mutate(Dat1, `CD4 ABS`=12)
#mutate(Dat1, `CD4 ABS`=rnorm(200, mean = 900, sd = 250))
#Dat1 <- mutate(Dat1, `CD4 ABS`=rnorm(n(), mean = 900, sd = 250))# n() is the current rows in data set
#Dat2 <- mutate(Dat1, `CD8 ABS`=rnorm(n(), mean = 500, sd = 100))
Dat1 <- mutate(Dat1 
               , ID = sprintf("EX-%04d", sample(1:1000, n()))
               , `CD4 ABS`=round(rnorm(n(), mean = 900, sd = 250))
               , `CD8 ABS`=rnorm(n(), mean = 500, sd = 100)
               , `WBC`=rnorm(n(), mean = 4.9, sd = .25)
               , `RBC`=rnorm(n(), mean = 4.6, sd = .3)
               , `Hct %`=rnorm(n(), mean = 42, sd = 1) 
               
               ) #round is function to turn values to whole integers
