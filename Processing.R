#'---
#' title: "TSCI 5050: Processing a Data Set"
#' author: 'Ciera Price'
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
# 
debug <- 0;nrows <-200;seed <-22;

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
prob_missing=c(.99,.01)

datafile1<-"Data/Simulate.xlx"
simdat <- import(datafile1) %>% mutate(train=sample(c(TRUE,FALSE),n(),replace = TRUE))
#simdat <-mutate(simdat,train=sample(c(TRUE,FALSE),n(),replace = TRUE))

select(Dat1,!any_of(c("ID","Specimen ID","PIN","VISIT","Notes"))) %>% ggpairs
#slice()
#....slice to cut data for training then testing, for modeling
##Read in the data Simulated Data
c()
