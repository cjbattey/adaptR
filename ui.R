#adaptR shiny UI
library(magrittr);library(reshape);library(ggplot2);library(rhandsontable)
shinyUI(fluidPage(
  titlePanel("adaptR?"),
  fluidRow(
    column(3,
           numericInput("p","Starting allele frequency A",value=0.1),
           sliderInput("m","Migration Rate",0,min=0,max=0.2),
           numericInput("nPop","Number of Populations",10),
           checkboxInput("infinitePop","Infinite Population (no drift)",value = F),
           checkboxGroupInput(inputId="plotStats",label="plot:",choices=c("p","He","Hs","Ht","Fst","W"),inline=T,selected="p"),
           actionButton("go","Run Simulation")
           ),
    column(9,
           #textOutput("test"))),
           plotOutput("plot"))),
  fluidRow(column(4,
                  helpText('adaptR simulates change in the frequency of genetic variants at a single 
                            site in a diploid genome across multiple populations and time periods. Each row in the 
                            table to the right represents a different time period, delimited by the vertical red lines
                            in the graph above. The first three columns represent the relative fitness of each genotype in 
                            each time period, the "n" column is the population size, and the "gen" column is the number
                            of generations for that time period. To run a simulation under new parameters, just edit 
                            the table as desired and hit "go".'),
                  helpText('Source code for adaptR can be found here: https://github.com/cjbattey/adaptR')),
           column(8,
                  rHandsontableOutput("hot"))
           )
  
  # fluidRow(column(3,""),
  #          column(3,
  #                 sliderInput("Waa1","AA",1,min=0,max=1),
  #                 sliderInput("Wab1","AB",1,min=0,max=1),
  #                 sliderInput("Wbb1","BB",1,min=0,max=1),
  #                 numericInput("n1","Population Size",10000),
  #                 numericInput("ngen1","Generations",100)),
  #          column(3,
  #                 sliderInput("Waa2","AA",1,min=0,max=1),
  #                 sliderInput("Wab2","AB",.9,min=0,max=1),
  #                 sliderInput("Wbb2","BB",.9,min=0,max=1),
  #                 numericInput("n2","Population Size",500),
  #                 numericInput("ngen2","Generations",100)),
  #          column(3,
  #                 sliderInput("Waa3","AA",1,min=0,max=1),
  #                 sliderInput("Wab3","AB",1,min=0,max=1),
  #                 sliderInput("Wbb3","BB",1,min=0,max=1),
  #                 numericInput("n3","Population Size",10000),
  #                 numericInput("ngen3","Generations",100)))
           ))
