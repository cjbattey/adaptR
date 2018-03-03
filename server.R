#adaptR server script (see adaptR.R for simulation function)

shinyServer(function(input,output,session){

library(magrittr);library(reshape);library(ggplot2);library(rhandsontable)
source("adaptR.R")

# fitness <- reactive({make_fitness_matrix(aa1=input$Waa1,ab1=input$Wab1,bb1=input$Wbb1,n1=input$n1,ngen1=input$ngen1,
#                                          aa2=input$Waa2,ab2=input$Wab2,bb2=input$Wbb2,n2=input$n2,ngen2=input$ngen2,
#                                          aa3=input$Waa3,ab3=input$Wab3,bb3=input$Wbb3,n3=input$n3,ngen3=input$ngen3)})

fitness <- reactive({
  if(!is.null(input$hot)){
    fitness <- hot_to_r(input$hot)
  } else if(is.null(input$hot)){
    fitness <- data.frame(AA=c(1,1,1),AB=c(1,.95,1),BB=c(1,.9,1),n=c(10000,100,1000),end_gen=c(100,200,300))
  }
  fitness
})

output$hot <- renderRHandsontable({
  rhandsontable(fitness(),useTypes = F)
})

df <- eventReactive(input$go,ignoreNULL = F,{run_sim(nPop=input$nPop,p=input$p,gen=fitness()$end_gen[3],W=fitness(),
                m=input$m)})
df2 <- reactive({meltPlotData(allele.freq.df = df(),stats=input$plotStats,nPop = input$nPop,gen = sum(input$ngen1,input$ngen2,input$ngen3))})

 # test <- reactive({fitness()})
 # output$test <- renderTable(test())

a <- eventReactive(input$go,ignoreNULL = F,{
      print(
        ggplot(df2(),aes(x=gen,y=value,col=variable))+
          facet_wrap(~dataType)+
          theme_bw()+ylim(0,1)+
          theme(legend.position="none",
                panel.grid.minor=element_blank(),
                axis.text=element_text(size=12),
                axis.title=element_text(size=12),
                strip.background = element_blank(),
                strip.text=element_text(size=12))+
          scale_color_viridis(discrete=T)+
          xlab("Generation")+ylab("Frequency of allele A")+
          geom_vline(aes(xintercept=fitness()$end_gen[1]),col="red")+
          geom_vline(aes(xintercept=fitness()$end_gen[2]),col="red")+
          geom_line()
      )
  })

output$plot <- renderPlot(a())

})