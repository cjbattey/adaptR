library(magrittr);library(ggplot2);library(viridis);library(reshape)

#time-variant selection
run_sim <- function(gen=100,p=0.5,W=fitness,nPop=2,m=0,stats=c("p","Fst"),Uab=0,Uba=0,
                    infinitePop=F,continue=F){
  withProgress(message="simulating populations...",value=0,{
  allele.freq <- data.frame(matrix(ncol=4*nPop)) #initialize summary stat matrix
  allele.freq[1,(1:nPop)] <- rep(p,nPop) #starting allele freqs
  for(i in 1:gen){
    if(i<=W$end_gen[1]){
      Waa <- W$AA[1]
      Wab <- W$AB[1]
      Wbb <- W$BB[1]
      n <- W$n[1]
    } else if(i>W$gen[1] & i<=W$end_gen[2]){
      Waa <- W$AA[2]
      Wab <- W$AB[2]
      Wbb <- W$BB[2]
      n <- W$n[2]
    } else if(i>W$end_gen[2] & i<=W$end_gen[3]){
      Waa <- W$AA[3]
      Wab <- W$AB[3]
      Wbb <- W$BB[3]
      n <- W$n[3]
    }
    mean.p <- as.numeric(rowMeans(allele.freq[i,(1:nPop)]))
    for(j in 1:nPop){
      p <- allele.freq[i,j]
      p <- (1 - Uab) * p + Uba * (1 - p) #mutation
      p <- p*(1-m)+m*mean.p # migration
      q <- 1-p
      w <- p*p*Waa+2*p*q*Wab+q*q*Wbb #population average fitness
      freq.aa <- (p*p*Waa)/w #post-selection genotype frequencies (weighted by relative fitness)
      freq.ab <- (2*p*q*Wab)/w
      Naa <- rbinom(1,n,freq.aa)
      if(freq.aa<1){ 
        Nab <- rbinom(1,(n-Naa),(freq.ab/(1-freq.aa)))
      }
      else {
        Nab <- 0
      }
      Nbb <- n-(Naa+Nab)
      p <- ((2*Naa)+Nab)/(2*n)
      q <- 1-p
      allele.freq[(i+1),j] <- p #new p after drift in columns 1:nPop
      allele.freq[(i+1),(j+nPop)] <- Nab/n #Ho in columns (nPop+1):(nPop*2)
      allele.freq[(i+1),(j+2*nPop)] <- 2*p*q #He in columns (nPop*2+1):nPop*3
      allele.freq[(i+1),(j+3*nPop)] <- w #pop mean fitness in last columns
    } #end populations loop
    incProgress(1/W$end_gen[3])
  } #end generations loop
  }) #end progress bar
  #summary stats
  names <- c()
  for(i in 1:nPop){names[i]<-paste0("p",i)}
  for(i in (nPop+1):(2*nPop)){names[i]<-paste0("Ho",i-nPop)}
  for(i in (nPop*2+1):(3*nPop)){names[i]<-paste0("He",i-2*nPop)}
  for(i in (nPop*3+1):(4*nPop)){names[i]<-paste0("W",i-3*nPop)}
  colnames(allele.freq) <- names
  allele.freq$meanHo <- rowMeans(allele.freq[(nPop+1):(nPop*2)])
  allele.freq$meanHe <- rowMeans(allele.freq[(nPop*2+1):(nPop*3)])
  allele.freq$Fis <- 1-(allele.freq$meanHo/allele.freq$meanHe)
  allele.freq$mean.p <- rowMeans(allele.freq[1:nPop]) 
  allele.freq$Hs <- rowMeans(allele.freq[(nPop*2+1):(nPop*3)])
  allele.freq$Ht <- 2*allele.freq$mean.p*(1 - allele.freq$mean.p)
  allele.freq$Fst <- (allele.freq$Ht-allele.freq$Hs)/allele.freq$Ht
  allele.freq$Fst[allele.freq$Fst<0] <- 0
  allele.freq$gen <- 0:gen
  allele.freq$Fst[allele.freq$gen == 0] <- NA
  return(allele.freq)
}
#format for plotting
meltPlotData <- function(allele.freq.df=allele.freq.df,gen=300,nPop=10,stats="p"){
  df <- melt(allele.freq.df,id.vars = "gen")
  df$dataType <- gsub("[[:digit:]]","",df$variable)
  df <- subset(df,dataType %in% stats)
  return(df)
}

make_fitness_matrix <- function(aa1,ab1,bb1,n1,ngen1,
                                aa2,ab2,bb2,n2,ngen2,
                                aa3,ab3,bb3,n3,ngen3){
 fitness <- data.frame(matrix(c(aa1,ab1,bb1,n1,ngen1,
                                aa2,ab2,bb2,n2,ngen2+ngen1,
                                aa3,ab3,bb3,n3,ngen3+ngen2+ngen1),nrow=3,byrow=T))
 colnames(fitness) <- c("AA","AB","BB","n","end_gen")
 fitness$epoch <- 1:3
 return(fitness)
}




