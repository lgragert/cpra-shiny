library(shiny)
library(stringr)
library(data.table)
library(cprad2)
library(DT)
options(DT.options = list(searching = FALSE))

shinyServer(function(input, output, session) {
  
  data <- reactiveValues(s = NULL, all = NULL, ag = NULL, 
    df.ag = data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA),
    df.all = data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA))
  
  table.allele.sero<-data.table(table.allele.sero)
  table.allele.sero<-table.allele.sero[,Allele:=as.character(Allele)] #PROB: strings turning into factors?
  table.allele.sero<-table.allele.sero[,Locus.Antigen:=as.character(Locus.Antigen)]
  table.allele.sero<-setkey(table.allele.sero,Locus.Antigen) #set key
  
  observeEvent(input$run, {
    data$s <- NULL
    data$ag <- NULL
    data$all <- NULL
    data$df.ag <- data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA)
    data$df.all <- data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA)
    
    #Process input string; separate into antigens and alleles 
    if (!(is.null(input$string))) {
      data$s <- unlist(str_split(input$string,"[ ,;//]"))
    } #if
    
    for (i in 1:length(data$s)) {
      if (grepl("\\*",data$s[i])) {
        data$all<-c(data$all,data$s[i])
      } else { data$ag<-c(data$ag,data$s[i]) }
    } #for
    
    #If antigens are input
    if (!(is.null(data$ag))) {
      
      #Determine equivalent antigens for input antigens
      strip<-function(ag,locus) { #if empty will return numeric(0)
        x<-ag[grep(locus,ag)]
        x<-as.numeric(str_extract(x,"\\d+"))
        return(x)
      } #fun
      A<-unlist(sapply(data$ag,strip,"A"))
      if (length(A)==0) { A<-NA }
      B<-unlist(sapply(data$ag,strip,"B"))
      if (length(B)==0) { B<-NA }
      C<-unlist(sapply(data$ag,strip,"C"))
      if (length(C)==0) { C<-NA }
      Q<-unlist(sapply(data$ag,strip,"DQ"))
      if (length(Q)==0) { Q<-NA }
      R<-unlist(sapply(data$ag,strip,"DR"))
      if (length(R)==0) { R<-NA }
      excludes<-list(A=A,B=B,C=C,Q=Q,R=R)
      excludes2<-equiv(table.equiv,excludes)
      
      #Display antigens as data.frame via DT package
      for (i in 1:length(excludes2)) {
        for (j in 1:length(excludes2[[i]])) {
          if (!is.null(excludes2[[i]][j])) {
            data$df.ag[j,i]<-excludes2[[i]][j]
          } else { data$df.ag[j,i]<-NA }
        } #for j
      } #for i
      colnames(data$df.ag)<-c("A","B","C","DQ","DR")
      
      #Convert antigens to alleles
      convert<-function(ag,locus) { #if empty will return numeric(0)
        locuskey<-paste0(locus,ag)
        temp<-table.allele.sero[list(locuskey),"Allele"]
      } #fun
      A<-unlist(sapply(excludes2["A"],convert,"A"))
      B<-unlist(sapply(excludes2["B"],convert,"B"))
      C<-unlist(sapply(excludes2["C"],convert,"C"))
      Q<-unlist(sapply(excludes2["Q"],convert,"DQ"))
      R<-unlist(sapply(excludes2["R"],convert,"DR"))
      excludes3<-list(A=A,B=B,C=C,Q=Q,R=R)
    } else { 
      excludes2<-list(A=NULL,B=NULL,C=NULL,Q=NULL,R=NULL) 
      excludes3<-list(A=NA,B=NA,C=NA,Q=NA,R=NA) 
    } #else      
    
    #Add any input alleles to excludes3 and remove dups
    excludes4<-list(A=NULL,B=NULL,C=NULL,Q=NULL,R=NULL)
    
    strip2<-function(ag,locus) { #if empty will return numeric(0)
      x<-ag[grep(locus,ag)]
      x<-str_extract(x,"\\d+:\\d+")
      return(x)
    } #fun
    A<-unlist(sapply(data$all,strip2,"A"))
    excludes4[[1]]<-unique(na.omit(c(A,excludes3[[1]])))
    B<-unlist(sapply(data$all,strip2,"B"))
    excludes4[[2]]<-unique(na.omit(c(B,excludes3[[2]])))
    C<-unlist(sapply(data$all,strip2,"C"))
    excludes4[[3]]<-unique(na.omit(c(C,excludes3[[3]])))
    Q<-unlist(sapply(data$all,strip2,"DQB1"))
    excludes4[[4]]<-unique(na.omit(c(Q,excludes3[[4]])))
    R<-unlist(sapply(data$all,strip2,"DRB1"))
    excludes4[[5]]<-unique(na.omit(c(R,excludes3[[5]])))
    
    #Display alleles as data.frame via DT package
    data$df.all<-data.frame(NULL)
    for (i in 1:length(excludes4)) {
      for (j in 1:length(excludes4[[i]])) {
        if (!is.null(excludes4[[i]][j])) {
          data$df.all[j,i]<-excludes4[[i]][j]
        } else { data$df.all[j,i]<-NA }
      } #for j
    } #for i
    colnames(data$df.all)<-c("A","B","C","DQ","DR")
    
    data$unos<-round(cpra_std(excludes2,table.haplo,ethnic.weights),6)*100      

    data$nmdp<-round(cpra_5locus(excludes4,table.5l.nmdp.allele,ethnic.weights),6)*100
    
  }) #observeEvent
  
  observeEvent(input$reset, {
    updateTextInput(session,"string", value="")
    data$s <- NULL
    data$ag <- NULL
    data$all <- NULL
    data$unos <- NULL
    data$nmdp <- NULL
    data$df.ag <- data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA)
    data$df.all <- data.frame(A=NA,B=NA,C=NA,DQ=NA,DR=NA)
  }) #observeEvent
  
  output$agtable = DT::renderDataTable({ data$df.ag
  }) #output
  
  output$alltable = DT::renderDataTable({ data$df.all
  }) #output
  
  output$o.unos <- renderPrint({ data$unos
    paste0("",data$unos)
  }) #output

  output$o.nmdp <- renderPrint({ data$nmdp
    paste0("",data$nmdp)
  }) #output
  
}) #Shiny