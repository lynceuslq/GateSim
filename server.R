#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rlang)
library(plotly)
library(plyr)
library(minpack.lm)
library(foreach)
library(dplyr)
library(rstan)
library(readxl)
library(xlsx)
library(MASS)
library(shinybusy)
library(deSolve)

### data
data.base <- list(
  nor.1 = read.table("www/cello.notgate.tsv", header = T, sep = "\t"),
  nor.2 =  read.table("www/notgate.tsv", header = T, sep = "\t"),
  activ.1 = read.table("www/cello.sensor.tsv", header = T, sep = "\t"),
  activ.2 = read.table("www/sensor.tsv", header = T, sep = "\t"),
  and.1 = read.table("www/andgate.tsv", header = T, sep = "\t")
)

###

model.script <- list(
  nor.1=c("ode","simprob"),
  nor.2=c("ode","simprob"),
  activ.1=c("ode","simprob"),
  activ.2=c("ode","simprob"),
  and.1=c("ode")
)

standata.type <- list(
  single.in=c("nor.1","nor.2","activ.1","activ.2"),
  multi.in=c("and.1")
) 

### functions
ode.func.list <- list(
  nor.1 = function(t, y, parms) {  
    with(as.list(c(y, parms)),{
      
      yt= ymin + (ymax - ymin) * k^n/(k^n+I^n)
      dM = yt - d*M
      
      list(c(dM))
    })
  },
  nor.2 = function(t, y, parms) {  
    with(as.list(c(y, parms)),{
      
      yt= k3*(alpha3+(K3^n3)/(K3^n3+I^n3))
      dM = yt - d*M
      
      list(c(dM))
    })
  },
  activ.1 = function(t, y, parms) {  
    with(as.list(c(y, parms)),{
      
      yt= ymin + (ymax - ymin) * I^n/(k^n+I^n)
      dM = yt - d*M
      
      list(c(dM))
    })
  },
  activ.2 = function(t, y, parms) {  
    with(as.list(c(y, parms)),{
      
      yt= k*(alpha+(I^n1)/(K1^n1+I^n1))
      dM = yt - d*M
      
      list(c(dM))
    })
  },
  and.1 = function(t, y, parms) {  
    with(as.list(c(y, parms)),{
      
      R= (-sqrt((M+N+1/e)^2-4*M*N) +M +N +1/e)/2
      
      dM = km - d*(M-R)
      dN = kn - d*(N-R)
      
      list(c(dM,dN))
    })
  }
)

par.list <- list(
  nor.1=c("ymin","ymax","k","n","d"),
  nor.2=c("alpha3","k3","K3","n3","d"),
  activ.1=c("ymin","ymax","k","n","d"),
  activ.2=c("alpha","k","K1","n1","d"),
  and.1=c("e","km","kn","d")
)

input.list=list(
  nor.1=c("I"),
  nor.2=c("I"),
  activ.1=c("I"),
  activ.2=c("I"),
  and.1=c()
)

init.list=list(
  nor.1=c("M"),
  nor.2=c("M"),
  activ.1=c("M"),
  activ.2=c("M"),
  and.1=c("M","N")
)

rand.simout <- list(
  ode=list(  nor.1=c("z"),
             nor.2=c("z"),
             activ.1=c("z"),
             activ.2=c("z"),
             and.1=c("m_obs","n_obs","r_obs"))
)

ode.sim.out <- list(
  nor.1=expr(m <- M),
  nor.2=expr(m <- M),
  activ.1=expr(m <- M),
  activ.2=expr(m <- M),
  and.1=expr({
    r <- (-sqrt((M+N+1/parameters[["e"]])^2-4*M*N) +M +N +1/parameters[["e"]])/2
    m <- M - r
    n <- N - r
  })
)

ode.sim.out.names <- list(
  nor.1=c("m"),
  nor.2=c("m"),
  activ.1=c("m"),
  activ.2=c("m"),
  and.1=c("r","m","n")
)

simu.array <- function(parlist, init, times,func, p) {
  # p <- plot_ly()
  df <- data.frame()
  for(i in 1:length(parlist)) {
    yout <- dede(y = init[[i]], 
                 times = times, 
                 func = func,
                 parms = parlist[[i]], 
                 atol = 1e-10)
    
    yout <- as.data.frame(yout)
    yout$init <- init[[i]]
    yout$pars <- paste0("parset_",i)
    p <- p %>% add_lines(x=yout$time, y=yout[[names(init[[1]])]],name=paste0("odesim_parset_",i) )
    
    df <- rbind(df, yout)
  }
  
  return(p)
  
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  rv <- reactiveValues()
  
  output$parlist <- renderUI({
    req(rv$pars)
    print(rv$rspars)
    
    div(foreach::foreach(a=names(rv$recpars()),b=rv$recpars()) %do% do.call("numericInput", list(inputId=a, label=paste0(a,": "),value=b)))

  })
  
  output$initlist <- renderUI({
    req(rv$init)

    div(foreach::foreach(a=rv$init) %do% do.call("numericInput", list(inputId=a, label=paste0(a,": "),value=0)))
    
  })
  
  output$script <- renderUI({
    
    if(input$genran) {
      div(selectInput("rsmodel","Select simulation model:",choices = model.script[[rv$func]]))
    }else{
      div()
    }
  })
  
  observeEvent(list(input$rsmodel,input$func), {
    req(input$rsmodel)
    rv$rsscript <- paste0("www/",rv$func,".",input$rsmodel,".stan")
    
    print(rv$rsscript)
  })
  
  observeEvent(input$func,{
    rv$func <- input$func
    rv$pars <- c(input.list[[rv$func]], par.list[[rv$func]])
    rv$rspars <- c(input.list[[rv$func]], paste0(par.list[[rv$func]],".se"), par.list[[rv$func]])
    rv$init <- init.list[[rv$func]]
  })
  
  rv$initvalues <- reactive({
    vallist <- foreach::foreach(a=rv$init, .combine = "c") %do% input[[a]]
    names(vallist) <- rv$init
    
    vallist
  })
  
  rv$rsparvalues <- reactive({
    req(input$rsmodel)

    vallist <- foreach::foreach(a=rv$rspars, .combine = "c") %do% input[[a]]
    names(vallist) <- rv$rspars
    
    vallist
  })
  
  rv$parvalues <- reactive({
    vallist <- foreach::foreach(a=rv$pars, .combine = "c") %do% input[[a]]
    names(vallist) <- rv$pars
    
    vallist
  })
  
  rv$ts <- reactive({
    req(input$start)
    req(input$end)
    req(input$intv)
    
    seq(input$start,input$end,input$intv)
    
  })
  
  observeEvent(input$simu,{
    print(rv$parvalues())
    print(rv$ts())
    print(rv$initvalues())
    
    yout <- ode(y = rv$initvalues(), 
                times = rv$ts(), 
                func = ode.func.list[[rv$func]],
                parms = rv$parvalues(), 
                atol = 1e-10)
    
    rv$yout <- as.data.frame(yout)
    
    print(head(rv$yout))
  })
  
  output$record_tab <- DT::renderDataTable({
    data.base[[rv$func]]
  }, options = list(pageLength = 5))
  
  dataModal <- function(failed = FALSE) {
    modalDialog(size="l",title=paste0("Parameter records for ",input$func),
                helpText("Choice gate paramters from:"
                ),
                box(title = "Parameter table", width = 12, solidHeader = T, status = "success",
                    DT::dataTableOutput("record_tab")
                    ),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("conf", "Confirm")
                )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$byrec, {
    showModal(dataModal())
  })
  
  rv$recpars <- reactive({
   # req(input$conf)
   # req(input$record_tab_rows_selected)
    # print(s)
    if(is.null(input$record_tab_rows_selected)) {
      s=1
    }else{
      s = input$record_tab_rows_selected
    }
    
    if(input$genran) {
      recpars <- c()
      notinp <- c()
      notin <- c("d","d.se","I")
      pari <- sort(rv$rspars[! rv$rspars %in% notin])
      
      for(i in 1:length(pari)) {
        if(is.null(data.base[[rv$func]][s,pari[i]])) {
          recpars[i] <- 0
        }else{recpars[i] <- data.base[[rv$func]][s,pari[i]]}
      }
      names(recpars) <- pari
      
      for(i in 1:length(notin)) {
        notinp[i] <- 0
      }
      names(notinp) <- notin
      
      recpars <- c(recpars,notinp)
    }else{
      recpars <- c()
      notinp <- c()
      notin <- c("d","I")
      pari <- sort(rv$pars[! rv$pars %in% notin])
      
      for(i in 1:length(pari)) {
        recpars[i] <- data.base[[rv$func]][s,pari[i]]
      }
      names(recpars) <- pari
      
      for(i in 1:length(notin)) {
        notinp[i] <- 0
      }
      names(notinp) <- notin
      
      recpars <- c(recpars,notinp)
    }
    
    recpars
  })
  
  observeEvent(input$conf,{

    print(rv$recpars())
    
    removeModal()
  })
  
  output$plotode <- renderPlotly({
    req(input$simu)
    req(rv$yout)
    
    outnames <- ode.sim.out.names[[input$func]]
    parameters <- rv$parvalues()
    
    rv$outtab <- within(rv$yout,eval(ode.sim.out[[input$func]]))
    
    pl <- plot_ly() 
    
    for(i in 1:length(outnames)) {
      
      pl <- pl %>% add_lines(x=rv$outtab$time, 
                             y=rv$outtab[,outnames[i]],
                             name=outnames[i])
    }
    
    pl 
  })
  
  output$priordist <- renderPrint({
    req(rv$rspars)
    
    pr <- par.list[[rv$func]]
    prv <- rv$rsparvalues()
    ele <- c("")
    
    for(i in 1:length(pr)) {
      ele <- paste0(ele,
                    pr[i],
                    " ~ Normal(",
                    prv[pr[i]],
                    ",",
                    prv[paste0(pr[i],".se")],
                    ");\n"
                    )
    }
    
    cat(ele)
    
  })
  
  output$priorplot <-  renderPlotly({
    req(rv$rsscript)
    req(rv$rspars)
    
    pr <- par.list[[rv$func]]
    prv <- rv$rsparvalues()
    pl <- list()
    
    for(i in 1:length(pr)) {
      g <- plot_ly() %>% add_trace(x=pr[i],
                                   y=rnorm(500,mean=prv[pr[i]],
                                           sd=prv[paste0(pr[i],".se")]),
                                   type = "violin",name=pr[i])
      
      pl <- c(pl,list(g))
    }
    
    do.call("subplot",pl)
    
  })
  
  output$postdist <- renderPlotly({
    req(rv$mydatatype)
    req(rv$mydata)
    req(rv$sim)
    
    clickData <- event_data("plotly_click", source = "rand_plot")
    
    if (is.null(clickData)) return(NULL)
    
    print(clickData)
    
    tt <- clickData[["x"]]
    ts <- rv$ts()[2:length(rv$ts())]
    
    numm <- (1:length(ts))[ts == tt]
    
    pl <- list()
    outele <- rand.simout[[input$genran]][[input$func]]
  #  head(as.data.frame(summary(rv$sim,outele)$summary))
    
    if(rv$mydatatype == "single.in"){
      if(input$rsmodel=="simprob") {
      for( x in 1:(rv$mydata$X)) {
        if(paste0(outele,"[1,",x,",",numm,"]") %in% rownames(summary(rv$sim,outele)$summary)) {
        vari <- extract(rv$sim,paste0(outele,"[1,",x,",",numm,"]"))[[1]]
        print(num)
        g1 <- plot_ly() %>% add_histogram(x=vari,name=paste0("input =",rv$mydata$input[x]))
        g2 <- plot_ly() %>% add_boxplot(x=vari,name=paste0("input =",rv$mydata$input[x]))
        
        g <- subplot(g1,g2,nrows = 2,shareX = T)
        pl <- c(pl,list(g))
        }
      }
      }
      
      if(input$rsmodel=="ode") {
        for( x in 1:(rv$mydata$X)) {
          if(paste0(outele,"[",numm,",",x ,"]")%in% rownames(summary(rv$sim,outele)$summary)) {
        vari <- extract(rv$sim,paste0(outele,"[",numm,",",x ,"]"))[[1]]
        g1 <- plot_ly() %>% add_histogram(x=vari,name=paste0("input =",rv$mydata$input[x]))
        g2 <- plot_ly() %>% add_boxplot(x=vari,name=paste0("input =",rv$mydata$input[x]))
        
        g <- subplot(g1,g2,nrows = 2,shareX = T)
        
        print(num)
        pl <- c(pl,list(g))
        }
        }
      }
    }
    
    if(rv$mydatatype == "multi.in"){
      if(input$rsmodel=="ode") {
        for(i in 1:length(outele)) {
          if(paste0(outele[i],"[",numm,",",1,"]")%in% rownames(summary(rv$sim,outele)$summary)) {
            vari <- extract(rv$sim,paste0(outele[i],"[",numm,",",1,"]"))[[1]]
            g1 <- plot_ly() %>% add_histogram(x=vari,name=paste0("dym=",outele[i]))
            g2 <- plot_ly() %>% add_boxplot(x=vari,name=paste0("dym =",outele[i]))
            
            g <- subplot(g1,g2,nrows = 2,shareX = T)
            
            print(num)
            pl <- c(pl,list(g))
          }
        }
        
      }
    }
    
    
    do.call("subplot",pl)
   
  })
  
  output$plotrand <- renderPlotly({
    req(rv$mydatatype)
    req(rv$mydata)
    req(rv$sim)
    
    outele <- rand.simout[[input$genran]][[input$func]]
    print(outele)
    p <- plot_ly(source = "rand_plot")
    
    if(rv$mydatatype == "multi.in"){
      
      ts <- rv$mydata$t
      
      for(i in 1:length(outele)) {
        summ <- as.data.frame(summary(rv$sim,outele[i])$summary)
        p <- p %>% add_lines(x=ts, y=summ$"50%",name=outele[i]) %>% 
          add_ribbons(x=ts,ymin=summ$"25%",
                      ymax=summ$"75%",
                      name=paste0(outele[i],";25%-75%"), 
                      line = list(shape = "spline", dash = 'dot'))
      }
    }
    
    
    if(rv$mydatatype == "single.in"){
      ts <- rv$ts()[2:length(rv$ts())]
      
        summ <- as.data.frame(summary(rv$sim,outele)$summary)
        print(summ)
        
        if(input$rsmodel=="simprob") {
          zvals50 <- matrix(summ$'50%',ncol = length(ts),byrow = T)
          # zvalsmean <- matrix(summ$'mean',ncol = length(ts),byrow = T)
          zvals75 <- matrix(summ$'75%',ncol = length(ts),byrow = T)
          zvals25 <- matrix(summ$'25%',ncol = length(ts),byrow = T)
        }
        
        if(input$rsmodel=="ode") {
          zvals50 <- t(matrix(summ$'50%',nrow  = length(ts),byrow = T))
          # zvalsmean <- matrix(summ$'mean',ncol = length(ts),byrow = T)
          zvals75 <- t(matrix(summ$'75%',nrow = length(ts),byrow = T))
          zvals25 <- t(matrix(summ$'25%',nrow = length(ts),byrow = T))
        }
        
        for( x in 1:(rv$mydata$X)) {
          num <- x
          print(num)
          p <-  p %>% add_ribbons(x=ts,ymin=zvals25[num,],ymax=zvals75[num,],
                                  name=paste0("input =",rv$mydata$input[x],";25-75%"), 
                                  line = list(shape = "spline", dash = 'dot'))
          
          p <-  p %>% add_lines(x=ts,y=zvals50[num,],
                                name=paste0("input =",rv$mydata$input[x],";50%"), 
                                line = list(shape = "spline"))
          
        }
      
    }
      
     p <- p %>%  layout(xaxis = list(
       #  type = 'log',
       zerolinecolor = '#ffff',
       zerolinewidth = 2,
       gridcolor = 'ffff',
       title = 'Time (h)'),
       yaxis = list(
         #  type = 'log',
         zerolinecolor = '#ffff',
         zerolinewidth = 2,
         gridcolor = 'ffff',
         title = 'Concentration (mol/l)'))
    
    p
    
  })
  
  observeEvent(input$stancon, {
    req(rv$rsscript)
    
    show_modal_spinner(
      spin = "orbit",
      # color = "firebrick",
      color = "royalblue",
      text = paste("HMC SAMPLING FOR MODEL",paste0(rv$func,".",input$rsmodel,".stan"))
    )
    
    vals <- rv$rsparvalues()
    
    invals <- as.numeric(vals[input.list[[rv$func]]])
    parvals <- as.numeric(vals[par.list[[rv$func]]])
    
    sevals <- as.numeric(vals[paste0(par.list[[rv$func]],".se")])
    
    ll <- unlist(lapply(standata.type, function(x) {
      rv$func %in% x
    }))
    
    rv$mydatatype <- names(standata.type)[ll]
    
    if(input$rsmodel=="ode") {
      
      if(rv$mydatatype == "multi.in"){
        rv$mydata <- list(N=length(rv$ts()[2:length(rv$ts())]),
                          t=rv$ts()[2:length(rv$ts())],
                          y_init=as.numeric(rv$initvalues()),
                          phi=parvals,
                          phi_sd=sevals)
      }
      if(rv$mydatatype == "single.in"){
        rv$mydata <- list(N=length(rv$ts()[2:length(rv$ts())]),
                          X=2,
                          t=rv$ts()[2:length(rv$ts())],
                          y_init=c(as.numeric(rv$initvalues()),
                                   as.numeric(rv$initvalues())),
                          input=c(0,invals),
                          phi=parvals,
                          phi_sd=sevals)
      }
    }
    if(input$rsmodel=="simprob") {
      if(rv$mydatatype == "single.in"){
      rv$mydata <- list(N=1, 
                        T=length(rv$ts()[2:length(rv$ts())]),
                     X=2, ts=rv$ts()[2:length(rv$ts())],
                     z_init=matrix(c(as.numeric(rv$initvalues()),
                                     as.numeric(rv$initvalues())), ncol = 2 ,byrow = T),
                     input=c(0,invals),
                     phi=matrix(parvals, ncol = 5 ,byrow = T ),
                     phi_sd=matrix(sevals,ncol = 5,byrow = T ),
                     uplim=10,
                     downlim=3)
      }
    }
    
    print(rv$mydata)
    
    rv$sim <- stan(file = rv$rsscript, 
                    model_name = "Simu2", 
                    data = rv$mydata,
                    chains=input$chains,
                    iter=input$iter,
                    cores=input$cores)
    
    print(summary(rv$sim,"theta")$summary)
    
    remove_modal_spinner()
    removeModal()
    
  })
  
  standataModal <- function(failed = FALSE) {
    modalDialog(size="l",title="Random Sampleing with HMC",
                helpText("Run sampling with priors below:"
                ),
                verbatimTextOutput("priordist"),
                plotlyOutput("priorplot"),
             numericInput("chains","Number of chains:",
                          value = 1,
                          min = 1,
                          max = 16),
             numericInput("iter","Number of iterations:",
                          value = 1000,
                          min = 0,
                          max = 10000),
             numericInput("cores","Number of cores for sampling:",
                          value = parallel::detectCores()-2,
                          min = 1,
                          max = 8),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("stancon", "Confirm")
                )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$simu, {
    if(input$genran) {
      showModal(standataModal())
    }
    
  })
})
