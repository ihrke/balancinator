#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#devtools::install_github("richarddmorey/divergingPips", subdir = "divergingPips")
library(divergingPips)
library(shiny)
library(DT)
library(tidyverse)


## 
ranmin=10
ranmax=100

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Balance Plot"),

    tabsetPanel(type="tabs",
                tabPanel("Data", 
                    column(3, wellPanel(
                        sliderInput("nyears", "Number of years", 1, 20, 3),
                        sliderInput("ndeps", "Number of departments", 1, 10,4)
                    )),
                    column(9, wellPanel(
                        uiOutput("ui")
                    )
                    )
                ),
                tabPanel("Balance Plot", 
                         plotOutput(outputId = "balanceplot", height = "500px", width="100%")
                ),
                tabPanel("Proportion Plot", 
                         plotOutput(outputId = "proportionplot", height = "500px", width="100%")
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    create_tbl <- function(n1,n2){
        #tbl=list(tags$table())
        tbl=list(HTML("<table><tr><th></th>"))
        for(j in 1:n2){
            tbl[[length(tbl)+1]]=HTML("<th colspan=2>")
            tbl[[length(tbl)+1]]=textInput(sprintf("department.%i",j), label="", value=sprintf("department %i",j)) 
            tbl[[length(tbl)+1]]=HTML("</th>")
        }
        tbl[[length(tbl)+1]]=HTML("</tr>")
        
        tbl[[length(tbl)+1]]=HTML("<tr><th>Year</th>")
        for(j in 1:n2){
            tbl[[length(tbl)+1]]=HTML("<th><center><font size='+2'>&#9792;</font></center></th>")
            tbl[[length(tbl)+1]]=HTML("<th><center><font size='+2'>&#9794;</font></center></th>")
            
        }
        tbl[[length(tbl)+1]]=HTML("</tr>")
        
        ix=1
        for(i in 1:n1){ # rows
            tbl[[length(tbl)+1]]=HTML("<tr>")
            tbl[[length(tbl)+1]]=list(HTML("<td>"),textInput(sprintf("year.%i",i), label="", value=sprintf("%i",2020-n1+i)),HTML("</td>"))
            for( j in 1:(2*n2)){ #c olumns
                tbl[[length(tbl)+1]]=HTML("<td>")
                tbl[[length(tbl)+1]]=textInput(sprintf("tbl.%i",ix), label="", width="100%", value=sample(ranmin:ranmax,1)) 
                ix=ix+1
                tbl[[length(tbl)+1]]=HTML("</td>") 
            }
            tbl[[length(tbl)+1]]=HTML("</tr>")
        }
        tbl[[length(tbl)+1]]=HTML("</table>")
        return(tbl)
    }
    
    output$ui <- renderUI( create_tbl(input$nyears, input$ndeps) )

    
    get_data <- function(input){
        years=lapply(1:input$nyears, function(i){input[[sprintf("year.%i",i)]]})
        deps=lapply(1:input$ndeps, function(i){input[[sprintf("department.%i",i)]]})
        n=input$nyears*(2*input$ndeps)
        frq=unlist(lapply(1:n, function(i){as.integer(input[[sprintf("tbl.%i",i)]])}))
        
        if(input$ndeps==1){
            frq=array(frq, dim=c(2,input$nyears))
            rownames(frq) = c("Women","Men")
            colnames(frq) = years
        } else {
            frq=array(frq, dim=c(2,input$ndeps,input$nyears))    
            rownames(frq) = c("Women","Men")
            colnames(frq) = deps #years
            dimnames(frq)[[3]] = years #deps
        }
        
        
        return(frq)
    }
    
    output$balanceplot <- renderPlot({
        frq=get_data(input)
        print(frq)
        #bar.col = array(c("red","blue","darkred","darkblue"),dim=c(2,2))
        bar.col=c("orange", "purple")
        diverging_pip_plot(frq, bar.width = .1, bar.width.n = 4, bar.col = bar.col,
                            sym = T, cluster.width = .4, panel.lty = 1)
        #divergingPips::diverging_pip_plot(frq,bar.width = .4, bar.width.n = 5, tick.every = 10, sym=F, bar.col=c("orange", "purple"))
    })
    output$proportionplot <- renderPlot({
        frq=get_data(input)
        freq.df=as.tibble(frq) %>% gather(var,val) %>% separate(var, into=c("dep","year"), sep="\\.") %>% 
            mutate(gender=rep(c("female","male"),n()/2)) %>% spread(gender,val) %>% 
            mutate(n=male+female, prop=female/n*100)
        print(freq.df)
        years=unique(freq.df$year)
        year.pairs=data.frame(t(combn(years,2))) %>% setNames(c("year1","year2"))

        print(years)
        print(year.pairs)
                
        map_df(as.data.frame(t(year.pairs)), function(x){
            y1=as.numeric(as.character(x[1]))
            y2=as.numeric(as.character(x[2]))
            d1=freq.df %>% filter(year==y1) %>% select(dep, n1=n, prop1=prop) %>% mutate(contr=sprintf("%i_%i",y1,y2))
            d2=freq.df %>% filter(year==y2) %>% select(n2=n, prop2=prop)
            bind_cols(d1,d2)
        }) -> dp
        print(dp)
        
        bgdat=data.frame(v=0:100) %>%
            mutate(c=case_when(#v>40 & v<60 ~ 0,
                               T ~ abs(v-50)))
        
        ggplot(NULL)+
            geom_rect(data=bgdat, aes(xmin=-Inf, xmax=Inf, ymin=v, ymax=v+1, fill=c), alpha=0.4, show.legend=F)+
            geom_point(data=dp, aes(x=prop1, y=prop2,size=n2))+
            scale_fill_gradient(high = "red",
                                low = "green")+
            geom_function(fun= ~ .x, xlim=c(0,100))+
            facet_wrap(~contr)+
            coord_fixed(xlim=c(0,100),ylim=c(0,100))+
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
