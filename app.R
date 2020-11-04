#
#
#devtools::install_github("richarddmorey/divergingPips", subdir = "divergingPips")
library(divergingPips)
library(shiny)
library(shinyBS)
library("shinyWidgets")
library(colourpicker)
library(tidyverse)
library(haven)
library(ggrepel)


## 
ranmin=10
ranmax=100

# --------------------------------------------------------------
# UI
# --------------------------------------------------------------
ui <- fluidPage(
    # Application title
    titlePanel(fixedRow(
        column(10,  h2("Balancinator")), 
        #column(2, tags$img(src = "balancinator.png", width="100px"))
        )),
    includeMarkdown("intro.md"),
    tabsetPanel(type="tabs",
                tabPanel("Data", 
                    column(3, wellPanel(
                        sliderInput("nyears", "Number of years", 1, 8, 2),
                        bsTooltip("nyears", "Number of years for which to plot the gender balance.", "right"),
                        
                        sliderInput("ndeps", "Number of departments", 1, 50,3),
                        bsTooltip("ndeps", "Number of departments for which to plot the gender balance.", "right"),
                        
                        actionButton("zeroButton", "Set to zero"),
                        actionButton("randomButton", "Fill with random values"),
                        
                        selectInput("downloadFormat", "Download Format", c("csv","xlsx")),
                        bsTooltip("downloadFormat", "Choose the format for your download.", "right"),
                        
                        downloadButton("downloadData", "Download data"),
                        bsTooltip("downloadData", "Download the dataset. After download, you can modify and upload the dataset again using the Upload Data button.", "right"),
                        

                        # Input: Select a file ----
                        fileInput("inputfile", "Fill data from file",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                             ".csv", ".xlsx")),
                        #actionButton("testButton", "test"),
                        #bsTooltip("testButton", "test button")
                    )),
                    column(9, wellPanel(
                        uiOutput("ui")
                    )
                    )
                ),
                tabPanel("Balance Plot", 
                         #column(2, wellPanel(
                         dropdownButton(
                             colourInput("maleColor", "Select colour for men", "purple"),
                             colourInput("femaleColor", "Select colour for women", "blue"),
                             checkboxInput("malesUp", "change order", value=T),
                             #actionButton("balanceReplotButton", "Replot"),
                             circle = TRUE, status = "danger",
                             icon = icon("gear"), width = "300px",
                             
                             tooltip = tooltipOptions(title = "Click to change plot settings")                                
                             
                         ),
#                         column(10, wellPanel(
                            plotOutput(outputId = "balanceplot", height = "500px", width="100%"),
                ),
                tabPanel("Scatter Plot", 
                         dropdownButton(
                                 colourInput("propStartColour", "Select start colour", "red"),
                                 colourInput("propEndColour", "Select end colour", "green"),
                                 #actionButton("propReplotButton", "Replot"),
                                 circle = TRUE, status = "danger",
                                 icon = icon("gear"), width = "300px",
                                 
                                 tooltip = tooltipOptions(title = "Click to change plot settings")                                 
                         ),
                         plotOutput(outputId = "proportionplot", height = "500px", width="100%")
                             
                         # column(2, wellPanel(
                         #     colourInput("propStartColour", "Select start colour", "red"),
                         #     colourInput("propEndColour", "Select end colour", "green"),
                         #     actionButton("propReplotButton", "Replot")
                         # )),
                         # column(10, wellPanel(
                         #     plotOutput(outputId = "proportionplot", height = "500px", width="100%")
                         # ))
                ),
                tabPanel("About", includeMarkdown("help.md"))
    ),
    hr(),
    includeMarkdown("footer.md")
    
)

# --------------------------------------------------------------
# SERVER
# --------------------------------------------------------------
server <- function(input, output,session) {
    g_years <- NULL ## all the years 
    g_departments <- NULL ## all the departments
    g_frq <- NULL   ## all frequencies for all combinations

    create_tbl <- function(n1,n2){
        #tbl=list(tags$table())
        if(is.null(g_years) || length(g_years)!=n1){
            g_years <<- sprintf("year %i", 1:n1)
        }
        if(is.null(g_departments) || length(g_departments)!=n2){
            g_departments <<- sprintf("department %i", 1:n2)
        }
        if(is.null(g_frq) || length(g_frq)<(2*n1*n2)){
            g_frq <<- rep(0,n1*n2*2)#sample(ranmin:ranmax,n1*2*n2)
        }
        if(length(g_frq)>2*n1*n2){
            g_frq <<- g_frq[1:(2*n1*n2)]
        }
        tbl=list(HTML("<table><tr><th></th>"))
        for(j in 1:n2){
            
            tbl[[length(tbl)+1]]=HTML("<th colspan=2>")
            tbl[[length(tbl)+1]]=textInput(sprintf("department.%i",j), label="", value=g_departments[j])
            tbl[[length(tbl)+1]]=HTML("</th>")
        }
        tbl[[length(tbl)+1]]=HTML("</tr>")
        
        tbl[[length(tbl)+1]]=HTML("<tr><th>Year</th>")
        for(j in 1:n2){
            tbl[[length(tbl)+1]]=HTML("<th style='font-weight:bold;'><center><font size='+0'>women</font></center></th>")#&#9792;
            tbl[[length(tbl)+1]]=HTML("<th><center><font size='+0'>men</font></center></th>") #&#9794;
            
        }
        tbl[[length(tbl)+1]]=HTML("</tr>")
        
        ix=1
        for(i in 1:n1){ # rows
            tbl[[length(tbl)+1]]=HTML("<tr>")
            tbl[[length(tbl)+1]]=list(HTML("<td>"),textInput(sprintf("year.%i",i), label="", value=g_years[i]),HTML("</td>")) 
            for( j in 1:(2*n2)){ #c olumns
                tbl[[length(tbl)+1]]=HTML("<td>")
                tbl[[length(tbl)+1]]=textInput(sprintf("tbl.%i",ix), label="", width="100%", value=g_frq[ix]) 
                ix=ix+1
                tbl[[length(tbl)+1]]=HTML("</td>") 
            }
            tbl[[length(tbl)+1]]=HTML("</tr>")
        }
        tbl[[length(tbl)+1]]=HTML("</table>")
        
        ## keep table and underlying data in sync
        map(1:n2, ~ observeEvent(input[[sprintf("department.%i",.x)]],{
            g_departments[.x] <<- (input[[sprintf("department.%i",.x)]])
        }))
        map(1:n1, ~ observeEvent(input[[sprintf("year.%i",.x)]],{
            g_years[.x] <<- (input[[sprintf("year.%i",.x)]])
        }))
        map(1:(2*n1*n2), ~ observeEvent(input[[sprintf("tbl.%i",.x)]],{
            g_frq[.x] <<- (input[[sprintf("tbl.%i",.x)]])
        }))
        
        return(tbl)
    }
    
    
    output$ui <- renderUI( create_tbl(input$nyears, input$ndeps) )

    get_data_tidy <- function(input){
        #years=unlist(lapply(1:input$nyears, function(i){input[[sprintf("year.%i",i)]]}))
        #deps=unlist(lapply(1:input$ndeps, function(i){input[[sprintf("department.%i",i)]]}))
        #n=input$nyears*(2*input$ndeps)
        #frq=unlist(lapply(1:n, function(i){as.integer(input[[sprintf("tbl.%i",i)]])}))
        data.frame(year=rep(g_years, each=input$ndeps*2),
                   department=rep(g_departments, each=2), 
                   gender=rep(c("women", "men"), input$ndeps*input$nyears),
                   freq=as.integer(g_frq))
    }
    

    # --------------------------------------------------------------
    # BALANCE PLOT
    # --------------------------------------------------------------
    output$balanceplot <- renderPlot({
        ## touching all the data-inputs to force redraw if they changed
        years=unlist(lapply(1:input$nyears, function(i){input[[sprintf("year.%i",i)]]}))
        deps=unlist(lapply(1:input$ndeps, function(i){input[[sprintf("department.%i",i)]]}))
        n=input$nyears*(2*input$ndeps)
        frq=unlist(lapply(1:n, function(i){as.integer(input[[sprintf("tbl.%i",i)]])}))
        
        #a=input$balanceReplotButton
        frq=array(as.integer(g_frq), dim=c(2,input$ndeps,input$nyears))    
        rownames(frq) = c("Women","Men")
        colnames(frq) = g_departments
        dimnames(frq)[[3]] = g_years
        bar.col=c(input$femaleColor, input$maleColor)
        if(input$malesUp){
            # let men be on top
            frq=frq[2:1,,]
            bar.col=bar.col[2:1]
        }
        #print(frq)
        #bar.col = array(c("red","blue","darkred","darkblue"),dim=c(2,2))
        diverging_pip_plot(frq, bar.width = .1, bar.width.n = 4, bar.col = bar.col, add.pct=T,
                           sym = T, cluster.width = .4, panel.lty = 1)
        #divergingPips::diverging_pip_plot(frq,bar.width = .4, bar.width.n = 5, tick.every = 10, sym=F, bar.col=c("orange", "purple"))
    })
    
    # --------------------------------------------------------------
    # Proportion Plot
    # --------------------------------------------------------------
    output$proportionplot <- renderPlot({
        ## touching all the data-inputs to force redraw if they changed
        years=unlist(lapply(1:input$nyears, function(i){input[[sprintf("year.%i",i)]]}))
        deps=unlist(lapply(1:input$ndeps, function(i){input[[sprintf("department.%i",i)]]}))
        n=input$nyears*(2*input$ndeps)
        frq=unlist(lapply(1:n, function(i){as.integer(input[[sprintf("tbl.%i",i)]])}))
        
        #a=input$propReplotButton
        d=get_data_tidy(input )%>% 
            spread(gender, freq) %>%
            mutate(n=men+women, prop=women/n*100) 
        
        year.pairs=data.frame(t(combn(g_years,2))) %>% setNames(c("year1","year2"))

        pmap_df(year.pairs, function(year1,year2){
            d1=d %>% filter(year==year1) %>% 
                select(department, n1=n, prop1=prop) %>% 
                mutate(contr=sprintf("%s_%s",year1,year2))
            d2=d %>% filter(year==year2) %>% select(n2=n, prop2=prop)
            bind_cols(d1,d2)
        }) %>%
            mutate(prop1=prop2-prop1)->dp 
    

        print(dp)
        
        bgdat=data.frame(v=0:100) %>%
            mutate(c=case_when(#v>40 & v<60 ~ 0,
                               T ~ abs(v-50)))
        
        ggplot(NULL)+
            geom_rect(data=bgdat, aes(xmin=-Inf, xmax=Inf, ymin=v, ymax=v+1, fill=c), alpha=1, show.legend=F)+
            geom_point(data=dp, aes(x=prop1, y=prop2,size=n2))+
            scale_fill_gradientn(colours=c("#b64a1a","#fec200","#6b9733"))+
            #scale_fill_gradient(high = input$propStartColour,
            #                    low = input$propEndColour)+
            geom_function(fun= ~ 50, xlim=c(-50,50))+
            facet_wrap(~contr)+
            coord_fixed(xlim=c(-50,50),ylim=c(0,100))+
            scale_x_continuous(limits = c(-50,50), expand = c(0, 0)) +
            scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
            geom_text_repel(data=dp,aes(x=prop1, y=prop2,label=department))+
            theme_bw()+
            labs(x="Percent change from year 1 to year 2", y="Gender balance (% women) year 2",
                 size="Total N")
    })
    
    
    # --------------------------------------------------------------
    # BUTTONS
    # --------------------------------------------------------------
    observeEvent(input$zeroButton, {
        n=input$nyears*(2*input$ndeps)
        g_frq <<- rep(0, n)
        map(1:n,  ~ updateTextInput(session, sprintf("tbl.%i",.x), value="0"))
    })
    
    observeEvent(input$randomButton, {
        n=input$nyears*(2*input$ndeps)
        g_frq <<- sample(ranmin:ranmax, n, replace=T)
        map(1:n,  ~ updateTextInput(session, sprintf("tbl.%i",.x), value=sprintf("%i",g_frq[.x])))
    })
    observeEvent(input$testButton, {
        print(g_departments)
        print(g_years)
        print(g_frq)
        #updateSliderInput(session, "nyears", value=7)
        #n=input$nyears*(2*input$ndeps)
        #map(1:n,  ~ updateTextInput(session, sprintf("tbl.%i",.x), value=sprintf("%i",sample(ranmin:ranmax,1))))
    })
    
    ## upload file button
    observeEvent(input$inputfile, {
        formatErrorPopUp <- function(){
            showModal(modalDialog(
                title = "Error",
                "File-format not recognized.",
                easyClose = TRUE,
                footer = NULL
            ))
        }
        
        req(input$inputfile)
        req_cols=c("year","department","gender","freq")
        d=NULL
        if (tolower(tools::file_ext(input$inputfile$datapath)) == "xlsx") { 
            tryCatch({ d=readxl::read_xlsx(input$inputfile$datapath) },
                error = function(e) { formatErrorPopUp() }
            )
        } else if(tolower(tools::file_ext(input$inputfile$datapath)) == "csv") {
            tryCatch({ d=read_csv(input$inputfile$datapath) },
                error = function(e) { formatErrorPopUp() } 
            )
        } else {
            formatErrorPopUp()
        }
        
        if(!all(unlist(map(req_cols, ~ .x %in% names(d))))){
            formatErrorPopUp()
            d=NULL
        }
        req(d)
        
        # format for internal structure
        years=unique(d$year)
        deps=unique(d$department)
        nyears=length(years)
        ndeps=length(deps)
        all_freq <- rep(0,2*nyears*ndeps)
        d %>% select(year,department,gender,freq) %>%
            pmap(function(year,department,gender,freq){
                yi=which(years==year)
                di=which(deps==department)
                gi=which(c("women","men")==gender)
                all_freq[((yi-1)*ndeps*2)+(2*(di-1))+gi] <<- freq
            })
        
        # update global dataset
        g_years <<- years
        g_departments <<- deps
        g_frq <<- all_freq
        
        # rebuild UI
        updateSliderInput(session, "nyears", value=nyears)
        updateSliderInput(session, "ndeps", value=ndeps)
    })
    
    # Download dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            sprintf("balancinator_data.%s", input$downloadFormat)
            },
        content = function(file) {
            if(input$downloadFormat=="csv"){
                write.csv(get_data_tidy(input), file, row.names = FALSE)
            } else {
                writexl::write_xlsx(get_data_tidy(input), file)
            }
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
