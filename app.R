#
#
#devtools::install_github("richarddmorey/divergingPips", subdir = "divergingPips")
library(divergingPips)
library(shiny)
library(shinyBS)
library("shinyWidgets")
library(colourpicker)
library(markdown)
library(tidyverse)
library(haven)
library(ggrepel)
library(DT)
library(htmltools)
library(glue)

this.year=as.integer(format(Sys.Date(), "%Y"))
## 
ranmin=10
ranmax=100
max_deps=50
balancinator_branding="http://uit.no/resources/balancinator"
twitter_url <- "https://twitter.com/intent/tweet?text=Create%20great%20graphs%20for%20gender%20balance%20in%20your%20organization%20with%20@balancinator!&url=http://uit.no/resources/balancinator"
facebook_url <- "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fuit.no%2Fresources%2Fbalancinator&amp;src=sdkpreparse"

# --------------------------------------------------------------
# UI
# --------------------------------------------------------------
ui <- fluidPage(
    # Application title
    titlePanel(fixedRow(
        column(10,  h2("Balancinator")), 
        #column(2, tags$img(src = "balancinator.png", width="100px"))
        ), windowTitle = "Balancinator"),
    includeMarkdown("intro.md"),
    tabsetPanel(type="tabs",
                tabPanel("Data", 
                    column(3, wellPanel(
                        pickerInput(
                            inputId = "years",
                            label = "Choose years...", 
                            choices = this.year:2000,
                            selected=c(this.year,this.year-1),
                            options = list(
                                `selected-text-format` = "count > 5"), 
                            multiple = TRUE
                        ),
                        
                        #sliderInput("nyears", "Number of years", 1, 8, 2),
                        #bsTooltip("nyears", "Number of years for which to plot the gender balance.", "right"),
                        
                        sliderInput("ndeps", "Number of units", 1, max_deps,3),
                        bsTooltip("ndeps", "Number of units for which to plot the gender balance.", "right"),
                        
                        actionButton("zeroButton", "Set to zero", style = "width:100%;"),
                        actionButton("randomButton", "Fill with random values", style = "width:100%;"),
                        
                        selectInput("downloadFormat", "Download Format", c("xlsx","csv")),
                        bsTooltip("downloadFormat", "Choose the format for your download.", "right"),
                        
                        downloadButton("downloadData", "Download data", style = "width:100%;"),
                        bsTooltip("downloadData", "Download the dataset. After download, you can modify and upload the dataset again using the Upload Data button.", "right"),
                        

                        # Input: Select a file ----
                        fileInput("inputfile", "Fill data from file",
                                  multiple = FALSE,
                                  accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                             "text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".xlsx",".csv")),
                        #actionButton("testButton", "test"),
                        #bsTooltip("testButton", "test button")
                    )),
                    column(9, wellPanel(
                        tags$b("Insert data manually:"),
                        tags$ul(
                            tags$li("Double-click on a cell to edit."),
                            tags$li("Press <TAB> to get to the next cell."),
                            tags$li("Press <CRTL>+Enter to save the data.")
                        ),
                        DTOutput("balancedata")
                        #uiOutput("ui")
                    )
                    )
                ),
                tabPanel("Balance Plot", 
                         column(3, wellPanel(
                         #dropdownButton(
                             colourInput("maleColor", "Select colour for men", "purple"),
                             colourInput("femaleColor", "Select colour for women", "blue"),
                             checkboxInput("malesUp", "change order", value=T),
                             sliderInput("bar_width_n", "Number of bars", 1, 20, 4),
                             sliderInput("bar_width", "Width of bars", 1, 100, 10, step=5),
                             sliderInput("balanceplot_width", "Width of plot", 200, 1000, 500, step=50),
                             sliderInput("balanceplot_height", "Height of plot", 200, 1000, 500, step=50),
                             checkboxInput("includeBalancinatorBrandingBalanceplot", "include Balancinator branding", value=T),
                             downloadButton('downloadBalancePlot', 'Download as PDF', style = "width:100%;")
                             #actionButton("balanceReplotButton", "Replot"),
                             #circle = TRUE, status = "danger",
                             #icon = icon("gear"), width = "300px",
                             
                             #tooltip = tooltipOptions(title = "Click to change plot settings")                                
                             
                         )), column(9, wellPanel(                         
                         uiOutput("balanceplots")
                         ))
                ),
                tabPanel("Prestige Plot", 
                         column(3, wellPanel(
                             checkboxInput("gridSwitch", "include grid", value=T),
                             checkboxInput("propMen", "show proportion men", value=F),
                             checkboxInput("prestigeplotDifference", "plot difference on x-axis", value=F),
                             #sliderInput("bar_width_n", "Number of bars", 1, 20, 4),
                             #sliderInput("bar_width", "Width of bars", 0.01, 1, 0.1, step=0.1),
                             sliderInput("prestigeplot_size", "Size of plot", 200, 1000, 500, step=50),
                             checkboxInput("includeBalancinatorBrandingPrestigeplot", "include Balancinator branding", value=T),
                             downloadButton('downloadPrestigePlot', 'Download as PDF', style = "width:100%;")
                         )), column(9, wellPanel(                         
                             uiOutput("prestigeplots")
                         ))                         
                ),
                tabPanel("Help", includeMarkdown("help.md")),
                tabPanel("About", includeMarkdown("about.md"))
    ),
    hr(),
    includeMarkdown("footer.md"),
    actionButton("twitter_share",
                 label = "Tweet",
                 icon = icon("twitter"),
                 onclick = sprintf("window.open('%s')", twitter_url)),
    actionButton("facebook_share",
                 label = "Share",
                 icon = icon("facebook"),
                 onclick = sprintf("window.open('%s')", facebook_url))
    
    
)

# --------------------------------------------------------------
# SERVER
# --------------------------------------------------------------
server <- function(input, output,session) {
    g_data <- NULL
    prestigeplot_objects <- NULL
    balanceplot_objects <- NULL
    
    # header for the table
    get_sketch = function(years){
        htmltools::withTags(table(
            class = 'display',
            thead(
                tr(
                    th(rowspan = 2, 'Units'),
                    lapply(years, function(.x) th(colspan = 2, align="center", style="text-align:center", .x)),
                ),
                tr(
                    lapply(rep(c('Men', 'Women'), length(years)), function(.x) {th(style="text-align:center",.x)})
                )
            )
        ))
    }
    
    #output$ui <- renderUI( create_tbl(input$nyears, input$ndeps) )
    output$balancedata <- renderDT({
        a=input$randomButton
        b=input$zeroButton
        #print("JO")
        #print(g_data)
        if(is.null(g_data)){
            # new initialization
            #g_years <<- sprintf("year%i", 1:input$nyears)
            g_data<<-tibble(
                Unit=sprintf("Unit %i", 1:input$ndeps)
            )
            for(year in input$years){
                g_data[sprintf("%s.men",year)]<<-rep(0,input$ndeps)
                g_data[sprintf("%s.women",year)]<<-rep(0,input$ndeps)
            }
        }  
        
        for(year in input$years){
            if(paste0(year,".men") %in% names(g_data))
                next
            else {
                g_data[sprintf("%s.men",year)]<<-rep(0,input$ndeps)
                g_data[sprintf("%s.women",year)]<<-rep(0,input$ndeps)
            }
        }
        all.years=unique(map_chr(str_split(names(g_data)[-1], pattern="\\."), ~ .x[1]))
        for(year in all.years){
            if(!(year %in% input$years)){
                g_data[,paste0(year,".men")]<<-NULL
                g_data[,paste0(year,".women")]<<-NULL
            }
        }

        if(input$ndeps!=dim(g_data)[1]){
            # num units changed
            if(input$ndeps>dim(g_data)[1]){
                # add rows
                new.deps=sprintf("Unit %i", (dim(g_data)[1]+1):input$ndeps)
                for(dep in new.deps){
                    g_data <<- rbind(g_data, c(dep, rep(0,dim(g_data)[2]-1)))
                }
            } else {
                # remove rows
                g_data <<- g_data[1:input$ndeps,]
            }
        }
        g_data
    }, server=T, selection = 'none', editable="all", container=get_sketch(input$years), 
        options=list(dom="tp", ordering=F,columnDefs = list(list(className = 'dt-center', targets = "_all"))),  rownames = F)
    
    observeEvent(input$balancedata_cell_edit, {
        g_data <<- editData(g_data, input$balancedata_cell_edit, 'balancedata', rownames=F)
        maxval <- g_data |> select(where(is.numeric)) |> max()
        updateSliderInput(session, "bar_width_n", max=maxval/10)
    })

    

    # --------------------------------------------------------------
    # BALANCE PLOT
    # --------------------------------------------------------------
    output$balanceplots <- renderUI({
        a=input$balancedata_cell_edit
        nyears=length(input$years)
        nplots=(nyears*(nyears-1))/2
        plot_output_list <- lapply(1:input$ndeps, function(i) {
            plotname <- paste("balanceplot", i, sep="")
            plotOutput(plotname, height = input$balanceplot_height, width = input$balanceplot_width)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })    
    for (i in 1:max_deps) {
        local({
            my_i <- i
            plotname <- paste("balanceplot", my_i, sep="")
            
            output[[plotname]] <- renderPlot({
                a=input$ndeps
                b=input$balancedata_cell_edit
                d=input$zeroButton
                e=input$randomButton
                
                dep=unique(pull(g_data, Unit))[my_i]
                #print(dep)
                g_data %>%
                    filter(Unit==dep) %>%
                    gather(var,val,-Unit) %>%
                    separate(var,c("year","gender"),sep="\\.") %>%
                    arrange(year,gender)-> d
                
                freq=as.array(as.integer(d$val))
                if(all(freq==0)){
                    plot(1,1)
                    text(1,1,"NO DATA")
                } else {
                    dim(freq)=c(2,length(input$years))
                    rownames(freq)=c("Men","Women")
                    colnames(freq)=unique(d$year)
                    bar.col=c(input$maleColor, input$femaleColor)
                    if(input$malesUp){
                        # let men be on top
                        freq=freq[2:1,]
                        bar.col=bar.col[2:1]
                    }
                    diverging_pip_plot(freq, bar.width = input$bar_width/100, bar.width.n = input$bar_width_n, bar.col = bar.col, add.pct=T,
                                       sym = T, cluster.width = .4, panel.lty = 1)
                    title(dep)
                }
                if(input$includeBalancinatorBrandingBalanceplot){
                    title(sub=balancinator_branding, adj=1, line=3, font=2)
                }
                
                p=recordPlot()
                balanceplot_objects[[my_i]] <<- p
                
            })
        })
    }
    
    output$downloadBalancePlot <- downloadHandler(
        filename = function() { "balanceplot.pdf"  },
        content = function(file) {
            w_in=input$balanceplot_width/90 
            h_in=input$balanceplot_height/90
            pdf("balanceplot.pdf", width=w_in, height=h_in)
            lapply(balanceplot_objects, replayPlot)
            dev.off()
            file.copy("balanceplot.pdf", file, overwrite=TRUE)
        }
    )
    # --------------------------------------------------------------
    # Prestige Plot
    # --------------------------------------------------------------
    output$prestigeplots <- renderUI({
        a=input$balancedata_cell_edit
        b=input$ndeps
        nyears=length(input$years)
        nplots=(nyears*(nyears-1))/2
        plot_output_list <- lapply(1:nplots, function(i) {
            plotname <- paste("prestigeplot", i, sep="")
            plotOutput(plotname,  height = input$prestigeplot_size, width = input$prestigeplot_size)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:max_deps) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        
        local({
            my_i <- i
            plotname <- paste("prestigeplot", my_i, sep="")

            output[[plotname]] <- renderPlot({
                a=input$ndeps
                b=input$balancedata_cell_edit
                d=input$zeroButton
                e=input$randomButton
                
                year.pairs=data.frame(t(combn(input$years,2))) %>% setNames(c("year1","year2"))
                year1=as.character(year.pairs[my_i,1])
                year2=as.character(year.pairs[my_i,2])
                yy=as.character(sort(as.integer(c(year1,year2))))
                year1=yy[1]
                year2=yy[2]
                g_data %>% gather(var,freq,-Unit) %>%
                    separate(var,c("year","gender"),sep="\\.") %>%
                    rename(unit=Unit) %>%
                    spread(gender, freq) %>%
                    mutate(n=men+women, prop=women/n*100) %>%
                    filter(year %in% c(year1,year2)) %>%
                    gather(var,val,n,prop) %>% 
                    unite(year,var,col = "var", sep = "_") %>%
                    select(unit,var,val) %>%
                    spread(var,val) %>%
                    rename(prop1=paste0(year1,"_prop"),
                           prop2=paste0(year2,"_prop"),
                           n1=paste0(year1,"_n"),
                           n2=paste0(year2,"_n")) -> d
                
                if(input$prestigeplotDifference){
                    d %>% mutate(prop1=prop2-prop1)-> d
                }
                #print(d)
                
                
                bgdat=data.frame(v=-10:110) %>%
                    mutate(c=case_when(#v>40 & v<60 ~ 0,
                        T ~ abs(v-50)))
                
                propgender="women"
                if(input$propMen){
                    if(input$prestigeplotDifference){
                    d$prop1=-d$prop1
                    } else {
                        d$prop1=100-d$prop1
                    }
                    d$prop2=100-d$prop2
                    propgender="men"
                }
                
                #print(paste0(year1,"_prop"))
                grid.maj.x=c()#element_blank()
                grid.min.x=c()#element_blank()
                grid.maj.y=c()#element_blank()
                grid.min.y=c()#element_blank()
                
                xlabel=glue("Gender balance (% {propgender}) {year1}")
                diagline=1
                vertline=0
                if(input$prestigeplotDifference){
                    diagline=0
                    vertline=1
                    xlabel=glue("Percent change (% {propgender}) from {year1} to {year2}")                    
                }
                if(input$gridSwitch){
                    grid.maj.x=c(0,25,50,75,100)
                    grid.maj.y=c(0,25,50,75,100)
                    grid.min.x=c(12.5, 37.5, 62.5, 87.5)
                    grid.min.y=c(12.5, 37.5, 62.5, 87.5)
                    if(input$prestigeplotDifference){
                        grid.maj.x = grid.maj.x-50
                        grid.min.x = grid.min.x-50
                    }
                }
                ggplot(NULL)+
                    geom_rect(data=bgdat, aes(xmin=-Inf, xmax=Inf, ymin=v, ymax=v+1, fill=c), alpha=1, show.legend=F)+
                    scale_fill_gradientn(colours=c("#6b9733","#fec200","#b64a1a"))+
                    #scale_fill_gradient(high = input$propStartColour,
                    #                    low = input$propEndColour)+
                    #geom_function(fun= ~ 50, xlim=c(-50,50))+
                    geom_vline(xintercept = 0, color="white", size=vertline)+
                    geom_abline(slope=1, intercept=0, color="white", size=diagline)+
                    geom_vline(xintercept = grid.maj.x, color="white", size=0.5)+
                    geom_vline(xintercept = grid.min.x, color="white", size=0.1)+
                    geom_hline(yintercept = grid.maj.y, color="white", size=0.5)+
                    geom_hline(yintercept = grid.min.y, color="white", size=0.1)+
                    geom_point(data=d, aes(x=prop1,y=prop2,size=pmax(n1,n2)))+
                    #coord_fixed(xlim=c(-50,50),ylim=c(0,100))+
                    scale_x_continuous(expand = c(.0, .0), limits=c(0,100)) +
                    scale_y_continuous(expand = c(.0, .0), limits=c(0,100)) +
                    #scale_x_continuous(limits = c(-50,50), expand = c(0, 0)) +
                    #scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
                    scale_size_continuous(limits = c(min(pmax(d$n1,d$n2)),max(pmax(d$n1,d$n2))),
                                          labels=\(lab) sprintf("<%.0f",lab))+
                    geom_text_repel(data=d,aes(x=prop1, y=prop2,label=unit),
                                    #arrow = arrow(),#length = unit(0.03, "npc"), type = "closed", ends = "first"),
                                    force=2)+
                    theme_bw()+
                    theme(#panel.grid.major=grid.maj,
                          #panel.grid.minor=grid.min,
                          #panel.ontop=input$gridSwitch,
                          panel.background = element_rect(fill = NA))+
                    labs(x=xlabel, 
                         y=glue("Gender balance (% {propgender}) {year2}"),
                         size="Total N") -> p
                if(input$includeBalancinatorBrandingPrestigeplot){
                    p<-p+labs(caption=balancinator_branding)+
                        theme(plot.caption=element_text(hjust = 1),
                              plot.caption.position = 'plot')
                }
                prestigeplot_objects[[my_i]] <<- p
                #ggsave(plot=p, filename="prestigeplot.pdf")
                p
            })
        })
    }
    output$downloadPrestigePlot <- downloadHandler(
        filename = function() { "prestigeplot.pdf"  },
        content = function(file) {
            w_in=input$prestigeplot_size/90 
            h_in=w_in
            pdf("prestigeplot.pdf", width=w_in, height=h_in)
            lapply(prestigeplot_objects, print)
            dev.off()
            file.copy("prestigeplot.pdf", file, overwrite=TRUE)
        }
    )
    # --------------------------------------------------------------
    # BUTTONS
    # --------------------------------------------------------------
    observeEvent(input$zeroButton, {
        g_data[,-1] <<- 0
    })
    
    observeEvent(input$randomButton, {
        datproxy = dataTableProxy('balancedata')
        n=prod(dim(g_data[,-1]))
        a=sample(ranmin:ranmax, n, replace=T)
        dim(a) <- dim(g_data[,-1])
        g_data[,-1] <<- a
        #print(g_data)
        #replaceData(datproxy, g_data, resetPaging = TRUE)  # important
    })
    observeEvent(input$testButton, {
        print(g_years)
        print(g_data)
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
        #req_cols=c("year","unit","gender","freq")
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
        
        req(d)
        
        g_data <<- d
        #print(g_data)
        all.years=as.integer(unique(map_chr(str_split(names(g_data)[-1], pattern="\\."), ~ .x[1])))
        ndeps=dim(g_data)[1]
        updatePickerInput(session, "years", selected=all.years)
        proxy = dataTableProxy('balancedata')
        replaceData(proxy, g_data, resetPaging = FALSE)  # important
        
        
        #g_data <<- editData(g_data, input$balancedata_cell_edit, 'balancedata', rownames=F)
        
        # rebuild UI
        #updateSliderInput(session, "nyears", value=nyears)
        updateSliderInput(session, "ndeps", value=ndeps)
    })
    
    # Download dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            sprintf("balancinator_data.%s", input$downloadFormat)
            },
        content = function(file) {
            if(input$downloadFormat=="csv"){
                write.csv(g_data, file, row.names = FALSE)
            } else {
                writexl::write_xlsx(g_data, file)
            }
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
