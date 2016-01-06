# Load libraries
library(shiny)
library(ggplot2)
library(grid)
library(jsonlite)

# Load keboola library
library(keboola.shiny.lib)

shinyServer(function(input, output, session) {    
    # create instance of keboola helper library
    klib <- KeboolaShiny$new()
    
    keboola <- reactive({
        # what does this application need to load from sapi
        tables <- list(
            "groupData" = list(name="GDT__1")
        )
        
        # start it up
        ret <- klib$startup(list(appTitle = "Distribution Groups",
                                  tables = tables,
                                  cleanData = FALSE,
                                  dataToSave = NULL,
                                  configCallback = NULL,
                                  description = FALSE,
                                  customElements = NULL))
        
        # return our sourceData
        klib$sourceData()()
    })
    
    sourceData <- reactive({
        dataSet <- keboola()
        print("source data")
        if (length(dataSet) == 0) {
            # app startup has not finished or authentication failure
            NULL
        } else {
            dataSet$groupData <- dataSet$groupData[,!names(dataSet$groupData) %in% c("run_id")]
            
            # for this app we retrieve the descriptor separately ** this is not ideal **
            dataSet$descriptor <- klib$kdat$getDescriptor()
            
            # get the target column from the descriptor and set as numeric
            targetColumn <- dataSet$descriptor$parameters$target
            dataSet$groupData[[targetColumn]] <- as.numeric(dataSet$groupData[[targetColumn]])
            names(dataSet$groupData)[names(dataSet$groupData) == 'automatic_bin'] <- 'group_name' 
            dataSet$groupData$group_name <- as.factor(dataSet$groupData$group_name)        
            
            # return our sourceData
            dataSet
        }
    })

    # this will trigger when the sourceData is loaded, 
    # so It's the best place to update input options.
    observe({
        dataSet <- sourceData()
        if (!is.null(dataSet$groupData)){
            updateSelectInput(session, 'listGroup', label = 'Show group', 
                              choices = levels(dataSet$groupData$group_name))    
        }
    })

    #   Render custom elements in description. Use this method to handle
    #   rendering of plots and other custom elements in descriptor. This method
    #   is used as a callback in getDescription(), it is not called directly.
    customElements <- function(elementId, content) {
        if (elementId == 'histogramEstimate') {
            ret <- plotGraph(elementId, content$target, content$selectedGroup)
        } else if (elementId == 'boxPlot') {
            ret <- plotBoxplot(elementId, content$target)
        } else if (elementId == 'tableData') {
            data <- sourceData()$groupData
            groupData <- data[which(data$group_name == tolower(content$groupName)),]
            ret <- shiny::renderDataTable(groupData)
        }
        ret
    }
    
    # since we are not getting the descriptor in the startup, but in the sourceData
    # we need to manually populate the description output.  Normally the startup will handle this.
    output$description <- renderUI({
        if (!is.null(sourceData())) {
            contentRet <- list()
            descriptor <- sourceData()$descriptor
            # keep only selected groups in descriptor if any
            if (length(input$listGroup) > 0) {
                sect <- list()
                for (subsection in descriptor$sections[[3]]$subsections) {
                    print(paste("getting subsection",subsection$title))
                    print(input$listGroup)
                    if (tolower(subsection$title) %in% input$listGroup) {
                        sect[[length(sect) + 1]] <- subsection
                    }
                }
                descriptor$sections[[3]]$subsections <- sect
            }
            
            print("description finished")        
            klib$kdat$getDescription("Distribution Groups", customElements, descriptor)    
        }
        
    })
    
    # this function creates a ggplot2 boxplot and returns it as an image
    plotBoxplot <- function(elementId, target) {
        # make box plot of the groups
        print("plotboxplot")
        data <- sourceData()$groupData
        outfile <- tempfile(fileext = ".png")
        gg <- ggplot(data, aes_string(x = "group_name", y = target)) + 
            geom_boxplot(aes(fill = group_name), size = 0.2) +
            theme(axis.text=element_text(size = 4, angle = 90), axis.title=element_text(size = 5), 
                      legend.text = element_text(size = 4), legend.title=element_text(size = 5),
                      legend.key.size = unit(0.2,"cm")) +
            theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
        ggsave(filename = outfile, width = 8, height = 5, units = "cm", dpi = 300, scale = 1)
        
        on.exit(unlink(outfile))
        img(src = session$fileUrl(elementId, outfile, contentType='image/png'))
    }
    # creates a plot as an image
    plotGraph <- function(elementId, target, selectedGroup) {
        print("plotGraph")
        outfile <- tempfile(fileext = ".png")
        data <- sourceData()$groupData
        group <- c()
        meanValue <- c()
        for (grp in levels(data$group_name)) {
            group <- c(group, grp)
            colData <- data[which(data$group_name == grp), target]
            meanValue <- c(meanValue, mean(colData))
        }
        inputDataMeans <- data.frame(group_name = group, meanValue)
        inputMean <- data.frame(meanValue = mean(data[[target]]))
        # make a correction of the x axis range
        xmin <- min(data[[target]])
        if (max(data[[target]]) > 100 * (inputMean - xmin)) {
            # if there should be 100x more space to the right of the mean, 
            # trim it to 10times more only so that longtailed distribution is plotted 
            xmax <- (inputMean - xmin) * 10
        } else {
            xmax <- max(data[[target]])
        }
        gg <- ggplot(data, aes_string(x = target)) +
            # density for the whole dataset
            geom_density(size = 0.5, linetype = "dotted") +
            # mean for whole dataset
            geom_vline(data = inputMean, aes(xintercept = meanValue), 
                       linetype = "dotted", size = 0.5) +
            geom_density(aes(colour = group_name)) + 
             # mean for each group
            geom_vline(data = inputDataMeans, aes(xintercept = meanValue, colour = group_name), 
                       linetype = "dashed", size = 0.5) +
            xlim(as.integer(xmin), as.integer(xmax)) +
            theme(axis.text=element_text(size = 4), axis.title=element_text(size = 5),
                  legend.text=element_text(size = 4), legend.title=element_text(size = 5),
                  legend.key.size=unit(0.2,"cm")) +
            theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
        if (!is.null(selectedGroup)) {
            groupData <- data[which(data$group_name == selectedGroup), ]
            groupMean <- inputDataMeans[which(inputDataMeans$group_name == selectedGroup), ]
            gg <- gg + geom_density(data = groupData, size = 1, linetype = "solid", aes(colour = group_name)) +
                geom_vline(data = groupMean, aes(xintercept = meanValue, colour = group_name),
                       linetype = "dashed", size = 1)
        }
        ggsave(filename = outfile, width = 8, height = 5, units = "cm", dpi = 300, scale = 1)
        
        on.exit(unlink(outfile))
        img(src = session$fileUrl(elementId, outfile, contentType='image/png'))
    }
})


