#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import dplyr
#' @import ggplot2
#' @import ggsignif
#' @import ggbeeswarm
#' @noRd
setClass("calcOut", slots = list(dataOut = "data.frame", pvalue = "vector"))

calc <- function(dataIn, group, ctrl, gene, int, value, test, p.adjust.method, geneShow) {
    data <- as.data.frame(dataIn)
    data[, value] <- as.numeric(data[, value])
    avgInt <- aggregate(
        as.formula(paste(value, "~", group)),
        data[data[, gene] == int, ],
        mean
    )
    colnames(avgInt)[2] <- "avgInt"

    data2 <- data[data[, gene] != int, ]
    data2 <- merge(data2, avgInt, by = group)
    data2$dCt <- data2[, value] - data2$avgInt

    avgCtrl <- aggregate(
        as.formula(paste("dCt", "~", gene)),
        data2[data2[, group] == ctrl, ],
        mean
    )
    colnames(avgCtrl)[2] <- "avgCtrl"

    data3 <- merge(data2, avgCtrl, by = gene)
    data3$ddCt <- data3$dCt - data3$avgCtrl

    data3$`2^(-ddCt)` <- 2^(-data3$ddCt)

    data3 <- data3[order(data3[, gene], data3[, group]), ]

    groups <- setdiff(unique(data[, group]), ctrl)
    pvalue <- list()
    for (i in groups) {
        if (test == "t.test") {
            tmp <- t.test(
                x = data3[((data3[, group] == i) & (data3[, gene] == geneShow)), "2^(-ddCt)"],
                y = data3[((data3[, group] == ctrl) & (data3[, gene] == geneShow)), "2^(-ddCt)"]
            )
        } else if (test == "wilcox.test") {
            tmp <- wilcox.test(
                x = data3[((data3[, group] == i) & (data3[, gene] == geneShow)), "2^(-ddCt)"],
                y = data3[((data3[, group] == ctrl) & (data3[, gene] == geneShow)), "2^(-ddCt)"]
            )
        }
        pvalue <- append(pvalue, tmp$p.value)
    }
    pvalue <- p.adjust(pvalue, method = p.adjust.method)
    print(pvalue)

    calcOut <- new("calcOut", dataOut = data3, pvalue = pvalue)
    return(calcOut)
}

myPlot <- function(dataOut, test, group, ctrl, gene, pvalue, styleComparisonLine, stylePvalue, title, xlab, ylab, show.legend, theme, geneShow) {
    data <- dataOut[dataOut[, gene] == geneShow,]
    data[, group] <- factor(data[, group], levels = c(ctrl, setdiff(unique(data[, group]), ctrl)))

    mid <- aggregate(as.formula(paste("2^(-ddCt)~", group)), data, mean)
    high <- aggregate(as.formula(paste("2^(-ddCt)~", group)), data, function(x){mean(x) + sd(x)})
    low <- aggregate(as.formula(paste("2^(-ddCt)~", group)), data, function(x){mean(x) - sd(x)})
    pData <- merge(merge(mid, high, by = group), low, by = group)
    colnames(pData) <- c(group, "mid", "high", "low")

    n <- length(unique(data[, group]))
    max <- max(data[, "2^(-ddCt)"])

    if (styleComparisonLine == "┌─┐") {
        tip_length <- 0.05
    } else if (styleComparisonLine == "──") {
        tip_length <- 0
    }

    if (stylePvalue == "P 值") {
        sign <- round(pvalue, 3)
        sign[sign < 0.001] <- "<0.001"
    } else if (stylePvalue == "星号") {
        sign <- as.character(symnum(
            x = pvalue,
            cutpoints = c(0, 0.001, 0.01, 0.05, 1),
            symbols = c("***", "**", "*", "NS"
            )))
    }

    p <- ggplot() +
        geom_bar(
            data = pData,
            aes_string(x = group, y = "mid", fill  = group),
            color = "black",
            stat = "identity",
            width = 0.6,
            show.legend = show.legend
        ) +
        geom_errorbar(
            data = pData,
            mapping = aes_string(
                x = group,
                ymin = "low",
                ymax = "high"
            ),
            width = 0.2,
            show.legend = FALSE
        ) +
        geom_beeswarm(
            data = data,
            aes_string(x = group, y = "2^(-ddCt)", fill  = group),
            color = "black",
            shape = 21,
            size = 3,
            cex = 3,
            show.legend = FALSE
        ) +
        labs(title = title, x = xlab, y = ylab) +
        geom_signif(
            data = pData,
            aes_string(x = group, y = "mid"),
            xmin = rep(1, n-1),
            xmax = seq(2, n, by = 1),
            y_position = seq(from = 1.1 * max, to = (1 + 0.1 * (n-1)) * max, by = 0.1 * max),
            annotations = sign,
            tip_length = tip_length,
            textsize = 6,
            show.legend = FALSE
        ) +
        scale_y_continuous(limits = c(0, (1 + 0.1 * n) * max), expand = c(0, 0, 0, 0)) +
        themes[theme] +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
        )
    return(p)
}


app_server <- function(input, output, session) {
    #### read dataIn ####
    dataIn <- reactive({
        fileIn <- input$fileIn
        ext <- tools::file_ext(fileIn$datapath)

        req(fileIn)
        if (ext == "csv") {
            read.csv(fileIn$datapath)
        } else if (ext == "tsv") {
            read.delim(fileIn$datapath)
        } else if (ext %in% c("xlsx", "xls")) {
            readxl::read_excel(fileIn$datapath)
        }
    })

    dataOut <- reactive({
        calc(dataIn(), input$group, input$ctrl, input$gene, input$int, input$value, input$test, input$p.adjust.method, input$geneShow)@dataOut
    })

    pvalue <- reactive({
        calc(dataIn(), input$group, input$ctrl, input$gene, input$int, input$value, input$test, input$p.adjust.method, input$geneShow)@pvalue
    })

    pOut <- reactive({
        myPlot(dataOut(), input$test, input$group, input$ctrl, input$gene, pvalue(), input$styleComparisonLine, input$stylePvalue, input$title, input$xlab, input$ylab, input$show.legend, input$theme, input$geneShow)
    })

    observeEvent(input$fileIn, {
        #### show dataIn ####
        output$dataIn <- renderTable({
            dataIn()
        })
        #### update choices ####
        updateSelectInput(session, inputId = "group", choices = colnames(dataIn()))
        updateSelectInput(session, inputId = "gene", choices = colnames(dataIn()))
        updateSelectInput(session, inputId = "value", choices = colnames(dataIn()))
    })
    observeEvent(input$group, {
        updateSelectInput(session, inputId = "ctrl", choices = unique(dataIn()[, input$group]))
        updateTextInput(session, inputId = "xlab", value = input$group)
    })
    observeEvent(input$gene, {
        updateTextInput(session, inputId = "title", value = input$gene)
        updateSelectInput(session, inputId = "int", choices = unique(dataIn()[, input$gene]))
    })
    observeEvent(input$int, {
        updateSelectInput(session, inputId = "geneShow", choices = setdiff(unique(dataIn()[, input$gene]), input$int))
    })

    observeEvent(input$calc, {
        #### show dataOut ####
        output$dataOut <- renderTable({
            dataOut()
        })
    })

    observeEvent(input$draw, {
        #### show plotOut ####
        output$pOut <- renderPlot(
            {
                pOut()
            },
            width = width(),
            height = height()
        )
    })

    observeEvent(input$slideWidth, {
        updateTextInput(session, inputId = "width", value = input$slideWidth)
    })
    observeEvent(input$slideHeight, {
        updateTextInput(session, inputId = "height", value = input$slideHeight)
    })
    observeEvent(input$width, {
        updateSliderInput(session, inputId = "slideWidth", value = input$width)
    })
    observeEvent(input$height, {
        updateSliderInput(session, inputId = "slideHeight", value = input$height)
    })

    width <- reactive({
        as.numeric(input$width)
    })
    height <- reactive({
        as.numeric(input$height)
    })

    observeEvent(input$geneShow, {
        output$pOut <- renderPlot(
            {
                pOut()
            },
            width = width(),
            height = height()
        )
    })

    output$csvDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".csv")
        },
        content = function(file){
            write.csv(dataOut(), file, row.names = F)
        }
    )
    output$tsvDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".tsv")
        },
        content = function(file){
            write.table(dataOut(), file, sep = "\t", row.names = F)
        }
    )
    output$excelDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".xlsx")
        },
        content = function(file){
            writexl::write_xlsx(dataOut(), file)
        }
    )

    output$jpgDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".jpg")
        },
        content = function(file){
            jpg(
                file,
                width = width(),
                height = height()
            )
            print(pOut())
            dev.off()
        }
    )
    output$pngDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".png")
        },
        content = function(file){
            png(
                file,
                width = width(),
                height = height()
            )
            print(pOut())
            dev.off()
        }
    )
    output$tifDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".tiff")
        },
        content = function(file){
            tiff(
                file,
                width = width(),
                height = height()
            )
            print(pOut())
            dev.off()
        }
    )
    output$pdfDown <- downloadHandler(
        filename = function(){
            paste0(Sys.time(), ".pdf")
        },
        content = function(file){
            cairo_pdf(
                file,
                width = width()/72,
                height = height()/72
            )
            print(pOut())
            dev.off()
        }
    )
}
