#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @noRd
#### UI ####
themes = list(
    "Default" = theme_classic()
)

#### uiFile ####
uiUpload <- fluidRow(
    br(),
    fileInput(
        inputId = "fileIn",
        label = "上传文件",
        buttonLabel = "浏览",
        placeholder = "请选择文件",
        accept = list(".csv", ".tsv", ".xlsx", ".xls")
    )
)

uiCalcConf <- fluidRow(
    selectInput(
        inputId = "test",
        label = "检验方法",
        choices = c(
            "t.test",
            "wilcox.test"
        ),
        selected = "wilcox.test"
    ),
    selectInput(
        inputId = "p.adjust.method",
        label = "p 值校正方法",
        choices = c(
            "bonferroni",
            "holm",
            "fdr"
        ),
        selected = "fdr"
    ),
    hr(),
    # Group
    fluidRow(
        column(
            6,
            selectInput(
                inputId = "group",
                label = "分组列",
                choices = c()
            )
        ),
        column(
            6,
            selectInput(
                inputId = "ctrl",
                label = "对照组",
                choices = c()
            )
        )
    ),
    # Gene
    fluidRow(
        column(
            6,
            selectInput(
                inputId = "gene",
                label = "基因列",
                choices = c()
            )
        ),
        column(
            6,
            selectInput(
                inputId = "int",
                label = "内参基因",
                choices = c()
            )
        )
    ),
    fluidRow(
        column(
            6,
            selectInput(
                inputId = "value",
                label = "Ct 值",
                choices = c()
            )
        ),
        column(
            6,
            align = "center",
            br(),
            actionButton(inputId = "calc", label = "计算")
        )
    )
)

uiDownload <- fluidRow(
    column(
        6,
        strong("导出表格"),br(),
        downloadButton(outputId = "csvDown", label = "CSV", style = "width:100%"),br(),
        downloadButton(outputId = "tsvDown", label = "TSV", style = "width:100%"),br(),
        downloadButton(outputId = "excelDown", label = "EXCEL", style = "width:100%")
    ),
    column(
        6,
        strong("导出图片"),br(),
        downloadButton(outputId = "jpgDown", label = "JPG", style = "width:100%"),br(),
        downloadButton(outputId = "pngDown", label = "PNG", style = "width:100%"),br(),
        downloadButton(outputId = "tifDown", label = "TIFF", style = "width:100%"),br(),
        downloadButton(outputId = "pdfDown", label = "PDF", style = "width:100%")
    )
)

uiFile <- fluidPage(
    uiUpload,
    uiCalcConf,
    hr(),
    uiDownload
)

#### uiPlotConf ####
uiPvalueConf <- fluidRow(
    fluidRow(
        column(
            6,
            selectInput(
                inputId = "styleComparisonLine",
                label = "比较线样式",
                choices = c(
                    "┌─┐",
                    "──"
                ),
                selected = "┌─┐"
            )
        ),
        column(
            6,
            selectInput(
                inputId = "stylePvalue",
                label = "P 值/星号",
                choices = c(
                    "P 值",
                    "星号"
                ),
                selected = "星号"
            )
        )
    )
)

uiLabelConf <- fluidRow(
    column(
        12,
        textInput(
            inputId = "title",
            label = "图标题"
        ),
        textInput(
            inputId = "xlab",
            label = "X 轴标题"
        ),
        textInput(
            inputId = "ylab",
            label = "Y 轴标题",
            value = "2^(-ΔΔCt)"
        )
    )
)

uiLegendConf <- fluidRow(
    column(
        6,
        checkboxInput(
            inputId = "show.legend",
            label = "显示图例",
            value = T
        )
    ),
    column(
        6,
        align = "center",
        actionButton(
            inputId = "draw",
            label = "画图"
        )
    )
)

uiThemeConf <- fluidRow(
    selectInput(
        inputId = "theme",
        label = "选择主题",
        choices = names(themes),
        selected = "Default"
    )
)

uiSizeConf <- fluidRow(
    textInput(
        inputId = "width",
        label = "宽度",
        value = 480
    ),
    sliderInput(
        inputId = "slideWidth",
        label = NULL,
        min = 128, max = 1024, step = 1,
        value = 480,
        ticks = FALSE
    ),
    textInput(
        inputId = "height",
        label = "高度",
        value = 480
    ),
    sliderInput(
        inputId = "slideHeight",
        label = NULL,
        min = 128, max = 1024, step = 1,
        value = 480,
        ticks = FALSE
    )
)

uiPlotConf <- fluidPage(
    br(),
    uiPvalueConf,
    hr(),
    uiLabelConf,
    hr(),
    uiThemeConf,
    uiSizeConf,
    uiLegendConf
)

#### uiFooter ####
uiFooter <- fluidRow(
    p(
        "bilibili: @默默杉",
        align = "center"
    )
)

app_ui <- function(request) {
    fluidPage(
        column(
            12,
            title = "qPCR 计算器",
            titlePanel("qPCR 计算器"),
            fluidRow(
                sidebarLayout(
                    sidebarPanel(
                        tabsetPanel(
                            type = "pills",
                            tabPanel("文件设置", uiFile),
                            tabPanel("图形设置", uiPlotConf)
                        )
                    ),
                    mainPanel(
                        tabsetPanel(
                            type = "tabs",
                            tabPanel("原始表", tableOutput("dataIn")),
                            tabPanel("结果表", tableOutput("dataOut")),
                            tabPanel(
                                "绘图",
                                selectInput(
                                    inputId = "geneShow",
                                    label = "目的基因",
                                    choices = c()
                                ),
                                plotOutput("pOut")
                            )
                        )
                    )
                )
            ),
            hr(),
            uiFooter
        )
    )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "qPCRcalculator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
