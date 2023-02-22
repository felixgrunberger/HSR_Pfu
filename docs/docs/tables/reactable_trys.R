# libraries ----
library(reactablefmtr)
library(htmltools)
library(tidyverse)
library(readxl)

# data ----
tab <- read_xlsx("/Users/felix/Documents/R/GITHUB/HSR_Pfu/docs/data/counts_tables.xlsx")
tab <- read_xlsx("../../data/counts_tables.xlsx")

# table ----
with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: none; text-decoration-style: none; cursor: help",
            title = tooltip, value)
}
orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ff9500"))(x), maxColorValue = 255)

# color scales ----
BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)
yesno  <- function(x){
  if(x <= 0.05){
    return("grey")
  }else{
    return("white")
  }
}

# reactable ----
colorsetter  <- function(c1,c2){
  if(c1 > 0 & c2 <= 0.05){
    return("#9B0010")
  }else if(c1 < 0 & c2 <= 0.05){
    return("#0C607F")
  }else{
    return("#5C5C5C")
  }
}
ncol(tab)
tab[,c(16,18,20)][is.na(tab[,c(16,18,20)])] <- 0
tab[,c(17,19,21)][is.na(tab[,c(17,19,21)])] <- 1
tab1 <- tab %>%
  rowwise() %>%
  dplyr::mutate(col_HS1_RNA = colorsetter(HS1_FC_RNA,HS1_padj_RNA),
                col_HS2_RNA = colorsetter(HS2_FC_RNA,HS2_padj_RNA),
                col_HS3_RNA = colorsetter(HS3_FC_RNA,HS3_padj_RNA),
                col_HS1_MS  = colorsetter(HS1_FC_MS,HS1_padj_MS),
                col_HS2_MS  = colorsetter(HS2_FC_MS,HS2_padj_MS),
                col_HS3_MS  = colorsetter(HS3_FC_MS,HS3_padj_MS))
                
tab2 <- tab1[,c(1,2,3,7,8,9,10,12,14,16,18,20,30,31,32,33,34,35)]
tab3 <- tab2[,1:12]

reactable(data = tab3,
          theme = fivethirtyeight(),
          defaultPageSize = 15,
          width = "auto",
          searchable = TRUE,
          filterable = TRUE,
          highlight = TRUE,
          columns = list(
            id = colDef(
              header = with_tooltip("Gene ID", "Gene ID - Links to Uniprot DB"),
              html = TRUE,
              cell = JS("function(cell) {
              return '<a href=\"https://www.uniprot.org/uniprot/?query=pyrococcus+furiosus+' + cell.value + '\">' + cell.value + '</a>'
              }")
            ),
            paperblast = colDef(
              header = with_tooltip("PaperBLAST", "Find paper about a protein or its homologs"),
              html = T,
              cell = JS("function(cell) {
              return '<a href=\"https://papers.genomics.lbl.gov/cgi-bin/litSearch.cgi?query=+' + cell.value + '\">' + 
'PaperBLAST' + '</a>'
              }")
            ),
            old_id = colDef(
              header = "PF numbers"
            ),
            HS1_FC_RNA = colDef(cell = setCelCol(tab2,"col_HS1_RNA")),
            HS2_FC_RNA = colDef(cell = setCelCol(tab2,"col_HS2_RNA")),
            HS3_FC_RNA = colDef(cell = setCelCol(tab2,"col_HS3_RNA")),
            HS1_FC_MS = colDef(cell = setCelCol(tab2,"col_HS1_MS")),
            HS2_FC_MS = colDef(cell = setCelCol(tab2,"col_HS2_MS")),
            HS3_FC_MS = colDef(cell = setCelCol(tab2,"col_HS3_MS"))))

setCelCol = function(data,col){
  return(data_bars(data, 
                   text_position = "above", 
                   max_value = as.numeric(max(tab2[,7:12])),
                   text_color_ref = eval(col),
                   fill_opacity = 1, 
                   fill_color_ref = eval(col)
  ))
}
