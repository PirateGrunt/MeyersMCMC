library(dplyr)
# library(ChainLadder)
library(raw)

lines <- c("comauto"
           , "ppauto"
           , "wkcomp"
           , "othliab"
           , "prodliab"
           , "medmal")

GetAllTriangles <- function(Line){
  require(raw)
  data(list = Line)
  assign("df", get(Line))

  rm(list = Line, envir = .GlobalEnv)
  
  df
}

# function to get Schedule P triangle data given ins group and line of business
SingleTriangle <- function(dfSchedP, WhichGroup){
  
  if ("GRCODE" %in% names(dfSchedP)) dfSchedP <- raw::CasColNames(dfSchedP, restore = FALSE)
  
  maxAY <- max(dfSchedP$AccidentYear)
  minAY <- min(dfSchedP$AccidentYear)
  
  dfSchedP <- dfSchedP %>% 
    filter(GroupCode == WhichGroup) %>% 
    arrange(AccidentYear, DevelopmentYear) %>% 
    mutate(IncrementalIncurred = CumulativeIncurred - dplyr::lag(CumulativeIncurred)
           , IncrementalPaid = CumulativePaid - dplyr::lag(CumulativePaid)
           , Upper = DevelopmentYear <= maxAY
           , AY_Index = AccidentYear - minAY + 1)
  
  dfSchedP
}