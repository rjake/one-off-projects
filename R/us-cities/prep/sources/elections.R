# Elections ----
"-----------------------------------------------------------------
Data from the 2024 elections
------------------------------------------------------------------"
# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)


if (FALSE) {
  download.file(
    "https://github.com/tonmcg/US_County_Level_Election_Results_08-24/raw/refs/heads/master/2024_US_County_Level_Presidential_Results.csv",
    "output/elections-2024.csv"
  )
}

elections <- readr::read_csv("output/elections-2024.csv")
