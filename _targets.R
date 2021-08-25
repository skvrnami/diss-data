library(targets)
library(tarchetypes)

source("R/loading-data.R")

tar_option_set(packages = c("dplyr", "rvest", "here", "listr", # "rimr", 
                            "readxl"))

# psp_2021 <- read_excel(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrk.xlsx"))

# TODO: convert .dbf to csv in LibreOffice
# TODO: install rimr

list(
    
    #######################################################
    # Chamber of Deputies
    #######################################################
    
    tar_target(psp_2013, command = {
        # TODO
    }),
    
    tar_target(psp_2017, command = {
        psp_parties <- read_parties(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrkl.xlsx"))
        cpp <- read_cpp(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cpp.xlsx"))
        cns <- read_cns(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cns.xlsx"))
        read_candidates(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrk.xlsx"), 
                        psp_parties, cpp, cns)
    }),
    
    tar_target(psp_2021, command = {
        psp_parties <- read_parties(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrkl.xlsx"))
        cpp <- read_cpp(here("data", "PS2021", "PS2021ciselniky20210824", "cpp.xlsx"))
        cns <- read_cns(here("data", "PS2021", "PS2021ciselniky20210824", "cns.xlsx"))
        read_candidates(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrk.xlsx"), 
                        psp_parties, cpp, cns)
    }), 
    
    # Preferential votes
    # tar_target(psp_2006_votes, parse_preferential_votes(here("data", "PS2006", "vysledky_kandid.xml"))),
    # tar_target(psp_2010_votes, parse_preferential_votes(here("data", "PS2010", "vysledky_kandid.xml"))),
    # tar_target(psp_2013_votes, parse_preferential_votes(here("data", "PS2013", "vysledky_kandid.xml"))), 
    # tar_target(psp_2017_votes, parse_preferential_votes(here("data", "PS2017NSS", "vysledky_kandid.xml"))), 
    
    #######################################################
    # Regional elections
    #######################################################
    
    tar_target(reg_2016, command = {
        # parties <- read_parties(here("data", "KZ2016", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
        #     unique
        cpp <- read_cpp(here("data", "KZ2016", "KZ2016ciselniky20161007", "cpp.xlsx"))
        cns <- read_cns(here("data", "KZ2016", "KZ2016ciselniky20161007", "cns.xlsx"))
        NULL
        # read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
        #                 parties, cpp, cns)
    })
    
    tar_target(reg_2020, command = {
        parties <- read_parties(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
            unique
        cpp <- read_cpp(here("data", "KZ2020", "KZ2020ciselniky20201002", "cpp.xlsx"))
        cns <- read_cns(here("data", "KZ2020", "KZ2020ciselniky20201002", "cns.xlsx"))
        read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
                        parties, cpp, cns)
    }),
    
    NULL
)