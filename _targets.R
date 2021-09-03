library(targets)
library(tarchetypes)

source("R/loading-data.R")
source("R/matching.R")

tar_option_set(packages = c("dplyr", "rvest", "here", # "listr", 
                            "rimr", "readxl", "readr"))

# TODO: zkontrolovat rekodovani titulu

list(
    
    # TODO: Parliament 1990, 1992 -------------------------
    
    # TODO: FS 1990, 1992
    # TODO: Czech National Council 1990, 1992
    
    #######################################################
    # Chamber of Deputies ---------------------------------
    #######################################################
    
    # TODO: kraje (1992, 1996, 1998)?
    
    tar_target(psp_1996, command = {
        cpp <- read_cpp(here("data", "PS1996", "CPPPS96.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
        read_excel(here("data", "PS1996", "PS96-RK.xlsx")) %>%
            clean_ps(., VSTRANA_MAP_96) %>%
            left_join(., cpp, by = "PSTRANA") %>%
            merge_and_recode_titles %>%
            mutate(row_id = row_number(), 
                   ROK_NAROZENI = 1996 - VEK)
    }),
    
    tar_target(psp_1998, command = {
        cpp <- read_cpp(here("data", "PS1998", "CPPPS98.xlsx"), function(x) {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
        read_excel(here("data", "PS1998", "PS98-RK.xlsx")) %>%
            clean_ps(., VSTRANA_MAP_98) %>%
            left_join(., cpp, by = "PSTRANA") %>%
            merge_and_recode_titles %>%
            mutate(row_id = row_number(), 
                   ROK_NAROZENI = 1998 - VEK)
    }),
    
    tar_target(psp_2002, command = {
        cpp <- read_cpp(here("data", "PS2002", "CPPPS02.xlsx"))
        cns <- read_cns(here("data", "PS2002", "CNSPS02.xlsx"))
        read_excel(here("data", "PS2002", "PS02-RK.xlsx")) %>%
            rename(NAZEV_STRK = KSTRANA_NAZEV) %>%
            left_join(., cpp, by = "PSTRANA") %>%
            left_join(., cns, by = "NSTRANA") %>%
            merge_and_recode_titles %>%
            mutate(row_id = row_number(), 
                   ROK_NAROZENI = 2002 - VEK)
    }),
    
    tar_target(psp_2006, command = {
        psp_parties <- read_parties(here("data", "PS2006", "PS2006reg2006", "PSRKL.xlsx"), 
                                    clean_dbf_excel_colnames)
        cpp <- read_cpp(here("data", "PS2006", "PS2006ciselniky2006", "CPP.xlsx"), 
                        clean_dbf_excel_colnames)
        cns <- read_cns(here("data", "PS2006", "PS2006ciselniky2006", "CNS.xlsx"), 
                        clean_dbf_excel_colnames)
        read_candidates(here("data", "PS2006", "PS2006reg2006", "PSRK.xlsx"), 
                        psp_parties, cpp, cns, clean_dbf_excel_colnames)
    }),
    
    tar_target(psp_2010, command = {
        psp_parties <- read_parties(here("data", "PS2010", "PS2010reg2010", "PSRKL.xlsx"), 
                                    clean_dbf_excel_colnames)
        cpp <- read_cpp(here("data", "PS2010", "PS2010ciselniky2010", "CPP.xlsx"), 
                        clean_dbf_excel_colnames)
        cns <- read_cns(here("data", "PS2010", "PS2010ciselniky2010", "CNS.xlsx"), 
                        clean_dbf_excel_colnames)
        read_candidates(here("data", "PS2010", "PS2010reg2010", "PSRK.xlsx"), 
                        psp_parties, cpp, cns, clean_dbf_excel_colnames)
    }),
    
    tar_target(psp_2013, command = {
        psp_parties <- read_parties(here("data", "PS2013", "PS2013reg20131026", "PSRKL.xlsx"), 
                                    clean_dbf_excel_colnames)
        cpp <- read_cpp(here("data", "PS2013", "PS2013ciselniky20131021", "CPP.xlsx"), 
                        clean_dbf_excel_colnames)
        cns <- read_cns(here("data", "PS2013", "PS2013ciselniky20131021", "CNS.xlsx"), 
                        clean_dbf_excel_colnames)
        read_candidates(here("data", "PS2013", "PS2013reg20131026", "PSRK.xlsx"), 
                        psp_parties, cpp, cns, clean_dbf_excel_colnames)
    }),
    
    tar_target(psp_2017, command = {
        psp_parties <- read_parties(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrkl.xlsx"))
        cpp <- read_cpp(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cpp.xlsx"))
        cns <- read_cns(here("data", "PS2017NSS", "PS2017ciselniky20170905", "cns.xlsx"))
        read_candidates(here("data", "PS2017NSS", "PS2017reg20171122_xlsx", "psrk.xlsx"), 
                        psp_parties, cpp, cns) %>%
            mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
                   MANDAT = ifelse(MANDAT == "A", 1, 0))
    }),
    
    tar_target(psp_2021, command = {
        psp_parties <- read_parties(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrkl.xlsx"))
        cpp <- read_cpp(here("data", "PS2021", "PS2021ciselniky20210824", "cpp.xlsx"))
        cns <- read_cns(here("data", "PS2021", "PS2021ciselniky20210824", "cns.xlsx"))
        read_candidates(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrk.xlsx"), 
                        psp_parties, cpp, cns) %>%
            mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
                   MANDAT = ifelse(MANDAT == "A", 1, 0))
    }), 
    
    ## Matching -------------------------------------------
    
    tar_target(psp_96_98, match_psp(psp_1996, psp_1998) %>% 
                   rename_cols(., "psp_1996", "psp_1998") %>%
                   append_missing(psp_1998, "row_id", ., .$psp_1998)
    ), 
    
    tar_target(psp_98_02, match_psp(psp_1998, psp_2002) %>%
                   rename_cols(., "psp_1998", "psp_2002")), 
    
    tar_target(psp_96_02, command = {
        missing_02 <- find_missing(psp_2002, "row_id", psp_98_02$psp_2002)
        missing_data_02 <- return_missing_data(psp_2002, "row_id", missing_02)
        noncons_96 <- return_nonconsecutive_data(psp_96_98, psp_1996, psp_1998, 
                                                 "row_id")
        match_psp(noncons_96, missing_data_02) %>% rename_cols(., "psp_1996", "psp_2002")
    }),
    
    tar_target(missing_final_2002, 
               find_missing(psp_2002, "row_id", c(psp_98_02$psp_2002, 
                                                  psp_96_02$psp_2002)) %>%
                   rename(psp_1998=from, psp_2002=to)
    ),
    
    tar_target(out_96_02, command = {
        dplyr::full_join(psp_96_98, psp_98_02, by = c("psp_1998")) %>%
            insert_nonconsecutive(., psp_96_02, "psp_1996", "psp_2002") %>%
            dplyr::bind_rows(., missing_final_2002)
    }),
    
    tar_target(psp_02_06, match_psp(psp_2002, psp_2006) %>%
                   rename_cols(., "psp_2002", "psp_2006")), 
    
    tar_target(psp_98_06, command = {
        missing_06 <- find_missing(psp_2006, "row_id", psp_02_06$psp_2006)
        missing_data_06 <- return_missing_data(psp_2006, "row_id", missing_06)
        noncons_98 <- return_nonconsecutive_data(psp_98_02, psp_1998, psp_2002, 
                                                 "row_id")
        match_psp(noncons_98, missing_data_06) %>% rename_cols(., "psp_1998", "psp_2006")
    }),
    
    tar_target(psp_96_06, command = {
        missing_06_2 <- find_missing(psp_2006, "row_id", 
                                     c(psp_02_06$psp_2006, 
                                       psp_98_06$psp_2006))
        missing_data_06 <- return_missing_data(psp_2006, "row_id", missing_06_2)
        noncons_96 <- return_nonconsecutive_data(out_96_02, psp_1996, psp_2002, 
                                                 "row_id")
        match_psp(noncons_96, missing_data_06) %>% rename_cols(., "psp_1996", "psp_2006")
    }),
    
    tar_target(missing_final_2006, command = {
        find_missing(psp_2006, "row_id", c(psp_96_06$psp_2006, 
                                           psp_98_06$psp_2006, 
                                           psp_02_06$psp_2006)) %>%
            rename(psp_2002=from, psp_2006=to)
    }),
    
    tar_target(out_96_06, command = {
        dplyr::full_join(out_96_02, psp_02_06, by = c("psp_2002")) %>% 
            insert_nonconsecutive(., psp_96_06, "psp_1996", "psp_2006")  %>%
            insert_nonconsecutive(., psp_98_06, "psp_1998", "psp_2006") %>%
            dplyr::bind_rows(., missing_final_2006)
    }),
    
    tar_target(psp_06_10, match_psp(psp_2006, psp_2010) %>%
                   rename_cols(., "psp_2006", "psp_2010")), 
    
    tar_target(psp_02_10, command = {
        missing_10 <- find_missing(psp_2010, "row_id", psp_06_10$psp_2010)
        missing_data_10 <- return_missing_data(psp_2010, "row_id", missing_10)
        noncons_02 <- return_nonconsecutive_data(psp_02_06, psp_2002, psp_2006, 
                                                 "row_id")
        match_psp(noncons_02, missing_data_10) %>%
            rename_cols(., "psp_2002", "psp_2010")
    }), 
    
    tar_target(psp_98_10, command = {
        missing_10_2 <- find_missing(psp_2010, "row_id", 
                                     c(psp_02_10$psp_2010, 
                                       psp_06_10$psp_2010))
        missing_data_10_2 <- return_missing_data(psp_2010, "row_id", missing_10_2)
        
        noncons_98 <- return_nonconsecutive_data(out_96_06, psp_1998, psp_2002, 
                                                 "row_id")
        match_psp(noncons_98, missing_data_10_2) %>%
            rename_cols(., "psp_1998", "psp_2010")
    }),
    
    tar_target(psp_96_10, command = {
        missing_10_3 <- find_missing(psp_2010, "row_id", 
                                     c(psp_02_10$psp_2010, 
                                       psp_06_10$psp_2010, 
                                       psp_98_10$psp_2010))
        missing_data_10_3 <- return_missing_data(psp_2010, "row_id", missing_10_3)
        noncons_96 <- return_nonconsecutive_data(out_96_06, psp_1996, psp_1998, 
                                                 "row_id")
        match_psp(noncons_96, missing_data_10_3) %>%
            rename_cols(., "psp_1996", "psp_2010")
    }), 
    
    tar_target(missing_final_2010, find_missing(psp_2010, "row_id", 
                                                c(psp_96_10$psp_2010, 
                                                  psp_98_10$psp_2010, 
                                                  psp_02_10$psp_2010, 
                                                  psp_06_10$psp_2010)) %>%
                   rename(psp_2006=from, psp_2010=to)), 
    
    tar_target(out_96_10, dplyr::full_join(out_96_06, psp_06_10, by = c("psp_2006")) %>% 
                   insert_nonconsecutive(., psp_96_10, "psp_1996", "psp_2010")  %>%
                   insert_nonconsecutive(., psp_98_10, "psp_1998", "psp_2010") %>%
                   insert_nonconsecutive(., psp_02_10, "psp_2002", "psp_2010") %>%
                   dplyr::bind_rows(., missing_final_2010)),
    
    tar_target(psp_10_13, match_psp(psp_2010, psp_2013) %>% rename_cols(., "psp_2010", "psp_2013")), 
    
    tar_target(psp_06_13, command = {
        missing_13 <- find_missing(psp_2013, "row_id", psp_10_13$psp_2013)
        missing_data_13 <- return_missing_data(psp_2013, "row_id", missing_13)
        noncons_06 <- return_nonconsecutive_data(psp_06_10, psp_2006, psp_2010, 
                                                 "row_id")
        match_psp(noncons_06, missing_data_13) %>% rename_cols(., "psp_2006", "psp_2013")
    }),
    
    tar_target(psp_02_13, command = {
        missing_13_2 <- find_missing(psp_2013, "row_id", 
                                     c(psp_10_13$psp_2013, 
                                       psp_06_13$psp_2013))
        missing_data_13_2 <- return_missing_data(psp_2013, "row_id", missing_13_2)
        noncons_02 <- return_nonconsecutive_data(out_96_10, psp_2002, psp_2006, 
                                                 "row_id")
        match_psp(noncons_02, missing_data_13_2) %>% rename_cols(., "psp_2002", "psp_2013")
    }),
    
    tar_target(psp_98_13, command = {
        missing_13_3 <- find_missing(psp_2013, "row_id", 
                                     c(psp_10_13$psp_2013, 
                                       psp_06_13$psp_2013, 
                                       psp_02_13$psp_2013))
        missing_data_13_3 <- return_missing_data(psp_2013, "row_id", missing_13_3)
        noncons_98 <- return_nonconsecutive_data(out_96_10, psp_1998, psp_2002, 
                                                 "row_id")
        match_psp(noncons_98, missing_data_13_3) %>% rename_cols(., "psp_1998", "psp_2013")
    }),
    
    tar_target(psp_96_13, command = {
        missing_13_4 <- find_missing(psp_2013, "row_id", 
                                     c(psp_10_13$psp_2013, 
                                       psp_06_13$psp_2013, 
                                       psp_02_13$psp_2013, 
                                       psp_98_13$psp_2013))
        missing_data_13_4 <- return_missing_data(psp_2013, "row_id", missing_13_4)
        noncons_96 <- return_nonconsecutive_data(out_96_10, psp_1996, psp_1998, 
                                                 "row_id")
        match_psp(noncons_96, missing_data_13_4) %>% rename_cols(., "psp_1996", "psp_2013")
    }),
    
    tar_target(missing_final_2013, find_missing(psp_2013, "row_id", 
                                                c(psp_96_13$psp_2013, 
                                                  psp_98_13$psp_2013, 
                                                  psp_02_13$psp_2013, 
                                                  psp_06_13$psp_2013, 
                                                  psp_10_13$psp_2013)) %>%
               rename(psp_2010=from, psp_2013=to)
    ),
    
    tar_target(out_96_13, command = {
        full_join(out_96_10, psp_10_13, by = c("psp_2010")) %>%
            insert_nonconsecutive(., psp_06_13, "psp_2006", "psp_2013") %>%
            insert_nonconsecutive(., psp_02_13, "psp_2002", "psp_2013") %>%
            insert_nonconsecutive(., psp_98_13, "psp_1998", "psp_2013") %>%
            insert_nonconsecutive(., psp_96_13, "psp_1996", "psp_2013") %>%
            bind_rows(., missing_final_2013)
    }),
    
    tar_target(psp_13_17, match_psp(psp_2013, psp_2017) %>% rename_cols(., "psp_2013", "psp_2017")), 
    
    tar_target(psp_10_17, command = {
        missing_17 <- find_missing(psp_2017, "row_id", psp_13_17$psp_2017)
        missing_data_17 <- return_missing_data(psp_2017, "row_id", missing_17)
        noncons_10 <- return_nonconsecutive_data(psp_10_13, psp_2010, psp_2013, 
                                                 "row_id")
        match_psp(noncons_10, missing_data_17) %>% rename_cols(., "psp_2010", "psp_2017")
    }),
    
    tar_target(psp_06_17, command = {
        missing_17_2 <- find_missing(psp_2017, "row_id", 
                                     c(psp_10_17$psp_2017, 
                                       psp_13_17$psp_2017))
        missing_data_17_2 <- return_missing_data(psp_2017, "row_id", missing_17_2)
        noncons_06 <- return_nonconsecutive_data(out_96_13, psp_2006, psp_2010, 
                                                 "row_id")
        match_psp(noncons_06, missing_data_17_2) %>% rename_cols(., "psp_2006", "psp_2017")
    }),
    
    tar_target(psp_02_17, command = {
        missing_17_3 <- find_missing(psp_2017, "row_id", 
                                     c(psp_13_17$psp_2017, 
                                       psp_10_17$psp_2017, 
                                       psp_06_17$psp_2017))
        missing_data_17_3 <- return_missing_data(psp_2017, "row_id", missing_17_3)
        noncons_02 <- return_nonconsecutive_data(out_96_13, psp_2002, psp_2006, 
                                                 "row_id")
        match_psp(noncons_02, missing_data_17_3) %>% rename_cols(., "psp_2002", "psp_2017")
    }),
    
    tar_target(psp_98_17, command = {
        missing_17_4 <- find_missing(psp_2017, "row_id", 
                                     c(psp_13_17$psp_2017, 
                                       psp_10_17$psp_2017, 
                                       psp_06_17$psp_2017, 
                                       psp_02_17$psp_2017))
        missing_data_17_4 <- return_missing_data(psp_2017, "row_id", missing_17_4)
        noncons_98 <- return_nonconsecutive_data(out_96_13, psp_1998, psp_2002, 
                                                 "row_id")
        match_psp(noncons_98, missing_data_17_4) %>% rename_cols(., "psp_1998", "psp_2017")
    }),
    
    tar_target(psp_96_17, command = {
        missing_17_5 <- find_missing(psp_2017, "row_id", 
                                     c(psp_13_17$psp_2017, 
                                       psp_10_17$psp_2017, 
                                       psp_06_17$psp_2017, 
                                       psp_02_17$psp_2017, 
                                       psp_98_17$psp_2017))
        missing_data_17_5 <- return_missing_data(psp_2017, "row_id", missing_17_5)
        noncons_96 <- return_nonconsecutive_data(out_96_13, psp_1996, psp_1998, 
                                                 "row_id")
        match_psp(noncons_96, missing_data_17_5) %>% rename_cols(., "psp_1996","psp_2017")
    }), 
    
    tar_target(missing_final_2017, find_missing(psp_2017, "row_id", 
                                                c(psp_96_17$psp_2017, 
                                                  psp_98_17$psp_2017, 
                                                  psp_02_17$psp_2017, 
                                                  psp_06_17$psp_2017, 
                                                  psp_10_17$psp_2017, 
                                                  psp_13_17$psp_2017)) %>%
                   rename(psp_2013=from, psp_2017=to)),
    
    tar_target(out_96_17, full_join(out_96_13, psp_13_17, by = c("psp_2013")) %>% 
                   insert_nonconsecutive(., psp_10_17, "psp_2010", "psp_2017")  %>%
                   insert_nonconsecutive(., psp_06_17, "psp_2006", "psp_2017") %>%
                   insert_nonconsecutive(., psp_02_17, "psp_2002", "psp_2017") %>%
                   insert_nonconsecutive(., psp_98_17, "psp_1998", "psp_2017") %>%
                   insert_nonconsecutive(., psp_96_17, "psp_1996", "psp_2017") %>%
                   bind_rows(., missing_final_2017)
    ),
    
    tar_target(psp_17_21, match_psp(psp_2017, psp_2021) %>% rename_cols(., "psp_2017", "psp_2021")),
    
    tar_target(psp_13_21, command = {
        missing_21 <- find_missing(psp_2021, "row_id", psp_17_21$psp_2021)
        missing_data_21 <- return_missing_data(psp_2021, "row_id", missing_21)
        noncons_13 <- return_nonconsecutive_data(psp_13_17, psp_2013, psp_2017, 
                                                 "row_id")
        match_psp(noncons_13, missing_data_21) %>% rename_cols(., "psp_2010", "psp_2021")
    }),
    
    tar_target(psp_10_21, command = {
        missing_21 <- find_missing(psp_2021, "row_id", c(psp_17_21$psp_2021, 
                                                         psp_13_21$psp_2021))
        missing_data_21 <- return_missing_data(psp_2021, "row_id", missing_21)
        noncons_10 <- return_nonconsecutive_data(out_96_17, psp_2010, psp_2013, 
                                                 "row_id")
        match_psp(noncons_10, missing_data_21) %>% rename_cols(., "psp_2010", "psp_2021")
    }),
    
    tar_target(psp_06_21, command = {
        missing_21 <- find_missing(psp_2021, "row_id", c(psp_17_21$psp_2021, 
                                                         psp_13_21$psp_2021, 
                                                         psp_10_21$psp_2021))
        missing_data_21 <- return_missing_data(psp_2021, "row_id", missing_21)
        noncons_06 <- return_nonconsecutive_data(out_96_17, psp_2006, psp_2010, 
                                                 "row_id")
        match_psp(noncons_06, missing_data_21) %>% rename_cols(., "psp_2006", "psp_2021")
    }),
    
    tar_target(psp_02_21, command = {
        missing_21 <- find_missing(psp_2021, "row_id", c(psp_17_21$psp_2021, 
                                                         psp_13_21$psp_2021, 
                                                         psp_10_21$psp_2021, 
                                                         psp_06_21$psp_2021))
        missing_data_21 <- return_missing_data(psp_2021, "row_id", missing_21)
        noncons_02 <- return_nonconsecutive_data(out_96_17, psp_2002, psp_2006, 
                                                 "row_id")
        match_psp(noncons_02, missing_data_21) %>% rename_cols(., "psp_2002", "psp_2021")
    }),
    
    tar_target(psp_98_21, command = {
        missing_21 <- find_missing(psp_2021, "row_id", c(psp_17_21$psp_2021, 
                                                         psp_13_21$psp_2021, 
                                                         psp_10_21$psp_2021, 
                                                         psp_06_21$psp_2021, 
                                                         psp_02_21$psp_2021))
        missing_data_21 <- return_missing_data(psp_2021, "row_id", missing_21)
        noncons_98 <- return_nonconsecutive_data(out_96_17, psp_1998, psp_2002, 
                                                 "row_id")
        match_psp(noncons_98, missing_data_21) %>% rename_cols(., "psp_1998", "psp_2021")
    }),
    
    tar_target(psp_96_21, command = {
        missing_21 <- find_missing(psp_2021, "row_id", c(psp_17_21$psp_2021, 
                                                         psp_13_21$psp_2021, 
                                                         psp_10_21$psp_2021, 
                                                         psp_06_21$psp_2021, 
                                                         psp_02_21$psp_2021, 
                                                         psp_98_21$psp_2021))
        missing_data_21 <- return_missing_data(psp_2021, "row_id", missing_21)
        noncons_96 <- return_nonconsecutive_data(out_96_17, psp_1996, psp_1998, 
                                                 "row_id")
        match_psp(noncons_96, missing_data_21) %>% rename_cols(., "psp_1996", "psp_2021")
    }),
    
    tar_target(missing_final_2021, find_missing(psp_2021, "row_id", 
                                                c(psp_17_21$psp_2021, 
                                                  psp_13_21$psp_2021, 
                                                  psp_10_21$psp_2021, 
                                                  psp_06_21$psp_2021, 
                                                  psp_02_21$psp_2021, 
                                                  psp_98_21$psp_2021, 
                                                  psp_96_21$psp_2021)) %>%
                   rename(psp_2017=from, psp_2021=to)),
    
    tar_target(out_96_21, full_join(out_96_17, psp_17_21, by = c("psp_2017")) %>% 
                   insert_nonconsecutive(., psp_13_21, "psp_2013", "psp_2021")  %>%
                   insert_nonconsecutive(., psp_10_21, "psp_2010", "psp_2021")  %>%
                   insert_nonconsecutive(., psp_06_21, "psp_2006", "psp_2021") %>%
                   insert_nonconsecutive(., psp_02_21, "psp_2002", "psp_2021") %>%
                   insert_nonconsecutive(., psp_98_21, "psp_1998", "psp_2021") %>%
                   insert_nonconsecutive(., psp_96_21, "psp_1996", "psp_2021") %>%
                   bind_rows(., missing_final_2021)
    ),
    
    tar_target(psp_panel, command = {
        all_data <- bind_rows(
            psp_1996 %>% mutate(data = "psp_1996"), 
            psp_1998 %>% mutate(data = "psp_1998"), 
            psp_2002 %>% mutate(data = "psp_2002"), 
            psp_2006 %>% mutate(data = "psp_2006"), 
            psp_2010 %>% mutate(data = "psp_2010"), 
            psp_2013 %>% mutate(data = "psp_2013"), 
            psp_2017 %>% mutate(data = "psp_2017"), 
            psp_2021 %>% mutate(data = "psp_2021"), 
        )
        
        create_panel(out_96_21, "row_id", all_data)
    }),
    
    #######################################################
    # Regional elections ----------------------------------
    #######################################################
    
    # TODO: 2000
    # TODO: 2004
    
    tar_target(reg_2008, command = {
        parties <- read_parties(here("data", "KZ2008", "kz2008_data_dbf", "KZRKL.xlsx"), 
                                clean_dbf_excel_colnames) %>% 
            unique
        cpp <- read_cpp(here("data", "KZ2008", "ciselnikyKZ2008", "CPP.xlsx"), 
                        clean_dbf_excel_colnames)
        cns <- read_cns(here("data", "KZ2008", "ciselnikyKZ2008", "CNS.xlsx"), 
                        clean_dbf_excel_colnames)
        read_candidates(here("data", "KZ2008", "kz2008_data_dbf", "KZRK.xlsx"), 
                        parties, cpp, cns, clean_dbf_excel_colnames)
    }),
    
    tar_target(reg_2012, command = {
        parties <- read_parties(here("data", "KZ2012", "kz2012_data_dbf", "KZRKL.xlsx"), 
                                clean_dbf_excel_colnames) %>% 
            unique
        cpp <- read_cpp(here("data", "KZ2012", "ciselniky20121010", "CPP.xlsx"), 
                        clean_dbf_excel_colnames)
        cns <- read_cns(here("data", "KZ2012", "ciselniky20121010", "CNS.xlsx"), 
                        clean_dbf_excel_colnames)
        read_candidates(here("data", "KZ2012", "kz2012_data_dbf", "KZRK.xlsx"), 
                        parties, cpp, cns, clean_dbf_excel_colnames)
    }),
    
    tar_target(reg_2016, command = {
        parties <- read_parties(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrkl.xlsx")) %>% 
             unique
        cpp <- read_cpp(here("data", "KZ2016", "KZ2016ciselniky20161007", "cpp.xlsx"))
        cns <- read_cns(here("data", "KZ2016", "KZ2016ciselniky20161007", "cns.xlsx"))
        read_candidates(here("data", "KZ2016", "KZ2016reg20161008_xlsx", "kzrk.xlsx"), 
                        parties, cpp, cns) %>%
            mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
                   MANDAT = ifelse(MANDAT == "A", 1, 0))
    }),
    
    tar_target(reg_2020, command = {
        parties <- read_parties(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
            unique
        cpp <- read_cpp(here("data", "KZ2020", "KZ2020ciselniky20201002", "cpp.xlsx"))
        cns <- read_cns(here("data", "KZ2020", "KZ2020ciselniky20201002", "cns.xlsx"))
        read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
                        parties, cpp, cns) %>%
            mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
                   MANDAT = ifelse(MANDAT == "A", 1, 0))
    }),
    
    ## Matching -------------------------------------------
    
    tar_target(reg_08_12, match_reg(reg_2008, reg_2012) %>% rename_cols(., "reg_2008", "reg_2012")), 
    
    tar_target(reg_12_16, match_reg(reg_2012, reg_2016) %>% rename_cols(., "reg_2012", "reg_2016")), 
    
    tar_target(reg_08_16, command = {
        missing_16 <- find_missing(reg_2016, "row_id", reg_12_16$reg_2016)
        missing_data_16 <- return_missing_data(reg_2016, "row_id", missing_16)
        noncons_08 <- return_nonconsecutive_data(reg_08_12, reg_2008, reg_2012, 
                                                 "row_id")
        match_reg(noncons_08, missing_data_16) %>% rename_cols(., "reg_2008", "reg_2016")
    }), 
    
    tar_target(missing_reg_2016, find_missing(reg_2016, "row_id", c(reg_12_16$reg_2016, 
                                                                    reg_08_16$reg_2016)) %>%
                   rename(reg_2012=from, reg_2016=to)),
    
    tar_target(out_08_16, full_join(reg_08_12, reg_12_16, by = "reg_2012") %>%
                   insert_nonconsecutive(., reg_08_16, "reg_2008", "reg_2016") %>%
                   bind_rows(missing_reg_2016)), 
    
    tar_target(reg_16_20, match_reg(reg_2016, reg_2020) %>% rename_cols(., "reg_2016", "reg_2020")), 
    
    tar_target(reg_12_20, command = {
        missing_20 <- find_missing(reg_2020, "row_id", reg_16_20$reg_2020)
        missing_data_20 <- return_missing_data(reg_2020, "row_id", missing_20)
        noncons_12 <- return_nonconsecutive_data(out_08_16, reg_2012, reg_2016, "row_id")
        
        match_reg(noncons_12, missing_data_20) %>% rename_cols(., "reg_2012", "reg_2020")
    }),
    
    tar_target(reg_08_20, command = {
        missing_20 <- find_missing(reg_2020, "row_id", c(reg_16_20$reg_2020, 
                                                         reg_12_20$reg_2020))
        missing_data_20 <- return_missing_data(reg_2020, "row_id", missing_20)
        noncons_08 <- return_nonconsecutive_data(out_08_16, reg_2008, reg_2012, "row_id")
        
        match_reg(noncons_08, missing_data_20) %>% rename_cols(., "reg_2008", "reg_2020")
    }),
    
    tar_target(missing_reg_2020, find_missing(reg_2020, "row_id", c(reg_16_20$reg_2020, 
                                                                    reg_12_20$reg_2020, 
                                                                    reg_08_20$reg_2020)) %>%
                   rename(reg_2016=from, reg_2020=to)),
    
    tar_target(out_08_20, full_join(out_08_16, reg_16_20, "reg_2016") %>%
                   insert_nonconsecutive(., reg_12_20, "reg_2012", "reg_2020") %>%
                   insert_nonconsecutive(., reg_08_20, "reg_2008", "reg_2020") %>%
                   bind_rows(., missing_reg_2020)),
    
    tar_target(reg_panel, command = {
        all_data <- bind_rows(
            reg_2008 %>% mutate(data = "reg_2008"), 
            reg_2012 %>% mutate(data = "reg_2012"), 
            reg_2016 %>% mutate(data = "reg_2016"), 
            reg_2020 %>% mutate(data = "reg_2020")
        )
        
        create_panel(out_08_20, "row_id", all_data)
    }),
    
    #######################################################
    # Municipal elections ---------------------------------
    #######################################################
    
    tar_target(mun_1994, command = {
        parties <- read_excel(here("data", "KV1994", "CVSKV94.xlsx"))
        cpp <- read_cpp(here("data", "KV1994", "CPPKV94.xlsx"), function(x) 
           {x %>% rename(ZKRATKAP8 = ZKRAT)})
        cns <- read_cns(here("data", "KV1994", "CNSKV94.xlsx"), function(x) 
           {x %>% rename(ZKRATKAN8 = ZKRAT)})
        candidates_path <- here("data", "KV1994", "KV94_RK.xlsx")
        year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
        read_excel(candidates_path) %>%
            mutate(JMENO = ifelse(JMENO == "Franišek", "František", JMENO)) %>%
            left_join(., parties, by = c("VSTRANA")) %>%
            left_join(., cpp, by = "PSTRANA") %>%
            left_join(., cns, by = "NSTRANA") %>%
            mutate(row_id = row_number(), 
                   ROK_NAROZENI = year - VEK) %>%
            extract_titles_from_last_name()
    }),
    
    tar_target(mun_1998, command = {
        parties <- read_excel(here("data", "KV1998", "CVSKV98.xlsx"))
        cpp <- read_cpp(here("data", "KV1998", "CPPKV98.xlsx"), function(x) 
            {x %>% rename(ZKRATKAP8 = ZKRATKAP)})
        cns <- read_cns(here("data", "KV1998", "CNSKV98.xlsx"))
        candidates_path <- here::here("data", "KV1998", "KV98_RK.xlsx")
        year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
        read_excel(candidates_path) %>%
            left_join(., parties, by = c("VSTRANA")) %>%
            left_join(., cpp, by = "PSTRANA") %>%
            left_join(., cns, by = "NSTRANA") %>%
            rename(TITULY = TITUL) %>%
            mutate(row_id = row_number(), 
                   ROK_NAROZENI = year - VEK, 
                   TITULY_KATEGORIE = categorize_titles(TITULY))
    }),
    
    tar_target(mun_2002, command = {
        # parties <- read_excel(here("data", "KV2002", "CVSKV02.xlsx"))
        cpp <- read_cpp(here("data", "KV2002", "CPPKV02.xlsx"))
        cns <- read_cns(here("data", "KV2002", "CNSKV02.xlsx"))
        candidates_path <- here::here("data", "KV2002", "KV02_RK.xlsx")
        year <- as.numeric(stringr::str_extract(candidates_path, "[0-9]{4}"))
        read_excel(candidates_path) %>%
            # left_join(., parties, by = c("VSTRANA")) %>%
            left_join(., cpp, by = "PSTRANA") %>%
            left_join(., cns, by = "NSTRANA") %>%
            merge_and_recode_titles %>%
            mutate(row_id = row_number(), 
                   ROK_NAROZENI = year - VEK)
    }),
    
    tar_target(mun_2006, command = {
        parties <- read_municipal_parties(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvros.xlsx"))
        cpp <- read_cpp_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cpp.xml"))
        cns <- read_cns_xml(here("data", "KV2006", "KV2006ciselniky20140909", "cns.xml"))
        read_municipal_candidates(here("data", "KV2006", "KV2006reg20140909_xlsx", "kvrk.xlsx"), 
                                  parties, cpp, cns)
    }),
    
    tar_target(mun_2010, command = {
        parties <- read_municipal_parties(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvros.xlsx"))
        cpp <- read_cpp_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cpp.xml"))
        cns <- read_cns_xml(here("data", "KV2010", "KV2010ciselniky20140903", "cns.xml"))
        read_municipal_candidates(here("data", "KV2010", "KV2010reg20140903_xlsx", "kvrk.xlsx"), 
                                  parties, cpp, cns)
    }),
    
    tar_target(mun_2014, command = {
        parties <- read_municipal_parties(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvros.xlsx"))
        cpp <- read_cpp_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cpp.xml"))
        cns <- read_cns_xml(here("data", "KV2014", "KV2014ciselniky20141008", "cns.xml"))
        read_municipal_candidates(here("data", "KV2014", "KV2014reg20141014_xlsx", "kvrk.xlsx"), 
                                  parties, cpp, cns)
    }),
    
    tar_target(mun_2018, command = {
        parties <- read_municipal_parties(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvros.xlsx"))
        cpp <- read_cpp(here("data", "KV2018", "KV2018ciselniky20181004", "cpp.xlsx"))
        cns <- read_cns(here("data", "KV2018", "KV2018ciselniky20181004", "cns.xlsx"))
        read_municipal_candidates(here("data", "KV2018", "KV2018reg20181008_xlsx", "kvrk.xlsx"), 
                        parties, cpp, cns) %>%
            mutate(PLATNOST = ifelse(PLATNOST == "A", 0, 1), 
                   MANDAT = ifelse(MANDAT == "N", 0, 1))
    }),
    
    ## City districts -------------------------------------
    
    tar_target(district_municipalities_map, read_csv(here("data", "mcast_obec.csv"), 
                                                     locale = locale(encoding = "WINDOWS-1250")) %>%
                   select(CITY_DISTRICT = CHODNOTA1, 
                          MUNICIPALITY = CHODNOTA2)),
    
    tar_target(mc_1994, filter_city_districts(mun_1994, district_municipalities_map)),
    tar_target(mc_1998, filter_city_districts(mun_1998, district_municipalities_map)),
    tar_target(mc_2002, filter_city_districts(mun_2002, district_municipalities_map)),
    tar_target(mc_2006, filter_city_districts(mun_2006, district_municipalities_map)),
    tar_target(mc_2010, filter_city_districts(mun_2010, district_municipalities_map)),
    tar_target(mc_2014, filter_city_districts(mun_2014, district_municipalities_map)),
    tar_target(mc_2018, filter_city_districts(mun_2018, district_municipalities_map)),
    
    ### Matching ------------------------------------------
    
    tar_target(mc_94_98, match_mc(mc_1994, mc_1998) %>% rename_cols(., "mc_1994", "mc_1998")),
    
    tar_target(mc_98_02, match_mc(mc_1998, mc_2002) %>% rename_cols(., "mc_1998", "mc_2002")),
    
    tar_target(mc_94_02, command = {
        missing_data_02 <- find_missing_data(mc_2002, mc_98_02$mc_2002, "row_id")
        noncons_94 <- return_nonconsecutive_data(mc_94_98, mc_1994, mc_1998, "row_id")
        match_mc(noncons_94, missing_data_02) %>% rename_cols(., "mc_1994", "mc_2002")
    }),
    
    tar_target(missing_mc_02, find_missing(mc_2002, "row_id", 
                                           c(mc_98_02$mc_2002, mc_94_02$mc_2002)) %>% 
                   rename(mc_1998=from, mc_2002=to)),
    
    tar_target(out_mc_94_02, full_join(mc_94_98, mc_98_02, "mc_1998") %>%
                   insert_nonconsecutive(., mc_94_02, "mc_1994", "mc_2002") %>%
                   bind_rows(missing_mc_02)),
    
    tar_target(mc_02_06, match_mc(mc_2002, mc_2006) %>% rename_cols(., "mc_2002", "mc_2006")),
    
    tar_target(mc_98_06, command = {
        missing_data_06 <- find_missing_data(mc_2006, mc_02_06$mc_2006, "row_id")
        noncons_98 <- return_nonconsecutive_data(out_mc_94_02, mc_1998, mc_2002, "row_id")
        match_mc(noncons_98, missing_data_06) %>% rename_cols(., "mc_1998", "mc_2006")
    }),
    
    tar_target(mc_94_06, command = {
        missing_data_06 <- find_missing_data(mc_2006, c(mc_02_06$mc_2006, 
                                                        mc_98_06$mc_2006), "row_id")
        noncons_94 <- return_nonconsecutive_data(out_mc_94_02, mc_1994, mc_1998, "row_id")
        match_mc(noncons_94, missing_data_06) %>% rename_cols(., "mc_1994", "mc_2006")
    }),
    
    tar_target(missing_mc_06, find_missing(mc_2006, "row_id", 
                                           c(mc_02_06$mc_2006, 
                                             mc_98_06$mc_2006,
                                             mc_94_06$mc_2006)) %>% 
                   rename(mc_2002=from, mc_2006=to)),
    
    tar_target(out_mc_94_06, full_join(out_mc_94_02, mc_02_06, "mc_2002") %>%
                   insert_nonconsecutive(., mc_98_06, "mc_1998", "mc_2006") %>%
                   insert_nonconsecutive(., mc_94_06, "mc_1994", "mc_2006") %>%
                   bind_rows(missing_mc_06)),
    
    tar_target(mc_06_10, match_mc(mc_2006, mc_2010) %>% rename_cols(., "mc_2006", "mc_2010")),
    
    tar_target(mc_02_10, command = {
        missing_data_10 <- find_missing_data(mc_2010, mc_06_10$mc_2010, "row_id")
        noncons_02 <- return_nonconsecutive_data(out_mc_94_06, mc_2002, mc_2006, "row_id")
        match_mc(noncons_02, missing_data_10) %>% rename_cols(., "mc_2002", "mc_2010")
    }),
    
    tar_target(mc_98_10, command = {
        missing_data_10 <- find_missing_data(mc_2010, c(mc_06_10$mc_2010, 
                                                        mc_02_10$mc_2010), "row_id")
        noncons_98 <- return_nonconsecutive_data(out_mc_94_06, mc_1998, mc_2002, "row_id")
        match_mc(noncons_98, missing_data_10) %>% rename_cols(., "mc_1998", "mc_2010")
    }),
    
    tar_target(mc_94_10, command = {
        missing_data_10 <- find_missing_data(mc_2010, c(mc_06_10$mc_2010, 
                                                        mc_02_10$mc_2010, 
                                                        mc_98_10$mc_2010), "row_id")
        noncons_94 <- return_nonconsecutive_data(out_mc_94_06, mc_1994, mc_1998, "row_id")
        match_mc(noncons_94, missing_data_10) %>% rename_cols(., "mc_1994", "mc_2010")
    }),
    
    tar_target(missing_mc_10, find_missing(mc_2010, "row_id", 
                                           c(mc_06_10$mc_2010,
                                             mc_02_10$mc_2010, 
                                             mc_98_10$mc_2010,
                                             mc_94_10$mc_2010)) %>% 
                   rename(mc_2006=from, mc_2010=to)),
    
    tar_target(out_mc_94_10, full_join(out_mc_94_06, mc_06_10, "mc_2006") %>%
                   insert_nonconsecutive(., mc_02_10, "mc_2002", "mc_2010") %>%
                   insert_nonconsecutive(., mc_98_10, "mc_1998", "mc_2010") %>%
                   insert_nonconsecutive(., mc_94_10, "mc_1994", "mc_2010") %>%
                   bind_rows(missing_mc_10)),
    
    tar_target(mc_10_14, match_mc(mc_2010, mc_2014) %>% rename_cols(., "mc_2010", "mc_2014")),

    tar_target(mc_06_14, command = {
        missing_data_14 <- find_missing_data(mc_2014, mc_10_14$mc_2014, "row_id")
        noncons_06 <- return_nonconsecutive_data(mc_06_10, mc_2006, mc_2010,
                                                 "row_id")
        match_mc(noncons_06, missing_data_14) %>% rename_cols(., "mc_2006", "mc_2014")
    }),
    
    tar_target(mc_02_14, command = {
        missing_data_14 <- find_missing_data(mc_2014, c(mc_10_14$mc_2014, 
                                                        mc_06_14$mc_2014), "row_id")
        noncons_02 <- return_nonconsecutive_data(out_mc_94_10, mc_2002, mc_2006,
                                                 "row_id")
        match_mc(noncons_02, missing_data_14) %>% rename_cols(., "mc_2002", "mc_2014")
    }),

    tar_target(mc_98_14, command = {
        missing_data_14 <- find_missing_data(mc_2014, c(mc_10_14$mc_2014, 
                                                        mc_06_14$mc_2014, 
                                                        mc_02_14$mc_2014), "row_id")
        noncons_98 <- return_nonconsecutive_data(out_mc_94_10, mc_1998, mc_2002,
                                                 "row_id")
        match_mc(noncons_98, missing_data_14) %>% rename_cols(., "mc_1998", "mc_2014")
    }),
    
    tar_target(mc_94_14, command = {
        missing_data_14 <- find_missing_data(mc_2014, c(mc_10_14$mc_2014, 
                                                        mc_06_14$mc_2014, 
                                                        mc_02_14$mc_2014), "row_id")
        noncons_94 <- return_nonconsecutive_data(out_mc_94_10, mc_1994, mc_1998,
                                                 "row_id")
        match_mc(noncons_94, missing_data_14) %>% rename_cols(., "mc_1994", "mc_2014")
    }),
    
    tar_target(missing_mc_14, find_missing(mc_2014, "row_id", c(mc_10_14$mc_2014,
                                                                mc_06_14$mc_2014, 
                                                                mc_02_14$mc_2014, 
                                                                mc_98_14$mc_2014, 
                                                                mc_94_14$mc_2014)) %>%
                   rename(mc_2010=from, mc_2014=to)),
    
    tar_target(out_mc_94_14, full_join(out_mc_94_10, mc_10_14, "mc_2010") %>%
                   insert_nonconsecutive(., mc_06_14, "mc_2006", "mc_2014") %>%
                   insert_nonconsecutive(., mc_02_14, "mc_2002", "mc_2014") %>%
                   insert_nonconsecutive(., mc_98_14, "mc_1998", "mc_2014") %>%
                   insert_nonconsecutive(., mc_94_14, "mc_1994", "mc_2014") %>%
                   bind_rows(missing_mc_14)),
    
    tar_target(mc_14_18, match_mc(mc_2014, mc_2018) %>% rename_cols(., "mc_2014", "mc_2018")),

    tar_target(mc_10_18, command = {
        missing_data_18 <- find_missing_data(mc_2018, mc_14_18$mc_2018, "row_id")
        noncons_10 <- return_nonconsecutive_data(out_mc_94_14, mc_2010, mc_2014, "row_id")
        match_mc(noncons_10, missing_data_18) %>% rename_cols(., "mc_2010", "mc_2018")
    }),

    tar_target(mc_06_18, command = {
        missing_data_18 <- find_missing_data(mc_2018, c(mc_14_18$mc_2018, mc_10_18$mc_2018),
                                             "row_id")
        noncons_06 <- return_nonconsecutive_data(out_mc_94_14, mc_2006, mc_2010, "row_id")
        match_mc(noncons_06, missing_data_18) %>% rename_cols(., "mc_2006", "mc_2018")
    }),

    tar_target(mc_02_18, command = {
        missing_data_18 <- find_missing_data(mc_2018, c(mc_14_18$mc_2018, 
                                                        mc_10_18$mc_2018, 
                                                        mc_06_18$mc_2018),
                                             "row_id")
        noncons_02 <- return_nonconsecutive_data(out_mc_94_14, mc_2002, mc_2006, "row_id")
        match_mc(noncons_02, missing_data_18) %>% rename_cols(., "mc_2002", "mc_2018")
    }),
    
    tar_target(mc_98_18, command = {
        missing_data_18 <- find_missing_data(mc_2018, c(mc_14_18$mc_2018, 
                                                        mc_10_18$mc_2018, 
                                                        mc_06_18$mc_2018, 
                                                        mc_02_18$mc_2018),
                                             "row_id")
        noncons_98 <- return_nonconsecutive_data(out_mc_94_14, mc_1998, mc_2002, "row_id")
        match_mc(noncons_98, missing_data_18) %>% rename_cols(., "mc_1998", "mc_2018")
    }),
    
    tar_target(mc_94_18, command = {
        missing_data_18 <- find_missing_data(mc_2018, c(mc_14_18$mc_2018, 
                                                        mc_10_18$mc_2018, 
                                                        mc_06_18$mc_2018, 
                                                        mc_02_18$mc_2018, 
                                                        mc_98_18$mc_2018),
                                             "row_id")
        noncons_94 <- return_nonconsecutive_data(out_mc_94_14, mc_1994, mc_1998, "row_id")
        match_mc(noncons_94, missing_data_18) %>% rename_cols(., "mc_1994", "mc_2018")
    }),
    
    tar_target(missing_mc_18, find_missing(mc_2018, "row_id", c(mc_14_18$mc_2018,
                                                                mc_10_18$mc_2018,
                                                                mc_06_18$mc_2018, 
                                                                mc_02_18$mc_2018, 
                                                                mc_98_18$mc_2018, 
                                                                mc_94_18$mc_2018)) %>%
                   rename(mc_2014=from, mc_2018=to)),

    tar_target(out_mc_94_18, full_join(out_mc_94_14, mc_14_18, "mc_2014") %>%
                   insert_nonconsecutive(., mc_10_18, "mc_2014", "mc_2018") %>%
                   insert_nonconsecutive(., mc_06_18, "mc_2006", "mc_2018") %>%
                   insert_nonconsecutive(., mc_02_18, "mc_2002", "mc_2018") %>%
                   insert_nonconsecutive(., mc_98_18, "mc_1998", "mc_2018") %>%
                   insert_nonconsecutive(., mc_94_18, "mc_1994", "mc_2018") %>%
                   bind_rows(missing_mc_18)),
    
    tar_target(mc_panel, command = {
        all_data <- bind_rows(
            mc_1994 %>% mutate(data = "mc_1994"),
            mc_1998 %>% mutate(data = "mc_1998"),
            mc_2002 %>% mutate(data = "mc_2002"),
            mc_2006 %>% mutate(data = "mc_2006"),
            mc_2010 %>% mutate(data = "mc_2010"),
            mc_2014 %>% mutate(data = "mc_2014"),
            mc_2018 %>% mutate(data = "mc_2018")
        )

        create_panel(out_mc_94_18, "row_id", all_data)
    }),
    
    ## Municipalities -------------------------------------
    
    tar_target(m_1994, filter_municipalities(mun_1994, district_municipalities_map) %>%
                   mutate(PRIJMENI = gsub('"o', "ö", PRIJMENI) %>%
                              gsub('o"', "ö", .) %>%
                              gsub('u"', "ü", .)
                   )),
    tar_target(m_1998, filter_municipalities(mun_1998, district_municipalities_map)),
    tar_target(m_2002, filter_municipalities(mun_2002, district_municipalities_map)),
    tar_target(m_2006, filter_municipalities(mun_2006, district_municipalities_map)),
    tar_target(m_2010, filter_municipalities(mun_2010, district_municipalities_map)),
    tar_target(m_2014, filter_municipalities(mun_2014, district_municipalities_map)),
    tar_target(m_2018, filter_municipalities(mun_2018, district_municipalities_map)),
    
    ### Matching ------------------------------------
    
    tar_target(m_94_98, match_m(m_1994, m_1998) %>% rename_cols(., "m_1994", "m_1998")),
    
    tar_target(m_98_02, match_m(m_1998, m_2002) %>% rename_cols(., "m_1998", "m_2002")),
    
    tar_target(m_94_02, command = {
        missing_data_02 <- find_missing_data(m_2002, m_98_02$m_2002, "row_id")
        noncons_94 <- return_nonconsecutive_data(m_94_98, m_1994, m_1998, "row_id")
        match_m(noncons_94, missing_data_02) %>% rename_cols(., "m_1994", "m_2002")
    }),
    
    tar_target(missing_m_02, find_missing(m_2002, "row_id", 
                                           c(m_98_02$m_2002, m_94_02$m_2002)) %>% 
                   rename(m_1998=from, m_2002=to)),
    
    tar_target(out_m_94_02, full_join(m_94_98, m_98_02, "m_1998") %>%
                   insert_nonconsecutive(., m_94_02, "m_1994", "m_2002") %>%
                   bind_rows(missing_m_02)),
    
    tar_target(m_02_06, match_m(m_2002, m_2006) %>% rename_cols(., "m_2002", "m_2006")),
    
    tar_target(m_98_06, command = {
        missing_data_06 <- find_missing_data(m_2006, m_02_06$m_2006, "row_id")
        noncons_98 <- return_nonconsecutive_data(out_m_94_02, m_1998, m_2002, "row_id")
        match_m(noncons_98, missing_data_06) %>% rename_cols(., "m_1998", "m_2006")
    }),
    
    tar_target(m_94_06, command = {
        missing_data_06 <- find_missing_data(m_2006, c(m_02_06$m_2006, 
                                                        m_98_06$m_2006), "row_id")
        noncons_94 <- return_nonconsecutive_data(out_m_94_02, m_1994, m_1998, "row_id")
        match_m(noncons_94, missing_data_06) %>% rename_cols(., "m_1994", "m_2006")
    }),
    
    tar_target(missing_m_06, find_missing(m_2006, "row_id", 
                                           c(m_02_06$m_2006, 
                                             m_98_06$m_2006,
                                             m_94_06$m_2006)) %>% 
                   rename(m_2002=from, m_2006=to)),
    
    tar_target(out_m_94_06, full_join(out_m_94_02, m_02_06, "m_2002") %>%
                   insert_nonconsecutive(., m_98_06, "m_1998", "m_2006") %>%
                   insert_nonconsecutive(., m_94_06, "m_1994", "m_2006") %>%
                   bind_rows(missing_m_06)),
    
    tar_target(m_06_10, match_m(m_2006, m_2010) %>% rename_cols(., "m_2006", "m_2010")),
    
    tar_target(m_02_10, command = {
        missing_data_10 <- find_missing_data(m_2010, m_06_10$m_2010, "row_id")
        noncons_02 <- return_nonconsecutive_data(out_m_94_06, m_2002, m_2006, "row_id")
        match_m(noncons_02, missing_data_10) %>% rename_cols(., "m_2002", "m_2010")
    }),
    
    tar_target(m_98_10, command = {
        missing_data_10 <- find_missing_data(m_2010, c(m_06_10$m_2010, 
                                                        m_02_10$m_2010), "row_id")
        noncons_98 <- return_nonconsecutive_data(out_m_94_06, m_1998, m_2002, "row_id")
        match_m(noncons_98, missing_data_10) %>% rename_cols(., "m_1998", "m_2010")
    }),
    
    tar_target(m_94_10, command = {
        missing_data_10 <- find_missing_data(m_2010, c(m_06_10$m_2010, 
                                                        m_02_10$m_2010, 
                                                        m_98_10$m_2010), "row_id")
        noncons_94 <- return_nonconsecutive_data(out_m_94_06, m_1994, m_1998, "row_id")
        match_m(noncons_94, missing_data_10) %>% rename_cols(., "m_1994", "m_2010")
    }),
    
    tar_target(missing_m_10, find_missing(m_2010, "row_id", 
                                           c(m_06_10$m_2010,
                                             m_02_10$m_2010, 
                                             m_98_10$m_2010,
                                             m_94_10$m_2010)) %>% 
                   rename(m_2006=from, m_2010=to)),
    
    tar_target(out_m_94_10, full_join(out_m_94_06, m_06_10, "m_2006") %>%
                   insert_nonconsecutive(., m_02_10, "m_2002", "m_2010") %>%
                   insert_nonconsecutive(., m_98_10, "m_1998", "m_2010") %>%
                   insert_nonconsecutive(., m_94_10, "m_1994", "m_2010") %>%
                   bind_rows(missing_m_10)),
    
    tar_target(m_10_14, match_m(m_2010, m_2014) %>% rename_cols(., "m_2010", "m_2014")),
    
    tar_target(m_06_14, command = {
        missing_data_14 <- find_missing_data(m_2014, m_10_14$m_2014, "row_id")
        noncons_06 <- return_nonconsecutive_data(m_06_10, m_2006, m_2010,
                                                 "row_id")
        match_m(noncons_06, missing_data_14) %>% rename_cols(., "m_2006", "m_2014")
    }),
    
    tar_target(m_02_14, command = {
        missing_data_14 <- find_missing_data(m_2014, c(m_10_14$m_2014, 
                                                        m_06_14$m_2014), "row_id")
        noncons_02 <- return_nonconsecutive_data(out_m_94_10, m_2002, m_2006,
                                                 "row_id")
        match_m(noncons_02, missing_data_14) %>% rename_cols(., "m_2002", "m_2014")
    }),
    
    tar_target(m_98_14, command = {
        missing_data_14 <- find_missing_data(m_2014, c(m_10_14$m_2014, 
                                                        m_06_14$m_2014, 
                                                        m_02_14$m_2014), "row_id")
        noncons_98 <- return_nonconsecutive_data(out_m_94_10, m_1998, m_2002,
                                                 "row_id")
        match_m(noncons_98, missing_data_14) %>% rename_cols(., "m_1998", "m_2014")
    }),
    
    tar_target(m_94_14, command = {
        missing_data_14 <- find_missing_data(m_2014, c(m_10_14$m_2014, 
                                                        m_06_14$m_2014, 
                                                        m_02_14$m_2014), "row_id")
        noncons_94 <- return_nonconsecutive_data(out_m_94_10, m_1994, m_1998,
                                                 "row_id")
        match_m(noncons_94, missing_data_14) %>% rename_cols(., "m_1994", "m_2014")
    }),
    
    tar_target(missing_m_14, find_missing(m_2014, "row_id", c(m_10_14$m_2014,
                                                                m_06_14$m_2014, 
                                                                m_02_14$m_2014, 
                                                                m_98_14$m_2014, 
                                                                m_94_14$m_2014)) %>%
                   rename(m_2010=from, m_2014=to)),
    
    tar_target(out_m_94_14, full_join(out_m_94_10, m_10_14, "m_2010") %>%
                   insert_nonconsecutive(., m_06_14, "m_2006", "m_2014") %>%
                   insert_nonconsecutive(., m_02_14, "m_2002", "m_2014") %>%
                   insert_nonconsecutive(., m_98_14, "m_1998", "m_2014") %>%
                   insert_nonconsecutive(., m_94_14, "m_1994", "m_2014") %>%
                   bind_rows(missing_m_14)),
    
    tar_target(m_14_18, match_m(m_2014, m_2018) %>% rename_cols(., "m_2014", "m_2018")),
    
    tar_target(m_10_18, command = {
        missing_data_18 <- find_missing_data(m_2018, m_14_18$m_2018, "row_id")
        noncons_10 <- return_nonconsecutive_data(out_m_94_14, m_2010, m_2014, "row_id")
        match_m(noncons_10, missing_data_18) %>% rename_cols(., "m_2010", "m_2018")
    }),
    
    tar_target(m_06_18, command = {
        missing_data_18 <- find_missing_data(m_2018, c(m_14_18$m_2018, m_10_18$m_2018),
                                             "row_id")
        noncons_06 <- return_nonconsecutive_data(out_m_94_14, m_2006, m_2010, "row_id")
        match_m(noncons_06, missing_data_18) %>% rename_cols(., "m_2006", "m_2018")
    }),
    
    tar_target(m_02_18, command = {
        missing_data_18 <- find_missing_data(m_2018, c(m_14_18$m_2018, 
                                                        m_10_18$m_2018, 
                                                        m_06_18$m_2018),
                                             "row_id")
        noncons_02 <- return_nonconsecutive_data(out_m_94_14, m_2002, m_2006, "row_id")
        match_m(noncons_02, missing_data_18) %>% rename_cols(., "m_2002", "m_2018")
    }),
    
    tar_target(m_98_18, command = {
        missing_data_18 <- find_missing_data(m_2018, c(m_14_18$m_2018, 
                                                        m_10_18$m_2018, 
                                                        m_06_18$m_2018, 
                                                        m_02_18$m_2018),
                                             "row_id")
        noncons_98 <- return_nonconsecutive_data(out_m_94_14, m_1998, m_2002, "row_id")
        match_m(noncons_98, missing_data_18) %>% rename_cols(., "m_1998", "m_2018")
    }),
    
    tar_target(m_94_18, command = {
        missing_data_18 <- find_missing_data(m_2018, c(m_14_18$m_2018, 
                                                        m_10_18$m_2018, 
                                                        m_06_18$m_2018, 
                                                        m_02_18$m_2018, 
                                                        m_98_18$m_2018),
                                             "row_id")
        noncons_94 <- return_nonconsecutive_data(out_m_94_14, m_1994, m_1998, "row_id")
        match_m(noncons_94, missing_data_18) %>% rename_cols(., "m_1994", "m_2018")
    }),
    
    tar_target(missing_m_18, find_missing(m_2018, "row_id", c(m_14_18$m_2018,
                                                                m_10_18$m_2018,
                                                                m_06_18$m_2018, 
                                                                m_02_18$m_2018, 
                                                                m_98_18$m_2018, 
                                                                m_94_18$m_2018)) %>%
                   rename(m_2014=from, m_2018=to)),
    
    tar_target(out_m_94_18, full_join(out_m_94_14, m_14_18, "m_2014") %>%
                   insert_nonconsecutive(., m_10_18, "m_2014", "m_2018") %>%
                   insert_nonconsecutive(., m_06_18, "m_2006", "m_2018") %>%
                   insert_nonconsecutive(., m_02_18, "m_2002", "m_2018") %>%
                   insert_nonconsecutive(., m_98_18, "m_1998", "m_2018") %>%
                   insert_nonconsecutive(., m_94_18, "m_1994", "m_2018") %>%
                   bind_rows(missing_m_18)),
    
    tar_target(m_panel, command = {
        all_data <- bind_rows(
            m_1994 %>% mutate(data = "m_1994"),
            m_1998 %>% mutate(data = "m_1998"),
            m_2002 %>% mutate(data = "m_2002"),
            m_2006 %>% mutate(data = "m_2006"),
            m_2010 %>% mutate(data = "m_2010"),
            m_2014 %>% mutate(data = "m_2014"),
            m_2018 %>% mutate(data = "m_2018")
        )
        
        create_panel(out_m_94_18, "row_id", all_data)
    }),
    
    #######################################################
    # European Parliament ---------------------------------
    #######################################################
    tar_target(ep_2004, command = {
        parties <- read_parties_xml(here("data", "EP2004", "EP2004reg", "eprkl.xml"), 
                                    function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
        cpp <- read_cpp_xml(here("data", "EP2004", "EP2004ciselniky", "cpp.xml"))
        cns <- read_cns_xml(here("data", "EP2004", "EP2004ciselniky", "cns.xml"))
        read_candidates_xml(here("data", "EP2004", "EP2004reg", "eprk.xml"), 
                            parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)})
    }),
    
    tar_target(ep_2009, command = {
        parties <- read_parties_xml(here("data", "EP2009", "EP2009reg", "eprkl.xml"), 
                                    function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
        cpp <- read_cpp_xml(here("data", "EP2009", "EP2009ciselniky", "cpp.xml"))
        cns <- read_cns_xml(here("data", "EP2009", "EP2009ciselniky", "cns.xml"))
        read_candidates_xml(here("data", "EP2009", "EP2009reg", "eprk.xml"), 
                            parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)})
    }),
    
    tar_target(ep_2014, command = {
        parties <- read_parties_xml(here("data", "EP2014", "EP2014reg20140525", "eprkl.xml"), 
                                    function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
        cpp <- read_cpp_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cpp.xml"))
        cns <- read_cns_xml(here("data", "EP2014", "EP2014ciselniky20140425", "cns.xml"))
        read_candidates_xml(here("data", "EP2014", "EP2014reg20140525", "eprk.xml"), 
                            parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)})
    }),
    
    tar_target(ep_2019, command = {
        parties <- read_parties(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprkl.xlsx"), 
                                    function(x) x %>% select(KSTRANA = ESTRANA, ZKRATKAK8 = ZKRATKAE8, NAZEV_STRK = NAZEV_STRE))
        cpp <- read_cpp(here("data", "EP2019", "EP2019ciselniky20190513", "cpp.xlsx"))
        cns <- read_cns(here("data", "EP2019", "EP2019ciselniky20190513", "cns.xlsx"))
        read_candidates(here("data", "EP2019", "EP2019reg20190526_xlsx", "eprk.xlsx"), 
                            parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA) %>%
                                    mutate(MANDAT = ifelse(MANDAT == "A", 1, 0))})
    }),
    
    ## Matching -------------------------------------------
    tar_target(ep_04_09, match_ep(ep_2004, ep_2009) %>% rename_cols(., "ep_2004", "ep_2009")),
    
    tar_target(ep_09_14, match_ep(ep_2009, ep_2014) %>% rename_cols(., "ep_2009", "ep_2014")), 
    
    tar_target(ep_04_14, command = {
        missing_14 <- find_missing(ep_2014, "row_id", c(ep_09_14$ep_2014))
        missing_data_14 <- return_missing_data(ep_2014, "row_id", missing_14)
        noncons_04 <- return_nonconsecutive_data(ep_04_09, ep_2004, ep_2009, 
                                                 "row_id")
        match_ep(noncons_04, missing_data_14) %>% rename_cols(., "ep_2004", "ep_2014")
    }),
    
    tar_target(missing_ep_2014, find_missing(ep_2014, "row_id", c(ep_04_14$ep_2014, 
                                                                  ep_09_14$ep_2014)) %>%
                   rename(ep_2009=from, ep_2014=to)),
    
    tar_target(out_04_14, full_join(ep_04_09, ep_09_14, by = c("ep_2009")) %>% 
                   insert_nonconsecutive(., ep_04_14, "ep_2004", "ep_2014")  %>%
                   bind_rows(., missing_ep_2014)),
    
    tar_target(ep_14_19, match_ep(ep_2014, ep_2019) %>% rename_cols(., "ep_2014", "ep_2019")),
    
    tar_target(ep_09_19, command = {
        missing_19 <- find_missing(ep_2019, "row_id", c(ep_14_19$ep_2019))
        missing_data_19 <- return_missing_data(ep_2019, "row_id", missing_19)
        noncons_09 <- return_nonconsecutive_data(out_04_14, ep_2009, ep_2014, 
                                                 "row_id")
        match_ep(noncons_09, missing_data_19) %>% rename_cols(., "ep_2009", "ep_2019")
    }),
    
    tar_target(ep_04_19, command = {
        missing_19 <- find_missing(ep_2019, "row_id", c(ep_14_19$ep_2019, 
                                                        ep_09_19$ep_2019))
        missing_data_19 <- return_missing_data(ep_2019, "row_id", missing_19)
        noncons_04 <- return_nonconsecutive_data(out_04_14, ep_2004, ep_2009, 
                                                 "row_id")
        match_ep(noncons_04, missing_data_19) %>% rename_cols(., "ep_2004", "ep_2019")
    }),
    
    tar_target(missing_ep_2019, find_missing(ep_2019, "row_id", c(ep_04_19$ep_2019, 
                                                                  ep_09_19$ep_2019, 
                                                                  ep_14_19$ep_2019)) %>%
                   rename(ep_2014=from, ep_2019=to)),
    
    tar_target(out_04_19, full_join(out_04_14, ep_14_19, by = c("ep_2014")) %>% 
                   insert_nonconsecutive(., ep_09_19, "ep_2009", "ep_2019")  %>%
                   insert_nonconsecutive(., ep_04_19, "ep_2004", "ep_2019")  %>%
                   bind_rows(., missing_ep_2019)),
    
    tar_target(ep_panel, command = {
        all_data <- bind_rows(
            ep_2004 %>% mutate(data = "ep_2004"), 
            ep_2009 %>% mutate(data = "ep_2009"), 
            ep_2014 %>% mutate(data = "ep_2014"), 
            ep_2019 %>% mutate(data = "ep_2019"))
        
        create_panel(out_04_19, "row_id", all_data)
    }),
    
    # TODO: Senate ----------------------------------------
    
    # TODO: Matching panels -------------------------------
    
    NULL
)