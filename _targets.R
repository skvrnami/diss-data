library(targets)
library(tarchetypes)

source("R/loading-data.R")
source("R/matching.R")

tar_option_set(packages = c("dplyr", "rvest", "here", # "listr", 
                            "rimr", "readxl"))

# TODO: zkontrolovat rekodovani titulu

list(
    
    #######################################################
    # Parliament 1990, 1992
    #######################################################
    
    # TODO: FS 1990, 1992
    # TODO: Czech National Council 1990, 1992
    
    #######################################################
    # Chamber of Deputies
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
    
    # Matching
    
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
    
    tar_target(missing_2006, command = {
        missing_06_2 <- find_missing(psp_2006, "row_id", 
                                     c(psp_02_06$psp_2006, 
                                       psp_98_06$psp_2006))
        return_missing_data(psp_2006, "row_id", missing_06_2)
    }),
    
    tar_target(psp_96_06, command = {
        noncons_96 <- return_nonconsecutive_data(out_96_02, psp_1996, psp_2002, 
                                                 "row_id")
        match_psp(noncons_96, missing_2006) %>% rename_cols(., "psp_1996", "psp_2006")
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
    
    # tar_target(panel_96_06, command = {
    #     psp_1996 <- psp_1996
    #     psp_1998 <- psp_1998
    #     psp_2002 <- psp_2002
    #     psp_2006 <- psp_2006
    #     create_panel_output(out_96_06)   
    # }),
    
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
    
    #######################################################
    # Regional elections
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
                        parties, cpp, cns)
    }),
    
    tar_target(reg_2020, command = {
        parties <- read_parties(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrkl.xlsx")) %>% 
            unique
        cpp <- read_cpp(here("data", "KZ2020", "KZ2020ciselniky20201002", "cpp.xlsx"))
        cns <- read_cns(here("data", "KZ2020", "KZ2020ciselniky20201002", "cns.xlsx"))
        read_candidates(here("data", "KZ2020", "KZ2020reg20201004a_xlsx", "kzrk.xlsx"), 
                        parties, cpp, cns)
    }),
    
    #######################################################
    # Municipal elections
    #######################################################
    
    # TODO: 1994
    # TODO: 1998
    # TODO: 2002
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
                        parties, cpp, cns)
    }),
    
    #######################################################
    # European Parliament
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
    
    # Matching
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
    
    
    #######################################################
    # TODO: Senate
    #######################################################
    
    
    NULL
)

# tar_load(psp_1996)
# tar_load(psp_1998)
# tar_load(psp_2002)
# tar_load(psp_2006)
# tar_load(psp_2010)
# tar_load(psp_2013)
# tar_load(psp_2017)
# tar_load(psp_2021)
# panel_96_21 <- create_panel_output(out_96_21)

# tar_load(ep_2004)
# tar_load(ep_2009)
# tar_load(ep_2014)
# tar_load(ep_2019)
# tar_load(out_04_19)
# panel_ep <- create_panel_output(out_04_19)