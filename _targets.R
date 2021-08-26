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
    
    # FIXME: Missing NSTRANA (MODS)
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
                   ROK_NAROZENI = 1998 - VEK)
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
                        psp_parties, cpp, cns)
    }),
    
    tar_target(psp_2021, command = {
        psp_parties <- read_parties(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrkl.xlsx"))
        cpp <- read_cpp(here("data", "PS2021", "PS2021ciselniky20210824", "cpp.xlsx"))
        cns <- read_cns(here("data", "PS2021", "PS2021ciselniky20210824", "cns.xlsx"))
        read_candidates(here("data", "PS2021", "PS2021reg20210824_xlsx", "psrk.xlsx"), 
                        psp_parties, cpp, cns)
    }), 
    
    # Matching
    # TODO: non-consecutive matching (e.g. 1996 - 2002)
    
    tar_target(psp_96_98, match_psp(psp_1996, psp_1998) %>% rename_cols(., "psp_1996", "psp_1998")), 
    
    # FIXME: low number of matches?
    tar_target(psp_98_02, match_psp(psp_1998, psp_2002)), 
    
    tar_target(psp_02_06, match_psp(psp_2002, psp_2006)), 
    
    tar_target(psp_06_10, match_psp(psp_2006, psp_2010)), 
    
    tar_target(psp_10_13, match_psp(psp_2010, psp_2013)), 
    
    tar_target(psp_13_17, match_psp(psp_2013, psp_2017)), 
    
    tar_target(psp_17_21, match_psp(psp_2017, psp_2021)),
    
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
                            parties, cpp, cns, function(x) {x %>% rename(KSTRANA = ESTRANA)})
    }),
    
    #######################################################
    # TODO: Senate
    #######################################################
    
    
    NULL
)