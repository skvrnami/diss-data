pass <- function(x){x}

#' Clean colnames in Excel files exported from DBF files
#' 
#' colnames in files exported from .dbf have format 'variable,variable_type,number(,number)'
#' this function removes all text after the first comma
clean_dbf_excel_colnames <- function(df){
    n <- colnames(df)
    colnames(df) <- stringr::str_remove(n, ",[A-Z0-9,]+")
    df
}

extract_el_value <- function(x, el){
    x %>% html_node(el) %>% html_text()
}

extract_el_value_num <- function(x, el){
    extract_el_value(x, el) %>% as.numeric()
}


VSTRANA_MAP_96 <- c(
    "1" = "SD",
    "2" = "ČSSD",
    "3" = "ODS",
    "4" = "PB",
    "5" = "NEZ",
    "6" = "MNS-HSMS",
    "7" = "DEU",
    "8" = "ODA",
    "9" = "SČK",
    "10" = "KDU-ČSL",
    "11" = "DŽJ",
    "12" = "SDL",
    "13" = "ČMUS",
    "14" = "ČP",
    "15" = "KSČM",
    "16" = "SZ",
    "17" = "SPR-RSČ",
    "18" = "LB",
    "19" = "HSMSMNSJ",
    "20" = "CAO"
)

VSTRANA_MAP_98 <- c(
    "1"="KDU-ČSL", 
    "2" = "NEZ", 
    "3" ="CAO", 
    "4" = "DEU", 
    "5" ="ODS", 
    "6" = "OK", 
    "7" ="ČSSD", 
    "9" = "KSČM", 
    "10" ="SPR-RSČ",
    "11" = "US", 
    "12" ="DŽJ", 
    "13" = "ČSNS",
    "14" ="SDČR",
    "15" = "A2000", 
    "16" ="PB",
    "17" = "ODA", 
    "18" ="SZ"
)

clean_ps <- function(df, VSTRANA_MAP){
    
    # - construct NSTRANA
    # - rename VSTRANA
    df %>%
        mutate(NSTRANA = purrr::map_chr(VSTRANA, function(x) VSTRANA_MAP[as.character(x)]), 
               KSTRANA = purrr::map_chr(VSTRANA, function(x) VSTRANA_MAP[as.character(x)]), 
               TITULZA = stringr::str_extract(PRIJMENI, ",[A-Za-z,\\. ]+") %>% gsub(",[ ]*", "", .), 
               PRIJMENI = stringr::str_remove(PRIJMENI, ",[A-Za-z,\\. ]+")
               ) %>%
        select(-VSTRANA) %>%
        rename(NAZEV_STRK = NAZEV_VSTRANA)
        
}

# reading psrkl.xlsx etc (seznam kandidujících stran)
read_parties <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(KSTRANA, ZKRATKAK8, NAZEV_STRK)
}

read_municipal_parties <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(KODZASTUP, COBVODU, POR_STR_HL, OSTRANA, NAZEVCELK)
}

parse_ep_rkl_row <- function(x){
    tibble::tibble(
        ESTRANA = x %>% html_node("estrana") %>% html_text() %>% as.numeric(),
        VSTRANA = x %>% html_node("vstrana") %>% html_text() %>% as.numeric(),
        NAZEVCELK = x %>% html_node("nazevcelk") %>% html_text(), 
        NAZEV_STRE = x %>% html_node("nazev_stre") %>% html_text(), 
        ZKRATKAE30 = x %>% html_node("zkratkae30") %>% html_text(), 
        ZKRATKAE8 = x %>% html_node("zkratkae8") %>% html_text(), 
        POCSTRVKO = x %>% html_node("pocstrvko") %>% html_text() %>% as.numeric(), 
        SLOZENI = x %>% html_node("slozeni") %>% html_text(), 
        STAVREG = x %>% html_node("stavreg") %>% html_text() %>% as.numeric(), 
        PLATNOST = x %>% html_node("platnost") %>% html_text() %>% as.numeric(), 
        POCMANDCR = x %>% html_node("pocmandcr") %>% html_text() %>% as.numeric(), 
    )
}

# (here("data", "EP2004", "EP2004reg", "eprkl.xlsx"))
read_parties_xml <- function(path, cleanup_f = pass){
    read_html(path, encoding = "WINDOWS-1250") %>%
        html_nodes("ep_rkl_row") %>%
        purrr::map_df(., parse_ep_rkl_row) %>%
        cleanup_f
} 

# reading cpp.xlsx etc. (strany pro stranickou příslušnost kandidátů)
read_cpp <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(PSTRANA, ZKRATKAP8)
}

parse_cpp_row <- function(x){
    tibble::tibble(
        PSTRANA = x %>% html_node("pstrana") %>% html_text() %>% as.numeric(), 
        NAZEV_STRP = x %>% html_node("nazev_strp") %>% html_text(), 
        ZKRATKAP30 = x %>% html_node("zkratkap30") %>% html_text(),
        ZKRATKAP8 = x %>% html_node("zkratkap8") %>% html_text() 
    )
}

read_cpp_xml <- function(path){
    read_html(path, encoding = "WINDOWS-1250") %>%
        html_nodes("cpp_row") %>%
        purrr::map_df(., parse_cpp_row) %>%
        select(PSTRANA, ZKRATKAP8)
}

# reading cns.xlsx etc. (strany nominující kandidáty)
read_cns <- function(path, cleanup_f = pass){
    read_excel(path) %>%
        cleanup_f %>%
        select(NSTRANA, ZKRATKAN8)
}

parse_cns_row <- function(x){
    tibble::tibble(
        NSTRANA = x %>% html_node("nstrana") %>% html_text() %>% as.numeric(), 
        NAZEV_STRN = x %>% html_node("nazev_strn") %>% html_text(), 
        ZKRATKAN30 = x %>% html_node("zkratkan30") %>% html_text(),
        ZKRATKAN8 = x %>% html_node("zkratkan8") %>% html_text() 
    )
}

read_cns_xml <- function(path){
    read_html(path, encoding = "WINDOWS-1250") %>%
        html_nodes("cns_row") %>%
        purrr::map_df(., parse_cns_row) %>%
        select(NSTRANA, ZKRATKAN8)
}

read_candidates <- function(list_path, parties_df, cpp_df, cns_df, 
                            cleanup_f = pass){
    read_excel(list_path) %>%
        cleanup_f %>%
        left_join(., parties_df, by = "KSTRANA") %>%
        left_join(., cpp_df, by = "PSTRANA") %>%
        left_join(., cns_df, by = "NSTRANA")
}

read_municipal_candidates <- function(list_path, parties_df, cpp_df, cns_df, 
                            cleanup_f = pass){
    read_excel(list_path) %>%
        cleanup_f %>%
        left_join(., parties_df, by = c("KODZASTUP", "COBVODU", "POR_STR_HL", "OSTRANA")) %>%
        left_join(., cpp_df, by = "PSTRANA") %>%
        left_join(., cns_df, by = "NSTRANA")
}

parse_ep_regkand_row <- function(x){
    tibble::tibble(
        ESTRANA = extract_el_value_num(x, "estrana"), 
        PORCISLO = extract_el_value_num(x, "porcislo"), 
        JMENO = extract_el_value(x, "jmeno"), 
        PRIJMENI = extract_el_value(x, "prijmeni"), 
        TITULPRED = extract_el_value(x, "titulpred"), 
        TITULZA = extract_el_value(x, "titulza"), 
        BYDLISTEN = extract_el_value(x, "bydlisten"), 
        BYDLISTEK = extract_el_value_num(x, "bydlistek"), 
        PSTRANA = extract_el_value_num(x, "pstrana"),
        NSTRANA = extract_el_value_num(x, "nstrana"),
        POCHLASU = extract_el_value_num(x, "pochlasu"),
        POCPROC = extract_el_value_num(x, "pocproc"),
        POCPROCVSE = extract_el_value_num(x, "pocprocvse"),
        MANDAT = extract_el_value_num(x, "mandat"),
        PORADIMAND = extract_el_value_num(x, "poradimand"),
        PORADINAHR = extract_el_value_num(x, "poradinahr")
    )
}

read_candidates_xml <- function(list_path, parties_df, cpp_df, cns_df, 
                                cleanup_f = pass){
    read_html(list_path, encoding = "WINDOWS-1250") %>%
        html_nodes("ep_regkand_row") %>%
        purrr::map_df(., parse_ep_regkand_row) %>%
        cleanup_f %>%
        left_join(., parties_df, by = "KSTRANA") %>%
        left_join(., cpp_df, by = "PSTRANA") %>%
        left_join(., cns_df, by = "NSTRANA")
}
