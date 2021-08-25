# reading psrkl.xlsx etc (seznam kandidujících stran)
read_parties <- function(path){
    read_excel(path) %>%
        select(KSTRANA, ZKRATKAK8)
}

# reading cpp.xlsx etc. (strany pro stranickou příslušnost kandidátů)
read_cpp <- function(path){
    read_excel(path) %>%
        select(PSTRANA, ZKRATKAP8)
}

# reading cns.xlsx etc. (strany nominující kandidáty)
read_cns <- function(path){
    read_excel(path) %>%
        select(NSTRANA, ZKRATKAN8)
}

read_candidates <- function(list_path, parties_df, cpp_df, cns_df){
    read_excel(list_path) %>%
        left_join(., parties_df, by = "KSTRANA") %>%
        left_join(., cpp_df, by = "PSTRANA") %>%
        left_join(., cns_df, by = "NSTRANA")
}

parse_candidates_preferential_votes <- function(region_node){
    candidates <- region_node %>%
        html_nodes("kandidat")
    
    region_no <-  region_node %>%
        html_attr("cis_kraj") %>%
        as.numeric()
    
    tibble::tibble(
        VOLKRAJ = region_no, 
        KSTRANA = candidates %>% html_attr("kstrana") %>% as.numeric, 
        PORCISLO = candidates %>% html_attr("porcislo") %>% as.numeric, 
        hlasy = candidates %>% html_attr("hlasy") %>% as.numeric
    )
}

parse_preferential_votes <- function(path){
    regions <- read_html(path) %>%
        html_nodes("kraj")
    
    purrr::map_df(regions, parse_candidates_preferential_votes)
}
