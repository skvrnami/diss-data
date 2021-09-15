rename_cols <- function(df, x_name, y_name){
    names_map <- c("x"=x_name, "y"=y_name)
    rename_with(df, ~names_map[.x])
}

match_psp <- function(x, y){
    find_all_similar(x, y, start = 1, 
                     cores = 4, 
                     eq = c("JMENO", "PRIJMENI"), 
                     eq_tol = list(c("ROK_NAROZENI", 1)), 
                     id = "row_id", 
                     compare_cols = c("JMENO", "PRIJMENI", 
                                      "ROK_NAROZENI", 
                                      "POVOLANI", "BYDLISTEN", 
                                      "KRAJ", "TITULY", "TITUL_KATEGORIE",
                                      "PSTRANA", "ZKRATKAN8"))
}

match_ep <- function(x, y){
    find_all_similar(x, y, start = 1, 
                     cores = 4, 
                     eq = c("JMENO", "PRIJMENI"), 
                     eq_tol = list(c("ROK_NAROZENI", 1)), 
                     id = "row_id", 
                     compare_cols = c("JMENO", "PRIJMENI", "ROK_NAROZENI", "POVOLANI", 
                                      "BYDLISTEN", "TITULY", "TITUL_KATEGORIE", 
                                      "PSTRANA", "NSTRANA"))
}

match_reg <- function(x, y){
    find_all_similar(x, y, start = 1, 
                     cores = 4, 
                     eq = c("JMENO", "PRIJMENI", "KRZAST"), 
                     eq_tol = list(c("ROK_NAROZENI", 1)), 
                     id = "row_id", 
                     compare_cols = c("JMENO", "PRIJMENI", "ROK_NAROZENI", "POVOLANI", 
                                      "BYDLISTEN", "TITULY", "TITUL_KATEGORIE", 
                                      "PSTRANA", "NSTRANA"))
}

match_mc <- function(x, y){
    find_all_similar(x, y, start = 1, 
                     cores = 4, 
                     eq = c("JMENO", "PRIJMENI", "KODZASTUP"), 
                     eq_tol = list(c("ROK_NAROZENI", 1)), 
                     id = "row_id", 
                     compare_cols = c("JMENO", "PRIJMENI", "ROK_NAROZENI", "POVOLANI", 
                                      "BYDLISTEN", "TITULY", "TITUL_KATEGORIE", 
                                      "PSTRANA", "NSTRANA", "KODZASTUP"))
}

match_m <- match_mc


# match_mc_start <- function(x, y, start = 1, end = nrow(x)){
#     for(row in start:end){
#         cat(row, "\n")
#         rimr::find_similar(x, y, row = row, 
#                      eq = c("JMENO", "PRIJMENI", "KODZASTUP"), 
#                      eq_tol = list(c("ROK_NAROZENI", 1)), 
#                      id = "row_id", 
#                      compare_cols = c("JMENO", "PRIJMENI", "ROK_NAROZENI", "POVOLANI", 
#                                       "BYDLISTEN", "TITULY", "TITUL_KATEGORIE", 
#                                       "PSTRANA", "NSTRANA", "KODZASTUP"))
#     }
# }


to_name <- function(df){
    rlang::as_name(enquo(df))
}

find_missing_data <- function(df, found_vector, row_id = "row_id"){
    find_missing(df, row_id, found_vector) %>%
        return_missing_data(df, row_id, .)
}


create_panel <- function(out, row_id = "row_id", all_data){
    tmp <- out %>% mutate(id = row_number()) %>%
        select(id, everything())
    iter <- tidyr::gather(tmp, "data", row_id, 2:ncol(tmp)) %>%
        filter(!is.na(row_id))
    iter %>% left_join(., all_data, by = c("data", "row_id"))
}
