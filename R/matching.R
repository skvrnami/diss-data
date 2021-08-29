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