#' Correct column classes.
#' 
#' @param df a dataframe
#' @return Processed dataframe that uses heurisitics to set columns to their apprpriate classification
#' @export
#' @examples
#' changeclass(mtcars)

changeclass <- function(df) {
    
    for (z in 1:length(colnames(df))) {
        if (nrow(df) < 32) {
            print("please change column types manually; record count < 32, stopping operation")
            break
        }
        len <- nrow(unique(df[colnames(df)[z]]))
        ## Turn off exponential notation otherwise regular expressions will identify these records as character
        options(scipen = 999)
        total <- length(grep("[A-z]|\\d{4}-\\d{2}-\\d{2}", df[[colnames(df)[z]]]))
        
        tbl <- table(grepl("\\d{4}-\\d{2}-\\d{2}", df[colnames(df)[z]][sample(nrow(df), 0.25 * (nrow(df))), ]))
        if (length(tbl) == 1 && names(tbl) == "FALSE") {
            tbl <- matrix(c(4, 1), ncol = 2, byrow = TRUE)
            tbl <- as.table(tbl)
        } else if (length(tbl) == 1 && names(tbl) == "TRUE") {
            tbl <- matrix(c(1, 4), ncol = 2, byrow = TRUE)
            tbl <- as.table(tbl)
        }
        
        if (tbl[1][[1]] < tbl[2][[1]] & class(df[[colnames(df)[z]]]) != "Date") {
            print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to date", sep = ""))
            df[colnames(df)[z]] <- as.Date(df[, colnames(df)[z]], format = "%Y-%m-%d")
        } else if (len > 32 & total == 0 & class(df[[colnames(df)[z]]]) %in% c("character", "factor", "Date")) {
            print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to numeric", sep = ""))
            df[, colnames(df)[z]] <- as.numeric(as.character(df[, colnames(df)[z]]))
        } else if (len > 32 & total > 1 & class(df[[colnames(df)[z]]]) != "character" & class(df[[colnames(df)[z]]]) != "Date") {
            print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to character", sep = ""))
            df[, colnames(df)[z]] <- as.character(df[[colnames(df)[z]]])
        } else if (len <= 32 & class(df[[colnames(df)[z]]]) != "factor" & class(df[[colnames(df)[z]]]) != "Date") {
            print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to factor", sep = ""))
            df[, colnames(df)[z]] <- as.factor(df[, colnames(df)[z]])
        }
    }
    print("If warning messages were output please check date classes")
    return(df)
}
