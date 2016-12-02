#' Create dummy columns.
#' 
#' @param df A dataframe
#' @param DummyColumns a collection or single value that will be converted to dummy columns. This should be a character value. 
#' @param dropcols True or False parameter indicating whether to keep or drop the columns that were dummy coded. 
#' @return Processed dataframe with new dummy columns
#' @export
#' @examples
#' dummyCode(mtcars, c('vs','gear','carb'),FALSE)

dummyCode <- function(df, DummyColumns, dropcols) {
    
    # Create a copy of original dataset
    FileToDummyCode = df
    # greate a unique id for this dataset to group on
    FileToDummyCode$ByVarID = 1:nrow(FileToDummyCode)
    
    for (i in 1:length(DummyColumns)) {
        
        print(DummyColumns[i])
        
        # Switch data to character
        Table = as.data.frame.matrix(table(FileToDummyCode$ByVarID, FileToDummyCode[, DummyColumns[i]]))
        
        # rename our columns
        colnames(Table) = paste(DummyColumns[i], ".", colnames(Table), sep = "")
        
        # bind data back to original dataset
        df = cbind(df, Table)
        
    }
    if (dropcols == T) {
        return(df[, -c(which(colnames(df) %in% DummyColumns))])
    } else {
        return(df)
    }
    
}

