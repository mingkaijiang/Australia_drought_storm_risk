prepare_and_order_discrete_value_label <- function(plotDF) {
    
    ### brk labels
    value_lab <- as.character(rev(unique(plotDF$value_cat)))
    value_lab <- gsub(",", " to ", value_lab)
    value_lab <- gsub("]", "", value_lab)
    value_lab <- sub('.', '', value_lab)
    
    ### ordering
    test1 <- gsub( " .*$", "", value_lab)
    test2 <- sub(".+? ", "", value_lab)
    test3 <- gsub("to ", "", test2)
    tmp <- data.frame(cbind(test1, value_lab, test3))
    tmp$test1 <- as.numeric(as.character(tmp$test1))
    tmp$test3 <- as.numeric(as.character(tmp$test3))
    
    tmp <- tmp[order(tmp$test1),]
    tmp$value_lab <- paste0(tmp$test1, " to ", tmp$test3)
    
    value_lab <- tmp$value_lab
    
    return(value_lab)
}