convert_continuous_to_discrete_bins_for_deficit <- function(inDF, n.discrete.colors) {
    
    ### subset values not == 0
    subDF <- inDF
    
    ### n.discrete.colors takes out value = 0
    n.discrete.colors.rev <- n.discrete.colors 

    ### get the value breaks
    value_brks <- round(quantile(subDF$intensity, 
                                 probs = seq(0, 1, 
                                             100/n.discrete.colors.rev/100)), 2)
    
    #### create categorical plotting labels for each plotting variables
    inDF$value_cat <- cut(inDF$intensity, 
                          breaks = value_brks)
    
    ### remove NAs
    inDF <- inDF[!is.na(inDF$value_cat),]
    
    
    ### brk labels
    value_lab <- as.character(rev(unique(inDF$value_cat)))
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
    
    ### return
    return(list(outDF=inDF, lab=value_lab))
    
}