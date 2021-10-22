calculate_VPD_based_on_es_and_vp3pm_Adriano <- function (sourceDir,
                                                         destDir,
                                                         varName,
                                                         user.region.name) {
    
    ### read in the R database
    es <- readRDS(paste0(sourceDir, "/es_", user.region.name, "_sites.rds"))
    vp3pm <- readRDS(paste0(sourceDir, "/vp3pm_", user.region.name, "_sites.rds"))
    
    
    ### dataset es has temporal coverage: 19110101 to 20200331
    ### dataset vp3pm has temporal coverage: 19710101 to 20200331
    vp.length <- dim(vp3pm)[3]
    es.length <- dim(es)[3]
    
    s.pos <- es.length - vp.length + 1
    e.pos <- es.length
    
    ### subset
    es.sub <- es[,,s.pos:e.pos]
    
    ### matrix deletion
    ### following Monteith and Unsworth (1990):
    ### D (kPa) = es - ea
    ### But for ease of plotting purpose, calculate ea - es so that there is no negative values
    
    ### BM: vp3pm in actually in hPa, not kPa
    ### BM: Divide vp3pm by 10, and calculate VPD as es - ea, where ea = vp3pm/10
    # out <- es.sub - vp3pm
    # out <- vp3pm - es.sub
    out <- es.sub - vp3pm/10
    
    ### save output
    saveRDS(out, file=paste0(destDir, "/", varName, "_", user.region.name, "_sites.rds"))
    
    
    
    
}