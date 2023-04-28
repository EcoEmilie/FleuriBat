DensityFonction = function(shape, site_buffer, buffer_area){
  tab = st_intersection(shape, site) %>%
    mutate(longueur = st_length(geom))
  density = c(ifelse(nrow(tab) == 0, 0,(sum(q$longueur)/buffer_area) * 10000))
  return(density)
}

DensityFonction()