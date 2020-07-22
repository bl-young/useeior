#' Disaggregate satellite tables based on specs
#' 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' 
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (model, sattable){
  
  # For each disaggregation:
  for (disagg in model$DisaggregationSpecs){
    
    # Subset the totals from the original sector
    old_sector_totals <- subset(sattable, SectorCode==disagg$OldSector, colnames(sattable))
    i<-0
    for (new_sector in disagg$NewSectors){
      i<-i+1
      new_sector_totals <- old_sector_totals
      new_sector_totals$SectorCode <- disagg$NewSectors[[i]]
      #new_sector_totals$SectorName <- newname
      
      # If satellite table data is provided for the new sector assign it here
      
      # Else if satellite table is disaggregated proportional to quantity do that here
      if(!is.null(disagg$NewSectorsOutput)){
        new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount * 
                                           (disagg$NewSectorsOutput[[i]] / Reduce("+",disagg$NewSectorsOutput)))
      }
      
      # Else, divide equally across new sectors
      else
        new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount / length(disagg$NewSectors))
      
      # Modify other metadata or DQI?
      
      
      # Append to the main dataframe
      sattable <- rbind(sattable,new_sector_totals)
    }
    # Remove the old_sector_totals
    sattable_disaggregated <- subset(sattable, SectorCode!=disagg$OldSector)
  }
  
  return (sattable_disaggregated)
}