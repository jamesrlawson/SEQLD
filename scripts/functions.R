

trim <- function(x) {
  gsub('\\s+', '',x)
}


CV <- function(x){
  sqrt(var(x))/mean(x)
}


units.SLA <- function(df) {
  
  df$SLA.units_new <- "m2/kg"
  
  for(i in 1:nrow(df)) {    
    
    if (df$SLA.units[i] == "cm2/g") {
      df$SLA[i] <- df$SLA[i] * 0.1
      
    }          
    
  }
  
  return(df)
  
}


units.LMA <- function(df) {
  
  df$SLAfromLMA <- "NA"
   
    for(i in 1:nrow(df)) {    
      
      if (df$LMA.units[i] == "g/cm2") {
        df$SLAfromLMA[i] <- 1 / (df$LMA[i]/1000) # the units for Rach's fieldwork data appear to be wrong, should be g/m2?
        
      } else {
        if (df$LMA.units[i] == "g/m2") {
          df$SLAfromLMA[i] <- 1/(df$LMA[i]/1000)      
          
          }
        }
      }
      
  return(df)
}
  

SLA_LMA.combine <- function(df) {

  for(i in 1:nrow(df)) {    
    
    if (is.na(df$SLA[i])) {
      df$SLA[i] <- df$SLAfromLMA[i]
    }
  }
  
return(df)

}


units.WD <- function(df) {
      
    for(i in 1:nrow(df)) {    
      
      if(is.na(df$wood.density[i])) {
        df$wood.density[i] <- as.numeric("NA")
        
       } else {          
          if (df$wood.density[i] < 10) {
            df$wood.density[i] <- as.numeric(df$wood.density[i])
            
          } else {
            if (df$wood.density[i] > 10) {
              df$wood.density[i] <- as.numeric(df$wood.density[[i]]) * 0.001
            }
          }                  
      }                
    }
    
    return(df)
    
  }

capitalise <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) ## capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   ## everything else lowercase
  paste0(first, rest)
}