library(gapfill)
library(downloader)

# Get LST data for 2015
download(url="https://www.dropbox.com/s/8a2wl6zdveos038/LST.RData?dl=1",
         destfile="LST.RData")
load("LST.RData")

# Change NaN's to NA's
LST[][is.na(LST[][])] <-NA

# Generate array
LST_array <- array(data=as.vector(LST),dim=c(41,41,12,1))
Image(LST_array) # This seems to be the wrong alignment

# Try gapfill
out <- Gapfill(LST_array)
Image(out$fill)
sum(is.na(out$fill)) # Still 29 NA values

# Write functiion to gap fill using GAM
gapfill_GAM <- function(x){
  
  missing_index <- which(is.na(x[]))
  data_index <- which(!is.na(x[]))
  
  # sample non-missing points from each layer
  n_layers <- dim(x)[3]
  
  # Create layer index
  layer_index <- sort(rep(1:n_layers,length(x)/n_layers))
  
  all_data <- data.frame(x=rep(coordinates(x)[,1],n_layers),
                         y=rep(coordinates(x)[,2],n_layers),
                         data=as.vector(x[]),
                         layer=layer_index)
  
  # Take a sequential sample of data
  sample_index <- data_index[round(seq(1,length(data_index),length.out=1000))]
  
  model_data <- all_data[sample_index,]
  pred_data <- all_data[missing_index,]
  
  # Build model
  gam_mod <- gam(data ~ te(x,y,layer), data=model_data)
  
  # Loop to inpute missing values
  predictions <- predict(gam_mod, newdata=pred_data)
 
   for(i in 1:nrow(pred_data)){
    x[[pred_data$layer[i]]][as.numeric(row.names(pred_data)[i])]<-predictions[i]
  }
  
}

