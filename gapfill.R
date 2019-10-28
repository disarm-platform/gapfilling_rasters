# Gapfilling function
# `x` is a raster stack of equally temporally spaced
# raster layers with missing values coded as `NA`.
# `data_points` controls the number of data used to 
# train gap filling model. Larger numbers take longer.
# `Elev` is the corresponding elevation raster 
# (at same resolution and extent as x, i.e. identical grids)
library(raster)
library(randomForest)
source("https://raw.githubusercontent.com/HughSt/gapfilling_rasters/master/helpers.R")

gapfill <- function(x, data_points = 20000, Elev){ 
  
  if(res(Elev)[1] != res(x)[1]){
    stop("Resolutions of raster stack and elevation need to be the same!")
  }

  # sample non-missing points from each layer
  n_layers <- dim(x)[3]
  
  # Create layer index
  layer_index <- sort(rep(1:n_layers,length(x)/n_layers))
  
  # Create data frame of all data
  all_data <- data.frame(x = rep(coordinates(x)[,1], n_layers),
                         y = rep(coordinates(x)[,2], n_layers),
                         data = as.vector(x[]),
                         elev = Elev[],
                         layer = layer_index)
  
  # Identify where missing values are
  data_index <- which(!is.na(all_data$data))
  missing_index <- which(is.na(all_data$data))
  
  # Identify all pixels adjacent to missing values
  # work in progress...

  # boundary_pixels <- data.frame(layer=NULL, cell=NULL)
  # for(i in 1:dim(x)[3]){
  #   missing_pixels_adjacent <- adjacent(x[[i]], which(is.na(x[[i]][])))[,2]
  # 
  #   if(length(missing_pixels_adjacent)>0){
  #     boundary_pixels_index <- missing_pixels_adjacent[which(!is.na(x[[i]][missing_pixels_adjacent]))]
  # 
  #     if(length(boundary_pixels_index)>(data_points/12)){
  #       boundary_pixels_index <- boundary_pixels_index[round(seq(1, length(boundary_pixels_index),
  #                                                                length.out=round(data_points/12)))]
  #     }
  # 
  #     # TODO (potentially) remove any that have a missing value in an adjacent
  #     # cell on the elevation raster (i.e. along the coast)
  #     boundary_pixels <- rbind(boundary_pixels, cbind(i,boundary_pixels_index))
  #   }
  # 
  # }
  
  # temporally_adjacent_pixels <- data.frame(layer=NULL, cell=NULL)
  # for(j in 1:dim(x)[3]){
  #   
  #   missing_pixels <- which(is.na(x[[j]][]))
  # }
    
    # Include nearest value in time for each missing pixel
  
  # Take a sequential sample of data
  if(length(data_index) >= data_points){
    sample_index <- data_index
  }else{
  sample_index <- data_index[round(seq(1, length(data_index), length.out=data_points))]
  }
  
  model_data <- all_data[sample_index,]
  model_data$cell <- raster::cellFromXY(x, model_data[,c("x","y")])
  pred_data <- all_data[missing_index,]
  pred_data$cell <- raster::cellFromXY(x, pred_data[,c("x","y")])
  
  # If there are multiple rasterLayers, use the approxNA function to get linear interpolations
  # for missing values which will act as a covariate
  if(n_layers > 1){
  model_data$interpolation <- get_approxNA(x, model_data)
  pred_data$interpolation <- get_approxNA(x, pred_data)
  }else{
    model_data$interpolation <- 0
    pred_data$interpolation <- 0
  }
  
  # Remove any cells which are na in Elevation
  # i.e. these lie outside the country
  model_data <- model_data[complete.cases(model_data),]
  pred_data <- pred_data[-which(pred_data$cell %in% which(is.na(Elev[]))),]
  
  # Build model
  rf_mod <- randomForest(data ~ interpolation + elev + x + y + layer,
                          data = model_data)
  
  # Loop to inpute missing values
  predictions <- predict(rf_mod, newdata = pred_data)
  
  if(n_layers == 1){
      x[pred_data$cell] <- as.vector(predictions)
  }else{
    for(i in 1:dim(x)[3]){
      x[[i]][pred_data$cell[pred_data$layer == i]] <- as.vector(predictions[pred_data$layer == i])
    }
  }
  return(x)
}
