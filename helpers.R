library(Metrics)

## Helpers for gapfill function
create_validation_stack <- function(x, n){
  
  n_layers <- dim(x)[3]
  n_per_layer <- round(n / n_layers, 0)
  validation_stack <- x
  validation_pixels <- data.frame(layer = NULL, pixel = NULL)
  validation_values <- NULL
  
  for(i in 1:n_layers){
    
    has_data <- which(!is.na(x[[i]][]))
    if(length(has_data) > 0){
      validation_pixels_layer <- sample(has_data, n_per_layer)
      validation_values <- c(validation_values,
                             validation_stack[[i]][validation_pixels_layer])
      validation_stack[[i]][validation_pixels_layer] <- NA
      validation_pixels <- rbind(validation_pixels,
                                 cbind(i,validation_pixels_layer))
    }
  }
  names(validation_pixels)  <- c("layer", "cell")
  return(list(validation_stack = validation_stack,
              validation_values = validation_values,
              validation_pixels = validation_pixels))
}

# Get approxNA values at specific points
get_approxNA <- function(x, query_df){
  
  for(i in unique(query_df$layer)){
    x[[i]][query_df$cell[query_df$layer == i]] <- NA
  }

  interpolated <- approxNA(x, rule = 2)
  extract_by_layer_cell(interpolated, query_df)
  
}


extract_by_layer_cell <- function(x, query_df){ 
  
  values <- NULL
  for(i in unique(query_df$layer)){
  values <- c(values, 
              x[[i]][query_df$cell[query_df$layer == i]])
  
  }
  return(values)
}

mse_na <- function (actual, predicted) 
{
  return(mean(se(actual, predicted), na.rm=T))
}