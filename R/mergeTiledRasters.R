#' Merge tiled \code{Raster*} objects back together
#'
#' @description
#' Merge tiled \code{Raster*} objects computed by \code{tileRaster}.
#'
#' @param rasterlist \code{Raster*}. List of raster objects that should be merged back together.
#' @param overlap 'integer'. Overlap in pixels used for \code{tileRaster}
#' @param outpath 'character'. Path and name of the output file.
#'
#' @return
#' None. Merged tiles are written to single GeoTiff file \code{outpath}.
#'
#' @references
#' NONE
#'
#' @examples
#' \notrun{
#' mergeTiledRasters(rasterlist = my_list, overlap = 30, outpath = merged_raster)
#' }
#'
#' @export mergeTiledRasters
#' @name mergeTiledRasters
#'
mergeTiledRasters <- function(rasterlist, overlap = 0, outpath){
  
  temp_tilepath = paste0(dirname(outpath), "/tile_no_overlap/")
  if (!dir.exists(temp_tilepath))
    dir.create(temp_tilepath, recursive = TRUE)
  rst = lapply(rasterlist, function(r){
    rstc = stack(r)
    tr = c(overlap+1, nrow(rstc)-overlap)
    tc = c(overlap+1, ncol(rstc)-overlap)
    
    rstc = crop(rstc, extent(rstc, tr[1], tr[2], tc[1], tc[2]))
    writeRaster(rstc, format = "GTiff",
                filename = paste0(temp_tilepath, basename(dirname(r)), "_", basename(r)),  
                bylayer = FALSE, overwrite = TRUE)
  })
  
  
  
  ## Compute col/row positions for cutting the tiles
  rows = nrow(raster[[1]])
  cols = ncol(raster[[1]])
  nr = rows%/%tilenbr[2]
  nc = cols%/%tilenbr[1]
  tilenbrs = tilenbr[1] * tilenbr[2]
  
  tc = lapply(seq(0, tilenbr[1]-1), function(i){
    data.frame(tc = c(i*nc+1, (i+1)*nc))
  })
  tc[[tilenbr[1]]][2,] = cols
  
  tr = lapply(seq(0, tilenbr[2]-1), function(i){
    data.frame(tr = c(i*nr+1, (i+1)*nr))
  })
  tr[[tilenbr[2]]][2,] = rows
  
  ## Add overlap
  tc[[1]][2,] = tc[[1]][2,] + overlap
  if(length(tc) > 2){
    for(i in seq(2, length(tc)-1)){
      tc[[i]][1, ] = tc[[i]][1, ] - overlap
      tc[[i]][2, ] = tc[[i]][2, ] + overlap
    }
  }
  tc[[tilenbr[1]]][1,] = tc[[tilenbr[1]]][1,] - overlap
  
  tr[[1]][2,] = tr[[1]][2,] + overlap
  if(length(tr) > 2){
    for(i in seq(2, length(tr)-1)){
      tr[[i]][1, ] = tr[[i]][1, ] - overlap
      tr[[i]][2, ] = tr[[i]][2, ] + overlap
    }
  }
  tr[[tilenbr[2]]][1,] = tr[[tilenbr[2]]][1,] - overlap
  
  
  ## Create tile list
  tiles = lapply(tr, function(atr){
    lapply(tc, function(atc){
      data.frame(tr = atr,
                 tc = atc)
    })
  })
  
  tiles = unlist(tiles, recursive=FALSE)
  
  for(r in seq(nlayers(raster))){
    rst = raster[[r]]
    for(t in seq(length(tiles))){
      tile = tiles[[t]]
      lzeros = nchar(as.character(max(unlist(tiles))))
      subpath = paste0(outpath, "c", 
                       sprintf(paste0("%0", lzeros, "d"), tile$tc[1]), 
                       "-", 
                       sprintf(paste0("%0", lzeros, "d"), tile$tc[2]), 
                       "_r", 
                       sprintf(paste0("%0", lzeros, "d"), tile$tr[1]), 
                       "-", 
                       sprintf(paste0("%0", lzeros, "d"), tile$tr[2]), "/")
      if (!dir.exists(subpath))
        dir.create(subpath)
      filename = paste0(names(rst), "_c", 
                        sprintf(paste0("%0", lzeros, "d"), tile$tc[1]), 
                        "-", 
                        sprintf(paste0("%0", lzeros, "d"), tile$tc[2]), 
                        "_r", 
                        sprintf(paste0("%0", lzeros, "d"), tile$tr[1]), 
                        "-", 
                        sprintf(paste0("%0", lzeros, "d"), tile$tr[2]))
      rst_tile = crop(rst, extent(rst, tile$tr[1], tile$tr[2], tile$tc[1], tile$tc[2]))
      writeRaster(rst_tile, format = "GTiff",
                  filename = paste0(subpath, filename),  
                  bylayer = FALSE, overwrite = FALSE)
    }
    
  }
}
