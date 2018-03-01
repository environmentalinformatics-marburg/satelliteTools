#' Tile \code{Raster*} object with or without overlap
#'
#' @description
#' Tile \code{Raster*} object using an optional overlap between the tiles.
#'
#' @param raster \code{Raster*}. Object that should be tiled.
#' @param tilenbr 'integer'. Vector given the number of tiles in c(x,y) direction.
#' @param overlap 'integer'. Overlap in pixels between the individual tiles. Default NULL does not include any overlap.
#' @param outpath 'character'. Top level path of the output files.
#' @param subpath 'character'. Path created within each tile path (e.g. for naming the product).
#'
#' @return
#' None. Tiles are written to GeoTiff files in directory \code{outpath}.
#'
#' @references
#' NONE
#'
#' @examples
#' \notrun{
#' combineRasterTiles(raster = ndvi, tilenbr = c(5,4), overlap = 30, outpath = modis_tiles)
#' }
#'
#' @export combineRasterTiles
#' @name combineRasterTiles
#'
combineRasterTiles <- function(inpath, subpath, outpath, trgt){
  path_tls = list.dirs(inpath, recursive = FALSE)
  tls = basename(path_tls)
  
  tfns = paste0(path_tls, "/", tls, ".txt")
  
  tinfo = lapply(tfns, function(t){
    info = read.csv(t)  
  })
  
  
  fptrn = list.files(paste0(path_tls[1], "/", subpath), recursive = FALSE, 
                     pattern = glob2rx("*.tif"), full.names = TRUE)
  
  foreach(i = seq(length(fptrn)), .packages = "raster",
          .export = ls(envir = globalenv())) %dopar% {
            
            act_ptrn = fptrn[i]
            
            target_filepath = basename(act_ptrn)
            target_filepath = gsub(paste0("_", tls[1]), "", target_filepath)
            target_filepath = paste0(outpath, "/", subpath, "/", target_filepath)
            
            target = raster(trgt)
            target = setValues(target, NA)
            names(target) = basename(target_filepath)
            
            act_files = lapply(tls, function(t){
              gsub(tls[1], t, act_ptrn)
            })

            for(j in seq(length(act_files))){
              rst_tile = raster::raster(act_files[[j]])
              
              trs = tinfo[[j]]$tr_non_ovlp[1] - tinfo[[j]]$tr[1] + 1 
              tre = nrow(rst_tile) - (tinfo[[j]]$tr[2] - tinfo[[j]]$tr_non_ovlp[2])
              
              tcs = tinfo[[j]]$tc_non_ovlp[1] - tinfo[[j]]$tc[1] + 1
              tce = ncol(rst_tile)  - (tinfo[[j]]$tc[2] - tinfo[[j]]$tc_non_ovlp[2])
              
              rst_tile = crop(rst_tile, 
                              extent(rst_tile, trs, tre, tcs, tce))
              target[tinfo[[j]]$tr_non_ovlp[1] : tinfo[[j]]$tr_non_ovlp[2], 
                     tinfo[[j]]$tc_non_ovlp[1] : tinfo[[j]]$tc_non_ovlp[2]] = 
                rst_tile
            }
            
            if (!dir.exists(target_filepath))
              dir.create(dirname(target_filepath), recursive = TRUE)
            
            writeRaster(target, format = "GTiff",
                        filename = target_filepath,  
                        bylayer = TRUE, overwrite = FALSE)

          }
  

}
