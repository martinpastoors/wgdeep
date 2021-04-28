get_onedrive <- function () {
  
  if (Sys.info()['sysname'] == 'Windows') {
    
    # set onedrive directory
    if(dir.exists(file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF'))) {
      onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF')   
    } else if(dir.exists('C:/DATA/PFA/PFA team site - PRF')) {
      onedrive <- 'C:/DATA/PFA/PFA team site - PRF'
    } else {
      stop("Onedrive directory not found")
    }
  }
  
  return(onedrive)
}

# get_onedrive()
