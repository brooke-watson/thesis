#' tidyembase
#'
#' This function encodes a season variable based on dates of data collection in the study.
#' @param filepath path to an exported list of citations from embase. filepath must include search strings.
#' @return cleaned data frame 
#' @keywords excel, csv, embase 
#' @import janitor, readxl, dplyr
#' @examples
#' tidyembase('path/to/file.csv')
#' tidyembase('path/to/file.xlsx') 

tidyembase = function(filepath){ 
    install.packages("dplyr", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"), dependencies = TRUE)
    install.packages("janitor", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"), dependencies = TRUE)
    install.packages("readxl", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"), dependencies = TRUE)
    library(janitor)
    library(readxl)
    
    # read in the data 
    if (grepl("xl",filepath)){
        db <- read_excel(filepath, col_names = FALSE)
    } else if (grepl('csv', filepath)){
        dbraw <- read.csv(filepath, stringsAsFactors = FALSE, header = FALSE, fill = FALSE,
                       blank.lines.skip = TRUE)
        db <- dbraw[c(1:2, 4:dim(dbraw)[1]),]
    } else{ stop("filepath does not lead to an xls, xlsx, or csv file.", call. = FALSE)
    }    
        if(grepl("search", db[1,1], ignore.case = TRUE)){ 
            
            attr(db, "strings") = db[1,2] # assign search strings as meta data 
            colnames(db, do.NULL = FALSE)
            colnames(db) = db[3,]  # assign headings as column names 
            db = db[c(4:dim(db)[1]),] #subset, remove blanks  
            rownames(db) = NULL
            return(db)
        }  else{ stop("search strings not included in the given filepath.", call. = FALSE)
        
    }
}     