\name{tidyembase}
\alias{tidyembase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tidyembase(filepath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
%%     ~~Describe \code{filepath} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filepath) 
{
    require(janitor)
    require(readxl)
    if (grepl("xl", filepath)) {
        db <- read_excel(filepath, col_names = FALSE)
    }
    else if (grepl("csv", filepath)) {
        dbraw <- read.csv(filepath, stringsAsFactors = FALSE, 
            header = FALSE, fill = FALSE, blank.lines.skip = TRUE)
        db <- dbraw[c(1:2, 4:dim(dbraw)[1]), ]
    }
    else {
        stop("filepath does not lead to an xls, xlsx, or csv file.", 
            call. = FALSE)
    }
    if (grepl("search", db[1, 1], ignore.case = TRUE)) {
        attr(db, "strings") = db[1, 2]
        colnames(db, do.NULL = FALSE)
        colnames(db) = db[3, ]
        db = db[c(4:dim(db)[1]), ]
        rownames(db) = NULL
        return(db)
    }
    else {
        stop("search strings not included in the given filepath.", 
            call. = FALSE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
