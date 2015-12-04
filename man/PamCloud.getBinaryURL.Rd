\name{PamCloud.getBinaryURL}
\alias{PamCloud.getBinaryURL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PamCloud.getBinaryURL
}
\description{
download file form the pamcloud
}
\usage{
PamCloud.getBinaryURL(url = "https://pamcloud.pamgene.com/jackrabbit/repository/default/PamCloud/BioNavigator/Resources/readme.txt", credentials = login.dialog())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
    url of the file to download
}
  \item{credentials}{
    credentials, a named list as returned by login.dialog function of this package. If NULL credentials     will be omitted
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  a file
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
\code{\link{login.dialog}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (url = "https://pamcloud.pamgene.com/jackrabbit/repository/default/PamCloud/BioNavigator/Resources/readme.txt", 
    credentials = login.dialog()) 
{
    if (!is.null(credentials)) {
        aFile = getBinaryURL(url, userpwd = paste(credentials$user, 
            credentials$pw, sep = ":"), ssl.verifypeer = FALSE)
    }
    else {
        aFile = getBinaryURL(url, ssl.verifypeer = FALSE)
    }
    return(aFile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
