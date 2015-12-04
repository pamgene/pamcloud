\name{PamCloud.installPackage}
\alias{PamCloud.installPackage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  mCloud.installPackage
}
\description{
 install a package from the pamcloud
}
\usage{
PamCloud.installPackage(packageUrl, tmpDir = "C:\\temp")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{packageUrl}{
    url pointing to the package
}
  \item{tmpDir}{
    temporary directory to store the package zip file. If it does not exist the current wd will be attempted.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns no value
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
function (packageUrl, tmpDir = "C:\\temp") 
{
    credentials = login.dialog()
    if (!PamCloud.checkAccess(credentials)) {
        stop("PamCloud access denied or not available")
    }
    print("...")
    aFile = PamCloud.getBinaryURL(packageUrl, credentials)
    if (!file.exists("C:\\temp")) {
        tmpDir = getwd()
    }
    pckFile = file.path(tmpDir, basename(packageUrl))
    writeBin(aFile, pckFile)
    install.packages(pckFile, repos = NULL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
