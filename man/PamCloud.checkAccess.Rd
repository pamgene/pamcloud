\name{PamCloud.checkAccess}
\alias{PamCloud.checkAccess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PamCloud.checkAccess
}
\description{
  Make a quick check on pamcloud access
}
\usage{
PamCloud.checkAccess(credentials = login.dialog())
}

\arguments{
  \item{credentials}{
    named list containing credentials, as returned by the login.dialog function of this package
}
}
\details{
  will attempt to download the dft URL of PamCLoud.getBinaryURL and check the contents.
}
\value{
 returns TRUE (if access) or FALSE (if not)
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
\code{\link{login.dialog}}
\code{\link{PamCloud.getBinaryURL}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (credentials = login.dialog()) 
{
    aFile = PamCloud.getBinaryURL(credentials = credentials)
    return(substring(rawToChar(aFile), 1, 33) == "<PamCloud Bionavigator Resources>")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
