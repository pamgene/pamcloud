\name{login.dialog}
\alias{login.dialog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
login.dialog
}
\description{
prompts for user name and password
}
\usage{
login.dialog()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a named list list(user = aUserName, pw = aPassWord)
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
function () 
{
    credentials = list(user = "PamCloud User Name", pw = "pass")
    onok = function() {
        tkcget
        tkdestroy(tt)
    }
    tt = tktoplevel()
    label.user = tklabel(tt, text = "User Name")
    userVal = tclVar(init = credentials$user)
    entry.user = tkentry(tt, textvariable = userVal)
    label.pw = tklabel(tt, text = "Password")
    pwVal = tclVar(init = credentials$pw)
    entry.pw = tkentry(tt, textvariable = pwVal, show = "*")
    button.ok = tkbutton(tt, text = "OK", command = onok)
    tkpack(label.user)
    tkpack(entry.user)
    tkpack(label.pw)
    tkpack(entry.pw)
    tkpack(button.ok)
    tkfocus(tt)
    tkwait.window(tt)
    credentials$user = tclvalue(userVal)
    credentials$pw = tclvalue(pwVal)
    return(credentials)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
