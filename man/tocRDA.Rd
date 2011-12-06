\name{tocRDA}
\alias{tocRDA}
\title{Get the table of contents of an RDA file}
\description{
 This function reads an RDA file created via the
 \code{\link[base]{save}} function and returns
 a description of all the top-level objects in that
 file. It avoids loading all of the objects
 in memory at the same time.

 The idea is to allow users look at RDA files without loading the entire
 collection of objects into the R session and to provide a framework
 that others can use to work with RDA files. The framework could be
 adapted to read specific objects.
}
\usage{
tocRDA(filename = system.file("sampleData", "m.rda", package = "RTools"), as.data.frame = TRUE)
}
\arguments{
  \item{filename}{a connection object or a file name}
  \item{as.data.frame}{a logical value indicating whether the result
    should be a data frame or a list with elements describing each object.}
}
\details{
  This is quite slow as it is written in R.
  It could be written in C, given the prototype as a guide.
}
\value{
  If \code{as.data.frame} is \code{TRUE},
  a data frame with a  row for each top-level object in the RDA
  file.
}
\references{R file format in the R Import/Export manual}
\author{Duncan Temple Lang}

\seealso{\code{\link[base]{save}}
and \code{\link[base]{load}}
}
\examples{

 tocRDA(system.file("sampleData", "lots.rda", package = "RDA"))

 els = tocRDA(system.file("sampleData", "lots.rda", package = "RDA"), FALSE)
 sapply(els, class)
}
\keyword{IO}
