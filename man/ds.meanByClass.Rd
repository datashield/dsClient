% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.meanByClass.R
\name{ds.meanByClass}
\alias{ds.meanByClass}
\title{Computes the mean and standard deviation across categories}
\usage{
ds.meanByClass(x = NULL, outvar = NULL, covar = NULL, type = "combine",
  datasources = NULL)
}
\arguments{
\item{x}{a character, the name of the dataset to get the subsets from or a text formula of the 
form 'A~B' where A is a single continuous vector and B a single factor vector}

\item{outvar}{a character vector, the names of the continuous variables}

\item{covar}{a character vector, the names of up to 3 categorical variables}

\item{type}{a character which represents the type of analysis to carry out. If \code{type} is set to 
'combine', a pooled table of results is generated. If \code{type} is set to 'split', a table of results 
is genrated for each study.}

\item{datasources}{a list of opal object(s) obtained after login in to opal servers; these objects hold 
also the data assign to R, as \code{dataframe}, from opal datasources.}
}
\value{
a table or a list of tables that hold the length of the numeric variable(s) and their mean 
and standard deviation in each subgroup (subset).
}
\description{
This function calculates the mean and SD of a continuous variable for each class 
of up to 3 categorical variables.
}
\details{
The functions splits the input dataset into subsets (one for each category) and calculates 
the mean and SD of the specified numeric variables. It is important to note that the process of 
generating the final table(s) can be time consuming particularly if the subsetting is done across 
more than one categorical variable and the run-time lengthens if the parameter 'split' is set to 
'split' as a table is then produced for each study. It is therefore advisable to run the function 
only for the studies of the user really interested in but including only those studies in the 
parameter 'datasources'.
}
\examples{
{

  # load that contains the login details
  data(logindata)
  library(opal)

  # Example 1: calculate the pooled mean proportion for LAB_HDL across GENDER categories where
  # both vectors are in a tabe structure "D"
  # login and assign LAB_HDL and GENDER to a table "D"
  opals <- datashield.login(logins=logindata,assign=TRUE, variables=list('LAB_HDL', 'GENDER'))
  ds.meanByClass(x='D$LAB_HDL~D$GENDER')
  
  # Example 2: calculate the mean proportion for LAB_HDL across GENDER categories where both
  # vectors are 'loose' (i.e. not in a table) assign both LAB_HDL and GENDER to vectors
  # not held in a table
  ds.assign("D$LAB_HDL", "ldl")
  ds.assign("D$GENDER", "sex")
  ds.meanByClass(x='ldl~sex')
  datashield.logout(opals)
  
  # Example 3: calculate the mean proportion for LAB_HDL across gender, bmi and
  # diabetes status categories login and assign all the variables stored on opal
  opals <- datashield.login(logins=logindata,assign=TRUE)
  ds.meanByClass(x='D', outvar=c('LAB_HDL','LAB_TSC'),
                 covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))

  # clear the Datashield R sessions and logout
  datashield.logout(opals)

}

}
\author{
Gaye, A.
}
\seealso{
\link{ds.subsetByClass} to subset by the classes of factor vector(s).

\link{ds.subset} to subset by complete cases (i.e. removing missing values), threshold, columns and rows.
}

