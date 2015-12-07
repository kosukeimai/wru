\name{names.all}
\alias{names.all}
\title{Dataset containing Census Surname List and Spanish Surname Lists.}
\description{
A dataset containing Census Surname List, which is augmented with Census Spanish Surname List. Variables are as follows:
  \itemize{
  \item surname
  \item p_whi (i.e., Pr(White | Surname)
  \item p_bla (i.e., Pr(Black | Surname)
  \item p_his (i.e., Pr(Hispanic/Latino | Surname)
  \item p_asi (i.e., Pr(Asian | Surname)
  \item p_oth (i.e., Pr(Other | Surname)
  }
For any surnames appearing only on Spanish Surname List, Pr(Hispanic/Latino | Surname) = 1, and remaining probabilities are set to zero.
}
\format{
A data frame with 157,728 rows and 6 variables.
}
