\name{spanish}
\alias{spanish}
\title{Dataset containing Census Spanish Surname List.}
\description{
A dataset containing Census Spanish Surname List. Variables are as follows:
  \itemize{
  \item surname
  \item p_whi (i.e., Pr(White | Surname)
  \item p_bla (i.e., Pr(Black | Surname)
  \item p_his (i.e., Pr(Hispanic/Latino | Surname)
  \item p_asi (i.e., Pr(Asian | Surname)
  \item p_oth (i.e., Pr(Other | Surname)
  }
Pr(Hispanic/Latino | Surname) = 1 for all surnames on the list, and remaining probabilities are set to zero.
}
\format{
A data frame with 6,057 rows and 6 variables.
}
