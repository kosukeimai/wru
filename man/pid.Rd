\name{pid}
\alias{pid}
\title{Dataset containing distribution of party registration by race.}
\description{
A dataset containing the distribution of party registration for each major racial group in the U.S., as measured by Gallup in February 2013 (http://www.gallup.com/poll/160373/democrats-racially-diverse-republicans-mostly-white.aspx). Variables are as follows:
  \itemize{
  \item party (i.e., "Dem", "Rep", or "Oth")
  \item PID (i.e., Democrat = 1, Republican = 2, Other = 0)
  \item r_pid_whi (i.e., Pr(Party | White)
  \item r_pid_bla (i.e., Pr(Party | Black)
  \item r_pid_his (i.e., Pr(Party | Hispanic/Latino)
  \item r_pid_asi (i.e., Pr(Party | Asian)
  \item r_pid_oth (i.e., Pr(Party | Other)
  }
}
\format{
A data frame with 3 rows and 7 variables.
}
