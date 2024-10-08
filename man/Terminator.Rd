% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Terminator.R
\name{Terminator}
\alias{Terminator}
\title{Abstract Terminator Class}
\description{
Abstract \code{Terminator} class that implements the base functionality each terminator must provide.
A terminator is an object that determines when to stop the optimization.

Termination of optimization works as follows:
\itemize{
\item Evaluations in a instance are performed in batches.
\item Before each batch evaluation, the \link{Terminator} is checked, and if it is positive, we stop.
\item The optimization algorithm itself might decide not to produce any more points, or even might decide to do a smaller batch in its last evaluation.
}

Therefore the following note seems in order:
While it is definitely possible to execute a fine-grained control for termination, and for many optimization algorithms we can specify exactly when to stop, it might happen that too few or even too many evaluations are performed, especially if multiple points are evaluated in a single batch (c.f. batch size parameter of many optimization algorithms).
So it is advised to check the size of the returned archive, in particular if you are benchmarking multiple optimization algorithms.
}
\section{Technical details}{

\code{Terminator} subclasses can overwrite \code{.status()} to support progress bars via the package \CRANpkg{progressr}.
The method must return the maximum number of steps (\code{max_steps}) and the currently achieved number of steps (\code{current_steps}) as a named integer vector.
}

\seealso{
Other Terminator: 
\code{\link{mlr_terminators}},
\code{\link{mlr_terminators_clock_time}},
\code{\link{mlr_terminators_combo}},
\code{\link{mlr_terminators_evals}},
\code{\link{mlr_terminators_none}},
\code{\link{mlr_terminators_perf_reached}},
\code{\link{mlr_terminators_run_time}},
\code{\link{mlr_terminators_stagnation}},
\code{\link{mlr_terminators_stagnation_batch}},
\code{\link{mlr_terminators_stagnation_hypervolume}}
}
\concept{Terminator}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier of the object.
Used in tables, plot and text output.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{param_set}}{\link[paradox:ParamSet]{paradox::ParamSet}\cr
Set of control parameters.}

\item{\code{label}}{(\code{character(1)})\cr
Label for this object.
Can be used in tables, plot and text output instead of the ID.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.
The referenced help package can be opened via method \verb{$help()}.}

\item{\code{properties}}{(\code{character()})\cr
Set of properties of the terminator.
Must be a subset of \code{\link[=bbotk_reflections]{bbotk_reflections$terminator_properties}}.}

\item{\code{unit}}{(\code{character()})\cr
Unit of steps.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Terminator-new}{\code{Terminator$new()}}
\item \href{#method-Terminator-format}{\code{Terminator$format()}}
\item \href{#method-Terminator-print}{\code{Terminator$print()}}
\item \href{#method-Terminator-status}{\code{Terminator$status()}}
\item \href{#method-Terminator-remaining_time}{\code{Terminator$remaining_time()}}
\item \href{#method-Terminator-clone}{\code{Terminator$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Terminator-new"></a>}}
\if{latex}{\out{\hypertarget{method-Terminator-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Terminator$new(
  id,
  param_set = ps(),
  properties = character(),
  unit = "percent",
  label = NA_character_,
  man = NA_character_
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier for the new instance.}

\item{\code{param_set}}{(\link[paradox:ParamSet]{paradox::ParamSet})\cr
Set of control parameters.}

\item{\code{properties}}{(\code{character()})\cr
Set of properties of the terminator.
Must be a subset of \code{\link[=bbotk_reflections]{bbotk_reflections$terminator_properties}}.}

\item{\code{unit}}{(\code{character()})\cr
Unit of steps.}

\item{\code{label}}{(\code{character(1)})\cr
Label for this object.
Can be used in tables, plot and text output instead of the ID.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.
The referenced help package can be opened via method \verb{$help()}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Terminator-format"></a>}}
\if{latex}{\out{\hypertarget{method-Terminator-format}{}}}
\subsection{Method \code{format()}}{
Helper for print outputs.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Terminator$format(with_params = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{with_params}}{(\code{logical(1)})\cr
Add parameter values to format string.}

\item{\code{...}}{(ignored).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Terminator-print"></a>}}
\if{latex}{\out{\hypertarget{method-Terminator-print}{}}}
\subsection{Method \code{print()}}{
Printer.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Terminator$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{(ignored).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Terminator-status"></a>}}
\if{latex}{\out{\hypertarget{method-Terminator-status}{}}}
\subsection{Method \code{status()}}{
Returns how many progression steps are made (\code{current_steps}) and the
amount steps needed for termination (\code{max_steps}).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Terminator$status(archive)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{archive}}{(\link{Archive}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
named \code{integer(2)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Terminator-remaining_time"></a>}}
\if{latex}{\out{\hypertarget{method-Terminator-remaining_time}{}}}
\subsection{Method \code{remaining_time()}}{
Returns remaining runtime in seconds. If the terminator is not
time-based, the reaming runtime is \code{Inf}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Terminator$remaining_time(archive)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{archive}}{(\link{Archive}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{integer(1)}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Terminator-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Terminator-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Terminator$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
