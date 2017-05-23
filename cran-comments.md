## Test environments
* local OS X install, R 3.4.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTEs:

* checking R code for possible problems ... NOTE
predict.JOUS: no visible binding for global variable ‘i’
Undefined global functions or variables:
  i

(I believe this is spurious: 'i' is a local variable in foreach.  In order to
get rid of this NOTE, I would need to make 'i' a global variable.)

