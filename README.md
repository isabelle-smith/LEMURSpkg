
to use:  
```
# detach(package:LEMURSpkg, unload=TRUE)       ##        <<  use as needed  >>
# remove.packages("LEMURSpkg")                 ##        <<  use as needed  >>
## . . .
if ( !("LEMURSpkg" %in% installed.packages()) ) { remotes::install_github("isabelle-smith/LEMURSpkg", quiet=TRUE, upgrade="always") }
library(LEMURSpkg)
```
