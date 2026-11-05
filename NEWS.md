# evidr 0.1.0

* First version: a local Shiny app that shows a reviewer the code and output changes of an AI-drafted R script,
  checks it against an independent implementation, and records every decision in a hash-chained `regulog` trail.
* Replays recompute code, data, environment and output fingerprints and mark earlier reviews stale.
* Blind drills with a committed, later revealed answer.
