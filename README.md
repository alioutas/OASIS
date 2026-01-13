# OASIS Shiny App (app.R)

`app.R` hosts a Shiny app that interactively appends OligoSTORM and/or OligoFISSEQ barcode bridges to Oligopaints.

## Access
- Preferred: use the web app at https://oasis.lioutas.net
- Local: fork or download this repo, go to the folder containing `app.R`, and run the Shiny app from there (R console or RStudio/Posit).

## Dependencies
- R packages: run `Rscript requirements.R` to install the needed CRAN packages and the GitHub-only `RBedtools` helper:
```sh
Rscript requirements.R
```
- System: `RBedtools` expects the `bedtools` command-line tool to be available in your `PATH`.

## Quick tour
- Step 1 — Upload: choose a default Oligopaint library by genome or upload your own intersected/coordinate `.bed` files; optional filters for off-target score, k-mer count, repeat masking, and advanced density settings.
- Step 2 — Barcodes: pick organism-safe barcodes, choose the scheme (Sequential OligoSTORM, OligoSTORM, OligoFISSEQ, custom), decide which streets (universals, Mainstreet 1|2, Backstreet 1|2) get which barcodes, toggle reverse complements, and use advanced tables to avoid or swap specific barcodes.
- Step 3 — Download: export appended Oligopaints tables plus the report, order file, bridge/toe sequences, and amplification primers as a zip.

## What it produces
- Appended Oligopaint libraries. 
- A comprehensive summary report.
- Order-ready text files for Oligopaints libraries, primers, and bridge/toe barcode sequences.

## Resources and walkthroughs
- Video tutorial: https://youtu.be/-OBmgxL7BNo
- Helper popovers draw from `help_files/*.md` (upload instructions, k-mer/off-target notes, etc.).
- Extra tools linked in the footer: reverse-complement helper (https://alioutas.github.io/revcomp_shinylive/), previously used barcodes (https://wulab.connect.hms.harvard.edu/barcodes_used/), OligoMiner (https://github.com/beliveau-lab/OligoMiner), PaintSHOP (https://paintshop.io/), and reference papers for OligoSTORM (https://doi.org/10.1371/journal.pgen.1007872; https://doi.org/10.1126/science.aau1783) and OligoFISSEQ (https://rdcu.be/c5yF9).

## Run it locally
- Install dependencies by running `Rscript requirements.R`.
- From the project root, open R and run:
```r
shiny::runApp("app.R")
```
- Or open the project in RStudio/Posit and click “Run App.”
- The UI walks you through upload → barcode choices → download; refresh the page to reset.
