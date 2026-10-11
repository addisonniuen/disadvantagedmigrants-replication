# `thesis/`: Quarto source of the thesis

The thesis is a Quarto book rendered to PDF with Typst, using a modified Monash honours template. The chapters run the analysis code in `project/code/` themselves, so the numbers in the text, tables and figures always come from the same pipeline.

## Build

```bash
Rscript project/thesis/render.R
```

`render.R` sets a UTF-8 locale and renders the book to `_book/thesis.pdf`. It then copies the PDF to `project/output/thesis.pdf`. You can also run `quarto render` from this folder, with `LANG=en_AU.UTF-8 LC_ALL=en_AU.UTF-8` set.

**Requirements:**

- Quarto 1.9.36 or later. Typst is bundled with Quarto, so nothing else needs installing.
- R with the packages listed in `project/code/README.md`.
- The `quarto` R package, if you build with `render.R`.

**Caching.** `execute: freeze: auto` caches the results of each chapter in `_freeze/`. A chapter is re-executed only when its `.qmd` changes. If you change the code in `project/code/` but not the `.qmd`, delete `_freeze/<chapter>/` to force a rerun. From an empty cache, a full render takes about 10 minutes.

## Files

| File | Purpose |
|---|---|
| `_quarto.yml` | Book configuration: chapter order, bibliography, the `notes.lua` filter, the font path, and section numbering. |
| `index.qmd` | Abstract (unnumbered). |
| `01_intro.qmd` | Chapters 1 and 2, Introduction and Conceptual Framework. |
| `02_data.qmd` | Chapter 3, Data. Runs `code/01` to `03`. |
| `03_methodology.qmd` | Chapter 4, Methodology. |
| `04_results.qmd` | Chapter 5, Results. Runs `code/01` to `07` and builds Tables 5.1 to 5.5 and Figures 5.1 to 5.4. |
| `05_discussion.qmd` | Chapter 6, Discussion. |
| `06_conclusion.qmd` | Chapter 7, Conclusion. The reference list (unnumbered) is placed at the end of this file so that it comes before the appendix. A raw Typst block after it sets the appendix numbering: lettered sections (cross-referenced as "Appendix A") and tables and figures numbered within each section (Table A.1). It sits here because Quarto places raw Typst at the top of `07_appendix.qmd` after that file's heading. |
| `07_appendix.qmd` | Appendix (unnumbered), with lettered sections A to F: Tables A.1 to F.1 and Figures C.1 and C.2. A raw Typst block at the top lets appendix tables break across pages and keeps captions with their tables. |
| `thesis_format.R` | All R formatting for the thesis in one file: the figure theme and Okabe-Ito colours, number formatting, the regression-table helpers (observation and individual rows, standard errors kept under their coefficients) and the owner income and risk tables. Also sourced by `code/exhibits/`, so the exhibit files match the PDF. |
| `notes.lua` | Lua filter that turns `::: {.notes}` blocks into the grey "*Notes:*" boxes under tables and figures. |
| `thesisrefs.bib`, `apa.csl` | Bibliography and citation style (APA). |
| `render.R` | Build script (see above). |
| `_extensions/quarto-monash/monashhonours/` | Typst template. `typst-template.typ` sets the fonts (Libertinus Serif and Libertinus Math), 9pt table text, and the `#tablenotes` box. `fonts/` ships Libertinus Math and Fira Sans. Each change from the original Monash template is commented in the file. |
| `figures/monash-logo.jpg` | Logo used on the title page. |
| `_freeze/`, `_book/` | Generated: the execution cache and the rendered book. `_book/thesis.pdf` is copied to `project/output/thesis.pdf`. |

**Code for each exhibit.** The code for each table and figure is in a chunk labelled with the exhibit's cross-reference, such as `#| label: tbl-b2-main`. The same code is available as a standalone script in `project/code/exhibits/` (see `project/README.md` for the mapping).

## Fonts

- The body text uses Libertinus Serif, which is built into Typst.
- Libertinus Math ships in the template's `fonts/` folder.
- Figures are drawn with `svglite`, which maps "Libertinus Serif" to Times New Roman metrics so that text in the figures is sized correctly. Times New Roman is installed by default on macOS and Windows.
