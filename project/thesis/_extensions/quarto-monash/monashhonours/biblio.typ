// Overrides Quarto's biblio.typ partial, which appends #bibliography(...)
// after the whole document. The thesis places the bibliography itself, at the
// top of 07_appendix.qmd, so that References come before the Appendix. Keep the
// two in sync: only one #bibliography() call may appear in the document.
