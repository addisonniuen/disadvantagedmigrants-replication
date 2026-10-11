// Monash Honours Thesis — Typst template for Quarto by Rob J Hyndman


// ── Notes under tables and figures ─────────────────────────────────────────────
// Every ::: {.notes} div in the .qmd files becomes #tablenotes[...] (via
// notes.lua), so all notes share one look: a light grey box with smaller text.
#let tablenotes(body) = block(
  width: 100%,
  fill: luma(243),
  inset: (x: 8pt, y: 6pt),
  radius: 2pt,
  above: 0.6em,
  below: 1.4em,
)[
  #set text(size: 8.5pt)
  #set par(leading: 0.5em, spacing: 0.6em, justify: true)
  #body
]

// ── Equation numbers for labelled equations ───────────────────────────────────
// Quarto numbers labelled equations with its own `equation-numbering`, which
// restarts in each chapter but omits the chapter, so "Equation (1)" would recur
// in every chapter. This definition replaces Quarto's (it comes later in the
// generated file) and matches the numbering of unlabelled equations below:
// (chapter.number) in the main text and (letter.number) in the appendix, where
// 06_conclusion.qmd sets the "thesis-appendix" state.
#let equation-numbering = (..nums) => {
  if state("thesis-appendix", false).at(here()) {
    let section = counter(heading).at(here()).at(1, default: 0)
    numbering("(A.1)", section, nums.pos().at(-1))
  } else {
    numbering("(1.1)", counter(heading).at(here()).first(), nums.pos().at(-1))
  }
}

// ── Main template function ─────────────────────────────────────────────────────
#let thesis(
  title:       none,
  author:      none,
  degrees:     none,
  degreetype:  "Bachelor of Commerce (Honours)",
  affiliation: none,
  submitted:   none,
  supervisor:  none,
  margin: (left: 2cm, right: 2cm, top: 1.5cm, bottom: 2cm),
  paper: "a4",
  lang: "en",
  region: "AU",
  // Libertinus Serif and Libertinus Math are designed as a pair, so inline and
  // display maths match the body text in weight and letter shapes. (New Computer
  // Modern looked mismatched: its text font is regular weight but Typst sets its
  // maths in the heavier "Book" weight.) Libertinus Math ships in fonts/ and is
  // found through font-paths in _quarto.yml.
  font:        "Libertinus Serif",
  fontsize:    11pt,
  heading-font: "Libertinus Serif",
  heading-weight: "bold",
  heading-style: "normal",
  linestretch: 1.5,
  sectionnumbering: "1.1",
  toc:         true,
  toc-title:   "Contents",
  toc-depth:   3,
  doc,
) = {

  // ── Colours ────────────────────────────────────────────────────────────────
  let monash-blue   = rgb("#253b72")

  // ── Document metadata ──────────────────────────────────────────────────────
  // set document(author: ...) requires a string, but author arrives as content
  // from the pandoc template, so we only set the title here.
  if title != none {
    set document(title: title)
  }

  // ── Typography ─────────────────────────────────────────────────────────────
  set text(font: font, size: fontsize, lang: lang, region: region)
  show math.equation: set text(font: "Libertinus Math")
  show raw: set text(font: "DejaVu Sans Mono", size: 0.9em)

  set par(
    justify: true,
    leading:  0.65em * linestretch,
    spacing:  1.0em  * linestretch,
  )

  // ── Heading numbering ──────────────────────────────────────────────────────
  set heading(numbering: (..nums) => {
    if nums.pos().len() <= 4 {
      numbering(sectionnumbering, ..nums)
    }
  })

  // ── Heading styles ─────────────────────────────────────────────────────────
  // Level 1 — chapter title with "Chapter N" prefix on numbered chapters
  // show heading.where(level: 1): it => {
  //  pagebreak(weak: true)
  //  v(2cm)
  //  set text(font: heading-font, fill: monash-blue)
  //  if it.numbering != none {
  //    // Numbered chapter: "Chapter N" label then title on new line
  //    block(width: 100%, below: 1.2em)[
  //      #set par(spacing: 0pt)
  //      #text(size: 0.9em, weight: "regular")[
  //        #context [Chapter #counter(heading).at(here()).first()]
  //      ]
  //      #v(1.5em)
  //      #text(size: 1.65em, weight: "bold")[#it.body]
  //    ]
  //  } else {
  //    // Unnumbered (front-matter headings: Abstract, Declaration, etc.)
  //    block(width: 100%, below: 1.2em)[
  //      #text(size: 1.65em, weight: "bold")[#it.body]
  //    ]
  //  }
  //  v(0.7cm)
  //}

  // Level 1 — section heading, "1 Introduction" style. Numbered sections
  // follow on from the previous section, as in a journal paper. Headings that
  // print without a number (Abstract, References, Appendix) start a new page.
  show heading.where(level: 1): it => {
    context {
      let num = if it.numbering != none {
        numbering(it.numbering, ..counter(heading).get())
      } else { none }
      if num == none { pagebreak(weak: true) } else { v(1.2em) }
    }
    set text(font: heading-font)
    block(width: 100%, below: 1.2em)[
      #text(size: 1.65em, weight: "bold")[
        // The number and the space after it are skipped when the numbering
        // gives nothing, as for the Appendix heading (see 07_appendix.qmd)
        #if it.numbering != none {
          context {
            let num = numbering(it.numbering, ..counter(heading).get())
            if num != none {
              num
              h(0.4em)
            }
          }
        }
        #it.body
      ]
    ]
    v(0.3cm)
  }

  // Level 2 — section
  show heading.where(level: 2): it => {
    v(2em, weak: true)
    set text(font: heading-font, size: 1.1em, weight: "bold")
    block(below: 0.5em)[#it]
    v(1.2em, weak: true)
  }

  // Level 3 — subsection
  show heading.where(level: 3): it => {
    v(1.6em, weak: true)
    set text(font: heading-font, size: 1.0em, weight: "bold")
    block(below: 0.25em)[#it.body]
    v(1em, weak: true)
  }

  // ── Table of contents styling ──────────────────────────────────────────────
  show outline: it => {
    set text(font: heading-font, size: 10.5pt)
    it
  }
  show outline.entry.where(level: 1): it => {
    v(1.4em, weak: true)
    // An entry whose numbering gives nothing (the Appendix heading, see
    // 06_conclusion.qmd) is drawn without the space for a number, so it lines
    // up with the unnumbered Abstract and References
    context {
      let el = it.element
      let blank = (
        type(el.numbering) == function
          and numbering(el.numbering, ..counter(heading).at(el.location())) == none
      )
      if blank {
        // black, like the other entries (links are otherwise coloured)
        strong(link(el.location(), text(fill: black, it.indented(none, it.inner()))))
      } else {
        strong(it)
      }
    }
  }

  // Chapter-based figure and table numbering
  set figure(numbering: (..nums) => {
    let chapter = counter(heading).at(here()).first()
    numbering("1.1", chapter, nums.pos().at(-1))
  })

  // Chapter-based equation numbering, (chapter.number), to match the tables;
  // equations restart in each chapter, so a bare (1) would repeat
  set math.equation(numbering: (..nums) => {
    let chapter = counter(heading).at(here()).first()
    numbering("(1.1)", chapter, nums.pos().at(-1))
  })

  // ── Figure captions ────────────────────────────────────────────────────────
  show figure.caption: it => {
    set text(size: 0.9em)
    align(left)[
      #strong[#it.supplement #it.counter.display(it.numbering):]
      #emph[#it.body]
    ]
  }

  // ── Tables ─────────────────────────────────────────────────────────────────
  // set table(inset: 6pt, stroke: none)
  set table(inset: (x: 5pt, y: 3.5pt), stroke: none)
  show table.cell.where(y: 0): set text(weight: "bold")
  show table.hline: set line(stroke: 0.4pt)

  // Never split a cell across pages. In regression tables each coefficient's
  // label spans its row and the standard-error row below it, so this also
  // keeps every coefficient on the same page as its standard error.
  set table.cell(breakable: false)

  // Mark where each table ends, so the page header below can tell which
  // table continues onto a page.
  show figure.where(kind: "quarto-float-tbl"): it => {
    it
    [#metadata("tbl-end") <tbl-end>]
  }

  // Significance stars (the Unicode asterisk operator, used only for stars)
  // are set as superscripts, in the tables and in the notes below them.
  show "∗": super

  // Tables keep the body's font size, but get tighter row spacing,
  // independent of the body's linestretch — this is what keeps rows compact
  // even when the surrounding prose is more loosely spaced.
  show table: set par(leading: 0.45em, spacing: 0.45em, justify: false)

  // One font size for every table. Individual tables do not set their own
  // size, so all tables in the thesis look the same.
  show table: set text(size: 9pt)

  // In regression tables that report standard errors on their own row, move
  // each standard error up to sit directly under its coefficient, so each pair
  // reads as one unit and the gap falls between pairs. A standard-error cell is
  // any cell outside the label column whose text starts with "(".
  let cell-text(c) = {
    if c == none { "" }
    else if type(c) == str { c }
    else if c.has("text") { c.text }
    else if c.has("children") { c.children.map(cell-text).join("", default: "") }
    else if c.has("body") { cell-text(c.body) }
    else if c.has("child") { cell-text(c.child) }
    else { "" }
  }
  show table.cell: it => {
    if it.x > 0 and cell-text(it.body).trim().starts-with("(") {
      move(dy: -0.45em, it)
    } else {
      it
    }
  }

  // Quarto wraps every table chunk in a figure of kind "quarto-float-tbl"
  // (not kind: table, so rules written for `table` figures never apply).
  // Typst never splits a figure across pages by default. Main-text tables are
  // short and stay whole (moving to the next page if needed); the appendix
  // switches on page breaking for its long tables at its start (see the typst
  // block at the top of 07_appendix.qmd).

  // ── Links ──────────────────────────────────────────────────────────────────
  show link: set text(fill: monash-blue)

  // ── Page layout (header / footer switch on body state) ─────────────────────
  // We manage all page numbering manually so we can freely switch between
  // roman numerals (front matter) and arabic (body) without set page changes.
  // On a page where a table that started on an earlier page continues, repeat
  // its number and title, followed by "(continued)", above the table.
  set page(
    paper: "a4",
    margin: 2.5cm,
    header: context {
      let pg = here().page()
      let figs = query(figure.where(kind: "quarto-float-tbl"))
      let ends = query(<tbl-end>)
      for (i, f) in figs.enumerate() {
        if i < ends.len() and f.location().page() < pg and ends.at(i).location().page() >= pg {
          set text(size: 0.9em)
          align(left + bottom)[
            #strong[#f.supplement #numbering(f.numbering, ..f.counter.at(f.location())):]
            #emph[#f.caption.body (continued)]
          ]
        }
      }
    },
  )

  // ── Title page ─────────────────────────────────────────────────────────────
  // Rendered as an explicit page element so its own settings don't persist.
  page(
    paper: "a4",
    margin: 2.5cm,
    numbering: none,
    header: none,
    footer: none,
  )[
    #set align(center)
    #set text(font: heading-font)

    #image("figures/monash-logo.jpg", width: 10cm)

    #v(3cm)

    #text(size: 16pt, weight: "bold")[
      Do Language Barriers Shift Bargaining Power \
      from Employers to Workers? \
      Evidence from Migrant-Owned Small Businesses
    ]

    #v(1.5cm)

    #text(size: 18pt)[#author]

    #if degrees != none {
      v(0.3cm)
      text(size: 13pt, style: "italic")[#degrees]
    }

    #v(1fr)

    #text(size: 13pt)[
      A thesis submitted for the degree of \
      #strong[#degreetype] \
      at Monash University in #submitted \
      #if affiliation != none { affiliation }
    ]

    #if supervisor != none {
      v(0.6cm)
      text(size: 11pt)[Supervised by: #supervisor]
    }
  ]

  // Reset counter so front matter starts at page i
  counter(page).update(1)

  // ── Table of contents ─────────────────────────────────────────────────────
  if toc {
    pagebreak(weak: true)
    outline(
      title: text(
        font: heading-font,
        size: 24pt,
        weight: "bold",
        toc-title,
      ),
      depth: toc-depth,
      indent: 1.5em,
    )
    pagebreak()
  }

  // ── Document body ──────────────────────────────────────────────────────────
  // Comprises front matter (index.qmd) followed by numbered chapters.
  // Front matter ends with  #thesis-body-start()  which switches pagination.
  doc
}
