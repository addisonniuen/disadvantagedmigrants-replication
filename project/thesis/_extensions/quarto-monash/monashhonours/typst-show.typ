// Maps Pandoc metadata variables to the thesis() template function.
// Custom metadata fields (degreetype, affiliation, degrees, submitted,
// supervisor) are set at the top level of _quarto.yml.

#show: doc => thesis(
$if(title)$
  title: [$title$],
$endif$
$if(author)$
  author: [$author$],
$endif$
$if(degrees)$
  degrees: [$degrees$],
$endif$
$if(degreetype)$
  degreetype: [$degreetype$],
$endif$
$if(affiliation)$
  affiliation: [$affiliation$],
$endif$
$if(submitted)$
  submitted: [$submitted$],
$endif$
$if(supervisor)$
  supervisor: [$supervisor$],
$endif$
$if(mainfont)$
  font: ("$mainfont$",),
$endif$
$if(fontsize)$
  fontsize: $fontsize$,
$endif$
$if(linestretch)$
  linestretch: $linestretch$,
$endif$
$if(lang)$
  lang: "$lang$",
$endif$
$if(region)$
  region: "$region$",
$endif$
$if(section-numbering)$
  sectionnumbering: "$section-numbering$",
$endif$
$if(toc)$
  toc: $toc$,
$endif$
$if(toc-title)$
  toc-title: [$toc-title$],
$endif$
  toc-depth: $if(toc-depth)$$toc-depth$$else$2$endif$,
  doc,
)
