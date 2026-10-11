-- notes.lua
-- Turns every ::: {.notes} div into a call to #tablenotes[...] (defined in the
-- Typst template), so the notes under every table and figure share one style:
-- a light grey box, smaller text, starting with *Notes:*.
function Div(el)
  if el.classes:includes("notes") and quarto.doc.is_format("typst") then
    local blocks = pandoc.List({ pandoc.RawBlock("typst", "#tablenotes[") })
    blocks:extend(el.content)
    blocks:insert(pandoc.RawBlock("typst", "]"))
    return blocks
  end
end
