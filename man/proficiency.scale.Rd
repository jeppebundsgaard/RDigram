% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proficiency.scale.R
\name{proficiency.scale}
\alias{proficiency.scale}
\title{Create a proficiency scale table}
\usage{
proficiency.scale(
  mod = NULL,
  dontplot = F,
  font.base.size = 10/.pt,
  nlevels = 4,
  level.width = 1.2,
  level1.base = -2.4,
  level.lims = level1.base + (0:nlevels) * level.width,
  color.palette = "BluGrn",
  level.names = paste("Level", 1:nlevels),
  level.descriptions = paste("Level description", 1:nlevels),
  item.params = NULL,
  item.descriptions = NULL,
  item.names.include = F,
  grid.color = "gray",
  level.rect.width = 1,
  level.rect.fill = hcl.colors(4, palette = color.palette)[2],
  level.rect.color = "white",
  level.name.family = "serif",
  level.name.size = font.base.size * 1.1,
  level.name.fontface = "bold",
  level.name.color = "white",
  level.name.hjust = "left",
  level.name.nudge_x = 0.02,
  level.name.nudge_y = 0.1,
  level.description.family = "serif",
  level.description.size = font.base.size,
  level.description.fontface = "plain",
  level.description.color = "white",
  level.description.nudge_x = 0.02,
  level.description.nudge_y = 0.2,
  level.description.linelength = 30,
  level.description.hjust = "left",
  items.include.categories = NULL,
  items.rect.width = 2,
  items.rect.fill = hcl.colors(4, palette = color.palette)[3],
  items.rect.color = "white",
  item.description.seperator = "\\n",
  item.description.bullet = "* ",
  items.description.family = "serif",
  items.description.size = font.base.size,
  items.description.fontface = "plain",
  item.description.color = "white",
  items.description.nudge_x = 0.02,
  items.description.nudge_y = 0.1,
  items.description.linelength = 30,
  items.description.hjust = "left",
  ngroups = nlevels,
  person.dist.type = c("in.levels", "none"),
  person.probability.0 = hcl.colors(4, palette = color.palette)[4],
  person.probability.100 = hcl.colors(4, palette = color.palette)[1],
  person.width = level.width,
  person1.base = level1.base,
  person.lims = person1.base + (0:ngroups) * person.width,
  person.rect.width = 0.2,
  person.rect.color = "white",
  person.family = "serif",
  person.size = font.base.size,
  person.fontface = "plain",
  person.color = "white",
  person.nudge_x = person.rect.width/10,
  person.nudge_y = 0.1,
  person.hjust = "center",
  header.level = "Levels",
  header.item = "Items",
  header.persons = paste("Group", 1:ngroups),
  header.show.percentage.in.group = T,
  header.heigth = 0.2,
  header.rect.color = "white",
  header.rect.fill = "lightgray",
  header.hjust = "center",
  header.nudge_x = 0,
  header.nudge_y = 0.05,
  header.family = "serif",
  header.size = font.base.size,
  header.fontface = "bold",
  header.color = "black",
  extra.verbose = F
)
}
\arguments{
\item{mod}{A tam.mml or eRm object.}

\item{dontplot}{Boolean. If TRUE, the proficiency table is not plotted.}

\item{font.base.size}{Size of base text in milimeters. From ggplot-specs: This is unusual, but makes the size of text consistent with the size of lines and points. Typically you specify font size using points (or pt for short), where 1 pt = 0.35mm. ggplot2 provides this conversion factor in the variable .pt, so if you want to draw 12pt text, set size = 12 / .pt.}

\item{nlevels}{Number of levels in the proficiency table.}

\item{level.width}{Width levels in logits}

\item{level1.base}{Lower limit of 1st level in logits}

\item{level.lims}{A vector of level limits (lower limit, middle levels ..., higher limit) (if you want equal sized levels, use level.width and level1.base)}

\item{color.palette}{hcl-palette to use as colorscheme. Get all available names: \code{\link[=hcl.pals]{hcl.pals()}}. See five colors from all palettes here: https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/}

\item{level.names}{A vector of names of levels}

\item{level.descriptions}{A vector of level descriptions}

\item{item.params}{A matrix of item parameters (for automatic item descriptions)}

\item{item.descriptions}{A matrix of item descriptions (if you don't provide them, they will be produced from item.params or mod)}

\item{item.names.include}{Boolean: Include item names before the description.}

\item{level.rect.width}{Width of the level descriptions. Width are relative to other widths.}

\item{level.rect.fill}{See header}

\item{level.rect.color}{See header}

\item{level.name.family}{See header}

\item{level.name.size}{See header}

\item{level.name.fontface}{See header}

\item{level.name.color}{See header}

\item{level.name.hjust}{See header}

\item{level.name.nudge_x}{See header}

\item{level.name.nudge_y}{See header}

\item{level.description.family}{See header}

\item{level.description.size}{See header}

\item{level.description.fontface}{See header}

\item{level.description.color}{See header}

\item{level.description.nudge_x}{See header}

\item{level.description.nudge_y}{See header}

\item{level.description.linelength}{Length of description in characters before new line.}

\item{level.description.hjust}{See header}

\item{items.include.categories}{An integer indicating how many categories to include. Set to NULL if all categories should be included.}

\item{items.rect.width}{Width of the item descriptions. Width are relative to other widths.}

\item{items.rect.fill}{See header}

\item{items.rect.color}{See header}

\item{item.description.seperator}{Character(s) to insert between item descriptions (\\n is newline)}

\item{item.description.bullet}{Character(s) to insert before item descriptions}

\item{items.description.family}{See header}

\item{items.description.size}{See header}

\item{items.description.fontface}{See header}

\item{item.description.color}{See header}

\item{items.description.nudge_x}{See header}

\item{items.description.nudge_y}{See header}

\item{items.description.linelength}{Length of description in characters before new line.}

\item{items.description.hjust}{See header}

\item{ngroups}{Number of person groups}

\item{person.dist.type}{Type of person group. At the moment just: "in.levels" or "none"}

\item{person.probability.0}{Color of 0 percent probability of correct answer in person groups}

\item{person.probability.100}{Color of 100 percent probability of correct answer in person groups}

\item{person.width}{Width person groups in logits}

\item{person1.base}{Lower limit of 1st person group in logits}

\item{person.lims}{A vector of person limits (lower limit, middle levels ..., higher limit) (if you want equal sized levels, use person.width and person1.base)}

\item{person.rect.width}{Width of person groups. Width are relative to other widths.}

\item{person.rect.color}{See header}

\item{person.family}{See header}

\item{person.size}{See header}

\item{person.fontface}{See header}

\item{person.color}{See header}

\item{person.nudge_x}{See header}

\item{person.nudge_y}{See header}

\item{person.hjust}{See header}

\item{header.level}{Levels header text}

\item{header.item}{Items header text}

\item{header.persons}{Person group header texts (vector of a text for each group)}

\item{header.show.percentage.in.group}{Boolean. Show percentage of persons in each group.}

\item{header.heigth}{Height of the header in "logits" relative to level limits.}

\item{header.rect.color}{Color of header border}

\item{header.rect.fill}{Fill of the header}

\item{header.hjust}{Justification of the header text}

\item{header.nudge_x}{Nudging of the header text on the x-axis}

\item{header.nudge_y}{Nudging of the header text on the y-axis}

\item{header.family}{Font family of the header text}

\item{header.size}{Size of the header text}

\item{header.fontface}{Font face (bold, normal, italic) of the header text}

\item{header.color}{Font color of the header text}

\item{extra.verbose}{Print warnings in PDF and HTML-output}
}
\value{
Returns a ggplot object.
}
\description{
Create a proficiency scale table
}
\examples{
mod<-digram.estimate(DHP)
proficiency.scale(mod)
}
