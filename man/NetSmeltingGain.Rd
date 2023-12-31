% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NetSmeltingGain.R
\name{NetSmeltingGain}
\alias{NetSmeltingGain}
\alias{net_smelting_gain}
\title{Net Smelting Gain}
\usage{
net_smelting_gain(total_smelt, self_smelt, craft_consumption = 1)
}
\arguments{
\item{total_smelt}{\code{numeric(1)}. The total number of items an item can
smelt. May be a fractional part.}

\item{self_smelt}{\code{numeric(1)}. The number of items of the same type that
must be spent turn the item into fuel.}

\item{craft_consumption}{\code{numeric(1)}. The number of the items used in
crafting a more complex item for fuel.}
}
\description{
The Net Smelting Gain (NSG) is a measure of the number of
items an individual fuel source item can smelt after accounting for the
number of its own items it must consume in order to turn itself into a fuel
source.

As an example, kelp may be used as a fuel source after it has
been smelted and crafted into a dried kelp block. A dried kelp block
is able to smelt twenty items (\code{total_smelt = 20}), requires 9 items to
be smelted to become fuel (\code{self_smelt = 9}), and 9 items are consumed
in the crafting of a dried kelp block (\code{craft_consumption = 9}). This
results in a net smelting gain of 1.22 items per kelp.

Bamboo is able to smelt 0.25 items (\code{total_smelt = 0.25}), requires zero
items be smelted to become fuel (\code{self_smelt = 0}), and one item is
consumed in making bamboo into fuel (itself, \code{craft_consumption = 1}).
This results in a net smelting gain of 0.25 items per bamboo.
}
