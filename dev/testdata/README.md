# dev/testdata/

`cef_fixture.rds` - the synthetic CEF data the tests in `dev/tests/` run on:
`list(catch = <CEF catches>, dist = <CEF distributions>)`, read through
`dev/tests/fixture.R` (`fx_catch()`, `fx_dist()`).

Entirely invented. The numbers come from a random generator and mean nothing; the labels
are neutral (flags BE, DE, DK, ES, FR, IE, NL; ICES areas 27.3.a to 27.7.f; stock
`syn.27.347`; years 2020-2023). It is built so the workflow has something to do: sampling is
uneven between fleets, flags, seasons and areas (a data set where everything is sampled
raises nothing), a few strata have discards but no landings and a few the reverse, length
sampling is broader than age sampling, and some flags report numbers in thousands (`NE3`)
and others in individuals (`N`).

Two flags share a domain abbreviation on purpose (the `domainBiology` of real CEF files
carries no country, so two countries can own the same key): the tests that link catch to
distributions check that each country gets its own.
