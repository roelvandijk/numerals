Changelog for numerals
======================


0.4
---

- Added language entries for all supported languages.
- Support API changes in `numerals-base` (inflection).
- Updated languages.
  + Fixed bug in Turkish (TR).
  + Fixed bug in Norwegian (NO).
  + Dutch (NL) ordinals, plural & dative cardinals, partitives and multiplicatives.
  + English (EN) ordinals.
  + German (DE) ordinals.
  + French (FR) ordinals & inflection.
  + Italian (IT) ordinals & inflection.
  + Large Norwegian (NO) cardinals.
  + Spanish (ES) inflection.
  + Portuguese (PT) ordinals & inflection.
- New languages.
  + Cherokee (CHR) cardinals.
  + Hebrew (HE) cardinals.
  + Polish (PL) cardinals.
  + Swiss German (GSW) cardinals.
  + Afrikaans (AF) cardinals & ordinals.
  + Czech (CS) cardinals.
  + Friulan (FUR) cardinals.
  + Pennsylvania German (PDC) cardinals.
  + Klallam (CLM) cardinals.
- Added numerical bounds to all language modules. All numbers within the bounds
  can be converted by the cardinal & ordinal functions.
- Merged numerals-base into numerals.
  + Added inflection (case, gender and number).
  + Added fractions to the numeral expression language.
  + Renamed `Text.Numeral.Exp` to `Text.Numeral.Exp.Reified`.
  + Renamed `Text.Numeral.Exp.Classes` to `Text.Numeral.Exp`.
  + Fast integral logarithm (if GHC ≥ 7.2.1).
  + Render combining functions now have access to the expressions being
    combined.
  + BigNum postfix names are now a function of their context.


0.3.0.1
-------

*Thu Sep 15 20:36:42 UTC 2011*

- Relaxed upper base dependency to < 4.5 in order to support GHC-7.2.


0.3
---

*Thu Sep 15 18:30:52 UTC 2011*

- Removed `specialise` flag.
- New interface for all language modules.
- Added a test suite.
- Wrote some documentation.
- Changed package structure.
  + Moved a number of modules to a new package `numerals-base`.
- Updated many languages.
  + Test suite for every language.
  + Implemented large numbers (≥ million) in many languages.
  + Latin (LA)
    - Added overcounting using `Subtract` expression (18 = 20 - 2).
- New languages
  + Alamblak (AMP)
  + Chinese (ZH)
  + Chinook Wawa (CHN)
  + Malagasy (MG)
  + Manx (GV)
  + Ndom (NQM)
  + Ojibwe (OJ)
  + Russian (RU)
  + Scots (SCO)
  + Turkish (TR)
  + Wolof (WO)
  + Yoruba (YOR)
- Temporarly removed Huli.


0.1
---

*Sun Apr 19 18:38:54 UTC 2009*

- First release.
- Supports cardinal numbers in DE, EN, EO, FR, IT, JA, LA, NL, NO, PT, SP and
  SV.
