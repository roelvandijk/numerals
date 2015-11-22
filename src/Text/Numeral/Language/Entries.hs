{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Text.Numeral.Language.Entries ( entries ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "this" Text.Numeral.Entry ( Entry(..) )
import qualified "this" Text.Numeral.Language.AFR as AFR
import qualified "this" Text.Numeral.Language.AMP as AMP
import qualified "this" Text.Numeral.Language.BUL as BUL
import qualified "this" Text.Numeral.Language.CES as CES
import qualified "this" Text.Numeral.Language.CHN as CHN
import qualified "this" Text.Numeral.Language.CHR as CHR
import qualified "this" Text.Numeral.Language.CLM as CLM
import qualified "this" Text.Numeral.Language.DEU as DEU
import qualified "this" Text.Numeral.Language.ENG as ENG
import qualified "this" Text.Numeral.Language.EPO as EPO
import qualified "this" Text.Numeral.Language.FIN as FIN
import qualified "this" Text.Numeral.Language.FRA as FRA
import qualified "this" Text.Numeral.Language.FUR as FUR
import qualified "this" Text.Numeral.Language.GLV as GLV
import qualified "this" Text.Numeral.Language.GSW as GSW
import qualified "this" Text.Numeral.Language.HEB as HEB
import qualified "this" Text.Numeral.Language.HOP as HOP
import qualified "this" Text.Numeral.Language.ITA as ITA
import qualified "this" Text.Numeral.Language.JPN as JPN
import qualified "this" Text.Numeral.Language.LAT as LAT
import qualified "this" Text.Numeral.Language.LLD as LLD
import qualified "this" Text.Numeral.Language.MLG as MLG
import qualified "this" Text.Numeral.Language.NEN as NEN
import qualified "this" Text.Numeral.Language.NLD as NLD
import qualified "this" Text.Numeral.Language.NOB as NOB
import qualified "this" Text.Numeral.Language.NQM as NQM
import qualified "this" Text.Numeral.Language.OJI as OJI
import qualified "this" Text.Numeral.Language.PDC as PDC
import qualified "this" Text.Numeral.Language.POL as POL
import qualified "this" Text.Numeral.Language.POR as POR
import qualified "this" Text.Numeral.Language.RUS as RUS
import qualified "this" Text.Numeral.Language.SCO as SCO
import qualified "this" Text.Numeral.Language.SPA as SPA
import qualified "this" Text.Numeral.Language.SWE as SWE
import qualified "this" Text.Numeral.Language.TUR as TUR
import qualified "this" Text.Numeral.Language.WOL as WOL
import qualified "this" Text.Numeral.Language.YOR as YOR
import qualified "this" Text.Numeral.Language.ZHO as ZHO


--------------------------------------------------------------------------------
-- Language entries
--------------------------------------------------------------------------------

entries :: [Entry]
entries =
  [ AFR.entry
  , AMP.entry
  , BUL.entry
  , CHN.entry
  , CHR.entry
  , CLM.entry
  , CES.entry
  , DEU.entry
  , ENG.gb_entry
  , ENG.us_entry
  , EPO.entry
  , SPA.entry
  , FIN.entry
  , FRA.entry
  , FUR.entry
  , GSW.entry
  , GLV.entry
  , HEB.entry
  , HOP.entry
  , ITA.entry
  , JPN.daiji_entry
  , JPN.kanji_entry
  , JPN.on'yomi_entry
  , JPN.preferred_entry
  , LAT.entry
  , LLD.entry
  , MLG.entry
  , NEN.entry
  , NLD.entry
  , NOB.entry
  , NQM.entry
  , OJI.entry
  , PDC.entry
  , POL.entry
  , POR.entry
  , RUS.entry
  , SCO.entry
  , SWE.entry
  , TUR.entry
  , WOL.entry
  , YOR.entry
  , ZHO.finance_simpl_entry
  , ZHO.finance_trad_entry
  , ZHO.pinyin_entry
  , ZHO.simpl_entry
  , ZHO.trad_entry
  ]
