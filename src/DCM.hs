{-# LANGUAGE RecordWildCards #-}
module DCM (dcm50MHz) where

import Language.Netlist.AST

-- | Use 50MHz DCM to replace clock signal
dcm50MHz :: Ident -> Module -> Module
dcm50MHz newClock Module{..} = Module name inputs outputs [] decls
  where
    name = module_name
    inputs = (rawClock, Nothing) : filter ((/= newClock) . fst) module_inputs
    outputs = module_outputs
    decls = routing : dcmInst : module_decls

    rawClock = "CLK_32MHZ"

    routing = NetDecl newClock Nothing Nothing

    dcmInst = InstDecl "work.dcm_32_to_50p35" "inst_dcm_32_to_50p35" []
              [ ("clkin_in",        ExprVar rawClock)
              , ("clkin_ibufg_out", open)
              ]
              [ ("clkfx_out",       ExprVar newClock)
              , ("clk0_out",        open)
              ]

    open = ExprVar "open"
