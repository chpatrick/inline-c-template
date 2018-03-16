{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.C.Inline.Cpp.Templates
  ( BlockPlaceholder(..)
  , block

  , Template
  , template
  , instantiate
  )
 where

import Control.Applicative
import Control.Monad
import Data.Traversable
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.C.Inline as C

import Control.Lens
import qualified Data.Data.Lens as Lens
import Language.Haskell.TH.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

newtype BlockPlaceholder = BlockPlaceholder String

block :: QuasiQuoter
block = QuasiQuoter
  { quoteExp = \str -> [|BlockPlaceholder $(stringE str)|]
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where unsupported _ = fail "block can not be used in this context."

newtype Template = Template DecsQ

template :: DecsQ -> Template
template = Template

-- TODO: find a way to resolve Haskell types from C types using the context
instantiate :: Template -> [ ( String, TypeQ ) ] -> DecsQ
instantiate (Template temp) typeParams = do
  decs <- temp
  ( inst, instType ) <- case decs of
    [ inst @(InstanceD _ _ instType _) ] -> return ( inst, instType )
    _ -> fail "The template must be a single instance declaration."
  let instTypeVars = instType ^.. typeVarsEx mempty
  unless (length instTypeVars == length typeParams) $
    fail "The number of type arguments must match the number of template type parameters."

  typeParamsHaskell <- for typeParams $ \( _, typeParam ) -> typeParam
  let typeSubstitution = M.fromList (zip instTypeVars typeParamsHaskell)

  let typeSubInst = over Lens.template (substType typeSubstitution :: Type -> Type) inst

  let ctypeSubstitution = M.fromList (zip (map nameBase instTypeVars) (map fst typeParams))
  let
    substituteCTypes blockStr = case parse typeQuotes "" blockStr of
      Left err -> fail ("Failed to substitute types: " ++ show err)
      Right subBlockStr -> return subBlockStr
      where
        typeQuotes :: Parsec Void String String
        typeQuotes = concat <$> many (try typeQuote <|> otherChar)

        typeQuote = do
          _ <- string "@("
          tyVarName <- some $ notChar ')'
          _ <- string ")"
          case M.lookup tyVarName ctypeSubstitution of
            Nothing -> fail ("Unknown type variable " ++ tyVarName)
            Just ctype -> return ctype

        otherChar = (:[]) <$> anyChar

    patchExpr (AppE (ConE conName) (LitE (StringL blockStr)))
      | conName == 'BlockPlaceholder = do
        substitutedBlockStr <- substituteCTypes blockStr
        cExp <- quoteExp C.block substitutedBlockStr
        return (Just cExp)
    patchExpr _ = return Nothing

  filledInst <- rewriteMOn Lens.template patchExpr typeSubInst

  return [ filledInst ]