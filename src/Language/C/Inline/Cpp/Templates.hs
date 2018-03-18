{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Language.C.Inline.Cpp.Templates
  ( Language.C.Inline.Cpp.Templates.block
  , Language.C.Inline.Cpp.Templates.exp
  , Language.C.Inline.Cpp.Templates.pure

  , Template(..)
  , instantiate
  )
 where

import Control.Applicative
import Control.Monad
import Data.Traversable
import qualified Data.Map as M

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import qualified Language.C.Inline.Internal as C
import qualified Language.C.Inline.Context as C

import Control.Lens
import qualified Data.Data.Lens as Lens
import Language.Haskell.TH.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Placeholder = Placeholder Safety C.Purity String

placeholder :: TExpQ Safety -> TExpQ C.Purity -> String -> QuasiQuoter
placeholder safety purity pName = QuasiQuoter
  { quoteExp = \str -> unTypeQ [||Placeholder $$(safety) $$(purity) $$(tStringE str)||]
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
    unsupported _ = fail (pName ++ " can only be used as an expression quoter.")
    tStringE = unsafeTExpCoerce @String . stringE

block :: QuasiQuoter
block = placeholder [||Safe||] [||C.IO||] "block"

exp :: QuasiQuoter
exp = placeholder [||Safe||] [||C.IO||] "exp"

pure :: QuasiQuoter
pure = placeholder [||Safe||] [||C.Pure||] "pure"

data Template = Template
  { templatePreDecs :: [ DecsQ ]
  , templateInstanceDec :: DecsQ
  }

-- TODO: find a way to resolve Haskell types from C types using the context
instantiate :: Template -> [ ( String, TypeQ ) ] -> DecsQ
instantiate temp typeParams = do
  decs <- templateInstanceDec temp
  ( inst, instType ) <- case decs of
    [ inst@(InstanceD _ _ instType _) ] -> return ( inst, instType )
    _ -> fail "The template must be a single instance declaration."
  let instTypeVars = instType ^.. typeVarsEx mempty
  unless (length instTypeVars == length typeParams) $
    fail ("Expected " ++ show (length instTypeVars) ++ " template parameter(s), got " ++ show (length typeParams))

  typeParamsHaskell <- for typeParams $ \( _, typeParam ) -> typeParam
  let typeSubstitution = M.fromList (zip instTypeVars typeParamsHaskell)

  let ctypeSubstitution = M.fromList (zip (map nameBase instTypeVars) (map fst typeParams))
  let
    substituteCTypes blockStr = case parse typeQuote "" blockStr of
      Left err -> fail ("Failed to substitute types: " ++ show err)
      Right subBlockStr -> return subBlockStr

    typeQuote :: Parsec Void String String
    typeQuote = do
      let escapeAt = char '\\' *> ((:) <$> char '@' <*> typeQuote)
      let quote = do
            someChar <- anyChar
            if someChar == '@'
              then do
                tyVarName <- (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_'")
                case M.lookup tyVarName ctypeSubstitution of
                  Nothing -> fail ("Unknown type variable " ++ tyVarName)
                  Just ctype -> (ctype ++) <$> typeQuote
              else (someChar:) <$> typeQuote

      try escapeAt <|> quote <|> ([] <$ eof)

    patchExpr :: Exp -> Q (Maybe Exp)
    patchExpr (ConE placeholderCons `AppE` ConE placeholderSafety `AppE` ConE placeholderPurity `AppE` LitE (StringL blockStr))
      | placeholderCons == 'Placeholder = do
        let safety
              | placeholderSafety == 'Unsafe = Unsafe
              | placeholderSafety == 'Safe = Safe
              | placeholderSafety == 'Interruptible = Interruptible
              | otherwise = error "Unexpected Safety"
        let purity
              | placeholderPurity == 'C.IO = C.IO
              | placeholderPurity == 'C.Pure = C.Pure
              | otherwise = error "Unexpected Purity"
        substitutedBlockStr <- substituteCTypes blockStr
        Just <$> quoteExp (C.genericQuote purity (C.inlineExp safety)) substitutedBlockStr
    patchExpr _ = return Nothing

  preDecs <- sequence (templatePreDecs temp)

  substInst <-
    -- find all placeholders and replace them with inline-c quoters
    rewriteMOn Lens.template patchExpr $
    -- substitute the template types everywhere
    over Lens.template (substType typeSubstitution :: Type -> Type)
    inst

  return (concat preDecs ++ [ substInst ])