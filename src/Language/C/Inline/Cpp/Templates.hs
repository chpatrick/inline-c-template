{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Language.C.Inline.Cpp.Templates
  ( Language.C.Inline.Cpp.Templates.block
  , Language.C.Inline.Cpp.Templates.exp
  , Language.C.Inline.Cpp.Templates.pure

  , Template(..)
  , instantiate
  , instantiateExp
  )
 where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Data
import qualified Data.Map as M
import qualified Data.Set as S

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

data QuoteType = QTBlock | QTExp

data Placeholder = Placeholder Safety C.Purity QuoteType String

placeholder :: TExpQ Safety -> TExpQ C.Purity -> TExpQ QuoteType -> String -> QuasiQuoter
placeholder safety purity quoteT pName = QuasiQuoter
  { quoteExp = \str -> unTypeQ [||Placeholder $$(safety) $$(purity) $$(quoteT) $$(tStringE str)||]
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
    unsupported _ = fail (pName ++ " can only be used as an expression quoter.")
    tStringE = unsafeTExpCoerce @String . stringE

block :: QuasiQuoter
block = placeholder [||Safe||] [||C.IO||] [||QTBlock||] "block"

exp :: QuasiQuoter
exp = placeholder [||Safe||] [||C.IO||] [||QTExp||] "exp"

pure :: QuasiQuoter
pure = placeholder [||Safe||] [||C.Pure||] [||QTExp||] "pure"

data Template = Template
  { templatePreDecs :: [ DecsQ ]
  , templateInstanceDec :: DecsQ
  }

data TypeArg = TypeArg
  { typeArgVar :: String
  , typeArgHaskellType :: TypeQ
  , typeArgCType :: String
  }

instantiateAst :: Data a => a -> [ TypeArg ] -> Q a
instantiateAst ast typeArgs = do
  let astTypeVars = S.toList $ S.fromList (ast ^.. Lens.template . typeVars @Type)

  let nameBaseCounts = M.fromListWith (\_ _ -> True) $ do
        typeVar <- astTypeVars
        return ( nameBase typeVar, False )

  for_ (M.toList nameBaseCounts) $ \( varNameBase, overlapping ) -> do
    when overlapping $
      fail ("Type variable `" ++ varNameBase ++ "`is ambiguous due to shadowing.")

  let typeArgsByName = M.fromList $ map (\arg -> ( typeArgVar arg, arg )) typeArgs

  typeParamsHaskell <- traverse typeArgHaskellType typeArgsByName
  let typeSubstitution = M.fromList $ do
        typeVar <- astTypeVars
        newHaskellType <- typeParamsHaskell ^.. ix (nameBase typeVar)
        return ( typeVar, newHaskellType )

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
                case M.lookup tyVarName typeArgsByName of
                  Nothing -> fail ("Unknown type variable " ++ tyVarName)
                  Just typeArg -> (typeArgCType typeArg ++) <$> typeQuote
              else (someChar:) <$> typeQuote

      try escapeAt <|> quote <|> ([] <$ eof)

    patchExpr :: Exp -> Q (Maybe Exp)
    patchExpr (ConE placeholderCons `AppE` ConE placeholderSafety `AppE` ConE placeholderPurity `AppE` ConE placeholderType `AppE` LitE (StringL blockStr))
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
        let mkExp
              | placeholderType == 'QTBlock = C.inlineItems
              | placeholderType == 'QTExp = C.inlineExp
              | otherwise = error "Unexpected QuoteType"
        Just <$> quoteExp (C.genericQuote purity (mkExp safety)) substitutedBlockStr
    patchExpr _ = return Nothing

  -- find all placeholders and replace them with inline-c quoters
  rewriteMOn Lens.template patchExpr $
    -- substitute the template types everywhere
    over Lens.template (substType typeSubstitution :: Type -> Type)
    ast

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

  preDecs <- sequence (templatePreDecs temp)
  let substitutions = zipWith (\tyVar ( ctype, hsType ) -> TypeArg (nameBase tyVar) hsType ctype) instTypeVars typeParams
  substInst <- instantiateAst inst substitutions

  return (concat preDecs ++ [ substInst ])

instantiateExp :: ExpQ -> [ TypeArg ] -> ExpQ
instantiateExp expq typeArgs = do
  expr <- expq
  instantiateAst expr typeArgs
