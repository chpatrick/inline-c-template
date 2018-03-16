{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.C.Inline.Cpp.Templates
  ( block
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
import qualified Language.C.Inline as C

import Control.Lens
import qualified Data.Data.Lens as Lens
import Language.Haskell.TH.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data PlaceholderType
  = PTBlock
  | PTExp
  | PTPure

data Placeholder = Placeholder PlaceholderType String

placeholder :: ExpQ -> String -> QuasiQuoter
placeholder ptType ptName = QuasiQuoter
  { quoteExp = \str -> [|Placeholder $(ptType) $(stringE str)|]
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where unsupported _ = fail (ptName ++ " can not be used in this context.")

block :: QuasiQuoter
block = placeholder [|PTBlock|] "block"

exp :: QuasiQuoter
exp = placeholder [|PTExp|] "exp"

pure :: QuasiQuoter
pure = placeholder [|PTPure|] "pure"

data Template = Template
  { templatePreDecs :: [ DecsQ ]
  , templateInstanceDec :: DecsQ
  }

-- TODO: find a way to resolve Haskell types from C types using the context
instantiate :: Template -> [ ( String, TypeQ ) ] -> DecsQ
instantiate temp typeParams = do
  decs <- templateInstanceDec temp
  ( inst, instType ) <- case decs of
    [ inst @(InstanceD _ _ instType _) ] -> return ( inst, instType )
    _ -> fail "The template must be a single instance declaration."
  let instTypeVars = instType ^.. typeVarsEx mempty
  unless (length instTypeVars == length typeParams) $
    fail ("Expected " ++ show (length instTypeVars) ++ " template parameter(s), got " ++ show (length typeParams))

  typeParamsHaskell <- for typeParams $ \( _, typeParam ) -> typeParam
  let typeSubstitution = M.fromList (zip instTypeVars typeParamsHaskell)

  let ctypeSubstitution = M.fromList (zip (map nameBase instTypeVars) (map fst typeParams))
  let
    substituteCTypes blockStr = case parse typeQuotes "" blockStr of
      Left err -> fail ("Failed to substitute types: " ++ show err)
      Right subBlockStr -> return subBlockStr

    typeQuotes :: Parsec Void String String
    typeQuotes = concat <$> many (try typeQuote <|> otherChar)

    typeQuote = do
      _ <- char '@'
      tyVarName <- (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_'")
      case M.lookup tyVarName ctypeSubstitution of
        Nothing -> fail ("Unknown type variable " ++ tyVarName)
        Just ctype -> return ctype

    otherChar = (:[]) <$> anyChar

    patchExpr :: Exp -> Q (Maybe Exp)
    patchExpr (ConE placeholderCons `AppE` ConE placeholderTypeName `AppE` LitE (StringL blockStr))
      | placeholderCons == 'Placeholder = do
        let quoterTypes =
              M.fromList
                [ ( 'PTBlock, C.block )
                , ( 'PTExp, C.exp )
                , ( 'PTPure, C.pure )
                ]
        quoter <- case M.lookup placeholderTypeName quoterTypes of
          Nothing -> fail "Invalid placeholder type."
          Just quoter -> return quoter
        substitutedBlockStr <- substituteCTypes blockStr
        Just <$> quoteExp quoter substitutedBlockStr
    patchExpr _ = return Nothing

  preDecs <- sequence (templatePreDecs temp)

  substInst <-
    -- find all placeholders and replace them with inline-c quoters
    rewriteMOn Lens.template patchExpr $
    -- substitute the template types everywhere
    over Lens.template (substType typeSubstitution :: Type -> Type)
    inst

  return (concat preDecs ++ [ substInst ])