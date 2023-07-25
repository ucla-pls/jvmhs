{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Jvmhs.Format.JsonTH where

import Control.Monad
import Data.Foldable
import Language.Haskell.TH

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (JSONPathElement (Key), Parser)
import Data.Traversable (for)
import Debug.Trace

-- jsonspec :: QuasiQuoter
-- jsonspec =
--   QuasiQuoter
--     { quoteExp = undefined
--     , quotePat = undefined
--     , quoteType = undefined
--     , quoteDec = \xs -> do
--         let a = fst . head $ readP_to_S parses xs
--         reportError (show a)
--
--         return []
--     }

data T = TA Name | TS Name [F]
  deriving (Show, Eq)
data C = C Name [F]
  deriving (Show, Eq)
data F = F
  { fname :: String
  , facc :: Name
  , fmods :: [String]
  , fcomment :: String
  }
  deriving (Show, Eq)

makeJson :: [T] -> Q [Dec]
makeJson ts =
  fmap fold . forM ts $ \t -> do
    fold
      <$> sequence
        [ makeToJsonT t
        , makeParseJsonT t
        ]

makeParseJson :: [T] -> Q [Dec]
makeParseJson ts =
  fmap fold . forM ts $ \t -> do
    makeParseJsonT t

makeToJsonT :: T -> Q [Dec]
makeToJsonT (TA nm) = do
  let fn = mkName ("toJSON" <> nameBase nm)
  sequence
    [ sigD fn [t|$(conT nm) -> Value|]
    , funD fn [clause [] (normalB [e|toJSON|]) []]
    ]
makeToJsonT (TS nm fs) = do
  let fn = mkName ("toJSON" <> nameBase nm)
  p <- newName ("p_" <> nameBase nm)
  let es = listE (fmap (handleField p) fs)
  sequence
    [ sigD fn [t|$(conT nm) -> Value|]
    , funD
        fn
        [ clause
            [varP p]
            (normalB [e|Object $ fold $es|])
            []
        ]
    ]

makeToJson :: [T] -> Q [Dec]
makeToJson ts =
  fmap fold . forM ts $ \t -> do
    makeToJsonT t

makeParseJsonT :: T -> Q [Dec]
makeParseJsonT (TA nm) = do
  let fn = mkName ("parseJSON" <> nameBase nm)
  sequence
    [ sigD fn [t|Value -> Parser $(conT nm)|]
    , funD fn [clause [] (normalB [e|parseJSON|]) []]
    ]
makeParseJsonT (TS nm fs) = do
  let fn = mkName ("parseJSON" <> nameBase nm)
  TyConI (DataD _ _ _ _ [RecC nx _] _) <- reify nm
  let parser o = do
        (x, recs) :: ([[StmtQ]], [Q (Name, Exp)]) <-
          unzip <$> forM fs \(F n a ms _) -> do
            tpe <- reifyType a
            t <- case tpe of
              AppT _ t ->
                findParseJSON t ms
              e -> fail $ show e
            n' <- newName "n"
            pure
              (
                [ bindS (varP n') [e|maybe (fail "key not found") $(pure t) (KM.lookup $(stringE n) $(varE o)) <?> Key $(stringE n)|]
                -- , noBindS [e|traceM $ $(stringE (nameBase nm <> "/" <> n <> ": ")) <> show $(varE n') |]
                ]
              , pure (a, VarE n')
              )

        r <- newName "r"
        doE $
          concat x
            <>
            -- noBindS [e|traceM $ $(stringE $ "parse " <> nameBase nm <> " : ")|] ]
            [ bindS (varP r) [e|pure $(recConE nx recs)|]
            , -- , noBindS [e|traceM $ $(stringE $ "parsed " <> nameBase nm <> " : ") <> show $(varE r)|]
              noBindS [e|pure $(varE r)|]
            ]

  sequence
    [ sigD fn [t|Value -> Parser $(conT nm)|]
    , funD
        fn
        [ clause
            []
            ( normalB
                [e|withObject $(stringE $ nameBase nm) $ \o -> $(parser 'o)|]
            )
            []
        ]
    ]

findToJSON :: (Quote m, MonadFail m) => Type -> [String] -> m Exp
findToJSON tpe ms = case ms of
  [] -> case tpe of
    ConT x ->
      varE $ mkName ("toJSON" <> nameBase x)
    _ -> fail $ show tpe
  ["!"] ->
    varE 'toJSON
  n : ms' -> case tpe of
    AppT _ b -> do
      [e|$(varE $ mkName ("toJSON" <> n)) $(findToJSON b ms')|]
    _ ->
      forceToJSON ms
 where
  forceToJSON [] = fail "unexpected bad"
  forceToJSON ["!"] =
    varE 'toJSON
  forceToJSON [x] =
    varE $ mkName ("toJSON" <> x)
  forceToJSON (x : ms') =
    [e|$(varE $ mkName ("toJSON" <> x)) $(forceToJSON ms')|]

findParseJSON :: (Quote m, MonadFail m) => Type -> [String] -> m Exp
findParseJSON tpe ms = case ms of
  [] -> case tpe of
    ConT x ->
      varE $ mkName ("parseJSON" <> nameBase x)
    _ -> fail $ show tpe
  ["!"] ->
    varE 'parseJSON
  n : ms' -> case tpe of
    AppT _ b -> do
      [e|$(varE $ mkName ("parseJSON" <> n)) $(findParseJSON b ms')|]
    _ ->
      forceParseJSON ms
 where
  forceParseJSON = \case
    [] -> fail "unexpected bad"
    ["!"] -> varE 'parseJSON
    [x] -> varE $ mkName ("parseJSON" <> x)
    (x : ms') ->
      [e|$(varE $ mkName ("parseJSON" <> x)) $(forceParseJSON ms')|]

handleField :: Name -> F -> Q Exp
handleField p = \case
  F n' a ms _ -> do
    tpe <- reifyType a
    t <- case tpe of
      AppT _ t ->
        findToJSON t ms
      e -> fail $ show e
    [e|$(stringE n') .= $(pure t) ($(varE a) $(varE p))|]
