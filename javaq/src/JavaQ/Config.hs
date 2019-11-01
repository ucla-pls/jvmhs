{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ViewPatterns              #-}
{-|
Module      : JavaQ.Config
Description : The config data structure
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

The config data structure
-}
module JavaQ.Config where

-- base
import           Data.Foldable
import           System.Environment

-- filepath
import           System.FilePath

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- optparse-applicative
import           Options.Applicative

-- text
import qualified Data.Text                    as Text

-- ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as D

-- jvmhs
import           Jvmhs

-- javaq
import JavaQ.Command

-- | The config
data Config = Config
  { _cfgFast           :: !Bool
  , _cfgCommand        :: !Command
  , _cfgCommandConfig  :: !CommandConfig
  , _cfgComputeClosure :: !Bool
  , _cfgClassNames     :: !([ ClassName ])
  } deriving (Show)

makeLenses ''Config

instance HasCommandConfig Config where
  commandConfig = cfgCommandConfig

parseConfig :: (Maybe ClassPath) -> FilePath -> [CommandSpec] -> Parser Config
parseConfig mcp jre cmds = do

  _cfgCommandConfig <- do
    _cfgClassPath <-
      option (maybeReader (Just . splitClassPath)) $ long "cp"
      <> help "The classpath to search for classes. Defaults to $CLASSPATH."
      <> metavar "CLASSPATH"
      <> (maybe mempty value $ mcp)
      <> showDefault

    _cfgJre <-
      option str $ long "jre"
      <> helpDoc ( Just
          $ "The jre folder to read stdlib from."
          D.</> "Defaults to $JAVA_HOME/jre"
          D.</> "or $(which java)/../jre."
          D.</> "-"
        )
      <> metavar "JRE"
      <> value jre
      <> showDefault

    _cfgUseStdlib <-
      switch $ long "stdlib"
      <> help "Include the stdlib, jars from <jre>/lib and <jre>/lib/ext, on the classpath."
    return $ CommandConfig {..}

  _cfgFast <-
    switch $ long "fast"
    <> short 'f'
    <> help (
      "Do not read any attributes, including the byte code instructions. "
      <> "This can be much faster but it changes the behavior of some commands."
      )


  _cfgCommand <-
    argument (maybeReader $ findCommand cmds . Text.pack) $
    metavar "CMD"
    <> help "The command, see Commands."
    <> value cmd
    <> showDefaultWith (Text.unpack . commandName)

  _cfgComputeClosure <-
    switch $ long "closure"
    <> short 'C'
    <> help "Compute a closure over all the classes, and include them."

  _cfgClassNames <-
    many . argument str $ metavar "CLASS .."
    <> help "Classes to investigate, if none present all will be listed."

  return $ do
    let 

    Config {..}

  where
    cmdSpec = head cmds
    cmd =
      case cmdSpec of
        CommandSpec n _ fmts tpe ->
          mkCommand n (head $ fmts) tpe

findCommand :: [CommandSpec] -> Text.Text -> Maybe Command
findCommand cmds txt =
  case find (Text.isPrefixOf name . commandSpecName) cmds of
    Just (CommandSpec n _ fmts tpe) ->
      mkCommand n <$> fmt <*> pure tpe
      where
        fmt = find (Text.isPrefixOf fmtname . formatName) fmts
    Nothing -> Nothing
  where
    (name, Text.drop 1 -> fmtname) = Text.breakOn "+" txt


getConfigParser :: [CommandSpec] -> IO (Parser Config)
getConfigParser cmds = do
  mclasspath <- fmap (fmap splitClassPath) $ lookupEnv "CLASSPATH"
  jre <-
    lookupEnv "JAVA_HOME" >>= \case
      Just javaHome -> return $ javaHome </> "jre"
      Nothing -> guessJre

  return $ parseConfig mclasspath jre cmds

getConfig :: [CommandSpec] -> IO Config
getConfig cmds = do
  pconfig <- getConfigParser cmds
  execParser $
    info (pconfig <**> helper)
    ( fullDesc
    <> header "javaq"
    <> footerDoc (Just (footerFromCommands cmds))
    <> progDesc "A program that can inspect JVM ClassFiles"
    )

footerFromCommands :: [CommandSpec] -> D.Doc
footerFromCommands cmds =
  D.nest 2 $
  D.text "Commands:"
  D.<$> D.string (
  "Here is a list of all commands."
  ++ " A command can be written as the first matching prefix,"
  ++ " and the format after a '+'.")
  D.<$> D.indent 2 (
    "Examples:"
    D.<$> D.indent 2 ("'li+c' = list-classes+csv" D.<$> "'d' = decompile+json")
    )
  D.<$> D.empty
  D.<$> joinFormats cmds formatCommand

  where
    joinFormats fs fn =
      D.vcat . map ((<> D.line) . fn) $ fs

    formatCommand CommandSpec {..} =
      D.nest 2 $
        doc commandSpecName D.<> ":" D.<+> formatCommandType commandSpecFormats commandSpecType
        D.<$> D.nest 2 commandSpecDescription

    formatCommandType :: [Format a] -> CommandType a -> D.Doc
    formatCommandType fmts = \case
      Stream a _ ->
        "stream" D.<+> D.brackets (formatGranularity a) D.<+> D.tupled (map formatFormat fmts)

      Accumulator a _ _ _  ->
        "accum" D.<+> D.brackets (formatGranularity a) D.<+> D.tupled (map formatFormat fmts)

      Algorithm _ ->
        "algorithm" D.<+> D.tupled (map formatFormat fmts)

    formatGranularity :: Granularity a -> D.Doc
    formatGranularity = \case
      ClassNames -> "classnames"
      ClassFiles -> "classfiles"
      Classes -> "classes"
      Methods -> "methods"
      Fields -> "fields"

    formatFormat = doc . formatName
