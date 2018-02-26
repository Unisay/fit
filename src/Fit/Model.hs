{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Fit.Model
  ( CommandLine(..)
  , Command(..)
  , Described(..)
  , Suggestion(..)
  , suggest
  , commandL
  , subCommandL
  , flagsL
  , emptyCommandLine
  , renderCommandLine
  ) where

import           Fit.Config
import           Fit.Lens            (customLensRules)
import           Lens.Micro.Platform
import           Protolude

type Name = Text
type Description = Text

class Described a where
  name :: a -> Name
  description :: a -> Description


data Command = Command Name Description
  deriving (Eq, Show)

instance Described Command where
  name (Command n _) = n
  description (Command _ d) = d


data Flag = Flag Name Description
  deriving (Eq, Show)

instance Described Flag where
  name (Flag n _) = n
  description (Flag _ d) = d


data SubCommand = SubCommand Name Description
  deriving (Eq, Show)

instance Described SubCommand where
  name (SubCommand n _) = n
  description (SubCommand _ d) = d


data Suggestion =
  FlagSuggestion Flag
  | CommandSuggestion Command
  | SubCommandSuggestion SubCommand
  deriving (Eq, Show)

instance Described Suggestion where
  name (FlagSuggestion f)       = name f
  name (CommandSuggestion c)    = name c
  name (SubCommandSuggestion s) = name s
  description (FlagSuggestion f)       = description f
  description (CommandSuggestion c)    = description c
  description (SubCommandSuggestion s) = description s


suggest :: FitConfig -> CommandLine -> [Suggestion]
suggest (FitConfig cs) (CommandLine Nothing _ _) =
  [CommandSuggestion $ Command n d | (CommandConfig n d) <- cs]
suggest (FitConfig _) (CommandLine (Just _) _ _) =
  [FlagSuggestion (Flag "-a" "--all")
  ,FlagSuggestion (Flag "-v" "--verbose")
  ]


data CommandLine = CommandLine
  { _command    :: Maybe Command
  , _subCommand :: Maybe SubCommand
  , _flags      :: [Flag]
  } deriving (Show, Eq)

emptyCommandLine :: CommandLine
emptyCommandLine =
  CommandLine
  { _command = Nothing
  , _subCommand = Nothing
  , _flags = []
  }

renderCommandLine :: CommandLine -> Text
renderCommandLine cl = maybe "" name (_command cl)

makeLensesWith customLensRules ''CommandLine
