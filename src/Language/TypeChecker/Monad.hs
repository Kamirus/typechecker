{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
module Language.TypeChecker.Monad where

import Protolude

import qualified Data.DList as DL
import           Data.Text.Prettyprint.Doc
import           Lens.Micro.Platform

import Language.Type
import Language.TypeChecker.Types

type MonadCheck m =
  ( MonadError Text m
  , MonadState Int m
  , MonadLogger m
  )

newtype CheckM a = CheckM
  { runCheckM :: ExceptT Text (StateT (Int, Log) Identity) a }
  deriving (Functor, Applicative, Monad, MonadError Text, MonadState (Int, Log))

runMonadCheck :: CheckM a -> Either Text a
runMonadCheck m = evalState (runExceptT $ runCheckM m) (0, Log mempty mempty)

throw :: MonadError Text m => Text -> m a
throw = throwError

-- | Create a new existential variable out of a type variable
freshHv :: MonadCheck m => TypeVar -> m HatVar
freshHv tv = do
  uid <- get
  put $ 1 + uid
  return $ HatVar tv uid

freshHv' :: MonadCheck m => m HatVar
freshHv' = freshHv $ TypeVar "t"


-- Logger

data LogLevel
  = LogInfo
  | LogWarn

class MonadLogger m where
  logg :: LogLevel -> Text -> m ()

instance MonadLogger CheckM where
  logg lvl msg = case lvl of
    LogInfo -> go _info
    LogWarn -> go _warn
    where go _field = modify $ _2 . _field %~ (`DL.snoc` msg)

logInfo :: MonadLogger m => Text -> m ()
logInfo = logg LogInfo

logWarn :: MonadLogger m => Text -> m ()
logWarn = logg LogWarn

data Log = Log
  { infoLog :: DL.DList Text
  , warnLog :: DL.DList Text
  } deriving (Show)
_info :: Lens' Log (DL.DList Text)
_info = lens infoLog (\s a -> s { infoLog = a })
_warn :: Lens' Log (DL.DList Text)
_warn = lens warnLog (\s a -> s { warnLog = a })

instance Pretty Log where
  pretty (Log info warn) = go "INFO:" info <+> line <+> go "WARN:" warn
    where
    go header = foldl (\acc x -> acc <+> header <+> pretty x <+> line) mempty
