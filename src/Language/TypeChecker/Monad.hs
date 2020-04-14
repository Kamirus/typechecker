{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.TypeChecker.Monad where

import Protolude

import qualified Data.DList as DL
import           Data.Text.Prettyprint.Doc
import           Lens.Micro.Platform

import Language.Type
import Language.TypeChecker.Types

type MonadCheck m =
  ( MonadError Text m
  , MonadState CheckState m
  , MonadLogger m
  )

type CheckState = (Int, Log)

newtype CheckM a = CheckM
  { runCheckM :: ExceptT Text (StateT CheckState Identity) a }
  deriving (Functor, Applicative, Monad, MonadError Text, MonadState CheckState)

runMonadCheck :: CheckM a -> (Either Text a, Log)
runMonadCheck m = (a, l)
  where
    (a, (_, l)) = runState (runExceptT $ runCheckM m') (0, Log mempty mempty)
    m' = catchError m $ \e -> logWarn (pretty e) >> throwError e

throw :: MonadError Text m => Text -> m a
throw = throwError

-- | Create a new existential variable out of a type variable
freshHv :: MonadCheck m => TypeVar -> m HatVar
freshHv tv = do
  s@(uid, _) <- get
  put $ s & _1 %~ succ
  return $ HatVar tv uid

freshHv' :: MonadCheck m => m HatVar
freshHv' = freshHv $ TypeVar "t"


-- Logger

data LogLevel
  = LogInfo
  | LogWarn

class MonadLogger m where
  logg :: LogLevel -> Doc () -> m ()

instance MonadLogger CheckM where
  logg lvl msg = case lvl of
    LogInfo -> go _info
    LogWarn -> go _warn
    where go _field = modify $ _2 . _field %~ (`DL.snoc` msg)

logInfo :: MonadLogger m => LogItem -> m ()
logInfo = logg LogInfo

logWarn :: MonadLogger m => LogItem -> m ()
logWarn = logg LogWarn

type LogItem = Doc ()

data Log = Log
  { infoLog :: DL.DList LogItem
  , warnLog :: DL.DList LogItem
  } deriving (Show)

_info :: Lens' Log (DL.DList LogItem)
_info = lens infoLog (\s a -> s { infoLog = a })

_warn :: Lens' Log (DL.DList LogItem)
_warn = lens warnLog (\s a -> s { warnLog = a })

ppLog :: Log -> Doc ()
ppLog (Log info warn) = go "INFO:" info <+> line <> go "WARN:" warn
  where go header = foldl (\acc x -> acc <+> header <+> x <+> line) mempty
