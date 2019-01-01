module FlacMonad where

import Control.Monad.IO.Class
import DflateTerm

class (Monad m, MonadIO m) => DFlacMonad m where
--  getDelContext :: m DelContext
  (≽) :: Principal -> Principal -> m Bool
  (≤) :: Label -> Type -> m Bool
  (⊑) :: Principal -> Principal -> m Bool
  (⊔) :: Principal -> Principal -> m Principal
  (⊓) :: Principal -> Principal -> m Principal
  clearance :: Place -> Principal -> m Bool
  getState :: m DelContext
  voice :: Principal -> m Principal


  
