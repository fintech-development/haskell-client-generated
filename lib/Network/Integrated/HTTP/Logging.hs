{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module Network.Integrated.HTTP.Logging
  ( module Network.Integrated.HTTP.LoggingKatip
  ) where

import Network.Integrated.HTTP.LoggingKatip

#else

module Network.Integrated.HTTP.Logging
  ( module Network.Integrated.HTTP.LoggingMonadLogger
  ) where

import Network.Integrated.HTTP.LoggingMonadLogger

#endif
