{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Synchronous.Enumerator
-- Copyright   :  (c) 2009–2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Iteratee enumerators for endpoints.
--
--------------------------------------------------------------------------------

module System.USB.IO.Synchronous.Enumerator
    ( enumReadBulk
    , enumReadInterrupt
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude               ( fromIntegral )
import Data.Function         ( ($) )
import Data.Word             ( Word8 )
import Data.Maybe            ( Maybe(Nothing, Just) )
import Control.Monad         ( return )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( castPtr )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )

#if __GLASGOW_HASKELL__ < 701
import Prelude               ( fromInteger )
import Control.Monad         ( (>>=), fail )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡), (≢) )
import Data.Bool.Unicode     ( (∧) )

-- from bindings-libusb:
import Bindings.Libusb ( c'libusb_bulk_transfer, c'libusb_interrupt_transfer
                       , c'LIBUSB_SUCCESS, c'LIBUSB_ERROR_TIMEOUT
                       )

-- from transformers:
import Control.Monad.IO.Class ( liftIO )

-- from monad-peel:
import Control.Monad.IO.Peel ( MonadPeelIO, liftIOOp )

-- from usb:
import System.USB.DeviceHandling ( DeviceHandle )
import System.USB.Descriptors    ( EndpointAddress )
import System.USB.IO.Synchronous ( Timeout, Size )

import System.USB.Unsafe ( C'TransferFunc
                         , getDevHndlPtr
                         , marshalEndpointAddress
                         , convertUSBException
                         )

#ifdef __HADDOCK__
import System.USB.Descriptors    ( maxPacketSize, endpointMaxPacketSize )
#endif

#if MIN_VERSION_iteratee(0,4,0)
-- from iteratee:
import Data.Iteratee.Base               ( Stream(EOF, Chunk), runIter, idoneM )
import Data.Iteratee.Iteratee           ( Enumerator, throwErr )
import Data.Iteratee.Base.ReadableChunk ( ReadableChunk(readFromPtr) )
import Data.NullPoint                   ( NullPoint(empty) )

-- from base:
import Control.Exception ( toException )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
#else
-- from iteratee:
import Data.Iteratee.Base ( EnumeratorGM
                          , StreamG(Chunk)
                          , IterGV(Done, Cont)
                          , runIter
                          , enumErr
                          , throwErr
                          )
import Data.Iteratee.Base.StreamChunk ( ReadableChunk(readFromPtr) )

-- from base:
import Text.Show ( show )
#endif


--------------------------------------------------------------------------------
-- Enumerators
--------------------------------------------------------------------------------
#if MIN_VERSION_iteratee(0,4,0)
enumReadBulk ∷ (ReadableChunk s Word8, NullPoint s, MonadPeelIO m)
             ⇒ DeviceHandle    -- ^ A handle for the device to communicate with.
             → EndpointAddress -- ^ The address of a valid 'In' and 'Bulk'
                               --   endpoint to communicate with. Make sure the
                               --   endpoint belongs to the current alternate
                               --   setting of a claimed interface which belongs
                               --   to the device.
             → Size            -- ^ Chunk size. A good value for this would be
                               --   the @'maxPacketSize' . 'endpointMaxPacketSize'@.
             → Timeout         -- ^ Timeout (in milliseconds) that this function
                               --   should wait for each chunk before giving up
                               --   due to no response being received.  For no
                               --   timeout, use value 0.
             → Enumerator s m α
enumReadBulk = enumRead c'libusb_bulk_transfer

enumReadInterrupt ∷ (ReadableChunk s Word8, NullPoint s, MonadPeelIO m)
                  ⇒ DeviceHandle    -- ^ A handle for the device to communicate
                                    --   with.
                  → EndpointAddress -- ^ The address of a valid 'In' and
                                    --   'Interrupt' endpoint to communicate
                                    --   with. Make sure the endpoint belongs to
                                    --   the current alternate setting of a
                                    --   claimed interface which belongs to the
                                    --   device.
                  → Size            -- ^ Chunk size. A good value for this would
                                    --   be the @'maxPacketSize' . 'endpointMaxPacketSize'@.
                  → Timeout         -- ^ Timeout (in milliseconds) that this
                                    --   function should wait for each chunk
                                    --   before giving up due to no response
                                    --   being received.  For no timeout, use
                                    --   value 0.
                  → Enumerator s m α
enumReadInterrupt = enumRead c'libusb_interrupt_transfer


--------------------------------------------------------------------------------

enumRead ∷ (ReadableChunk s Word8, NullPoint s, MonadPeelIO m)
         ⇒ C'TransferFunc → ( DeviceHandle
                            → EndpointAddress
                            → Size
                            → Timeout
                            → Enumerator s m α
                            )
enumRead c'transfer = \devHndl
                       endpoint
                       chunkSize
                       timeout → \iter →
    liftIOOp alloca $ \transferredPtr →
     liftIOOp  (allocaBytes chunkSize) $ \dataPtr →
        let loop i = runIter i idoneM on_cont
            on_cont _ (Just e) = return $ throwErr e
            on_cont k Nothing  = do
              err ← liftIO $ c'transfer (getDevHndlPtr devHndl)
                                        (marshalEndpointAddress endpoint)
                                        (castPtr dataPtr)
                                        (fromIntegral chunkSize)
                                        transferredPtr
                                        (fromIntegral timeout)
              if err ≢ c'LIBUSB_SUCCESS ∧
                 err ≢ c'LIBUSB_ERROR_TIMEOUT
                then return ∘ k $ EOF $ Just $ toException $ convertUSBException err
                else do
                  t ← liftIO $ peek transferredPtr
                  if t ≡ 0
                    then return ∘ k $ Chunk empty
                    else do
                      s ← liftIO ∘ readFromPtr dataPtr $ fromIntegral t
                      loop ∘ k $ Chunk s
        in loop iter


--------------------------------------------------------------------------------
#else
enumReadBulk ∷ (ReadableChunk s Word8, MonadPeelIO m)
             ⇒ DeviceHandle    -- ^ A handle for the device to communicate with.
             → EndpointAddress -- ^ The address of a valid 'In' and 'Bulk'
                               --   endpoint to communicate with. Make sure the
                               --   endpoint belongs to the current alternate
                               --   setting of a claimed interface which belongs
                               --   to the device.
             → Size            -- ^ Chunk size. A good value for this would be
                               --   the @'maxPacketSize' . 'endpointMaxPacketSize'@.
             → Timeout         -- ^ Timeout (in milliseconds) that this function
                               --   should wait for each chunk before giving up
                               --   due to no response being received.  For no
                               --   timeout, use value 0.
             → EnumeratorGM s Word8 m α
enumReadBulk = enumRead c'libusb_bulk_transfer

enumReadInterrupt ∷ (ReadableChunk s Word8, MonadPeelIO m)
                  ⇒ DeviceHandle    -- ^ A handle for the device to communicate
                                    --   with.
                  → EndpointAddress -- ^ The address of a valid 'In' and
                                    --   'Interrupt' endpoint to communicate
                                    --   with. Make sure the endpoint belongs to
                                    --   the current alternate setting of a
                                    --   claimed interface which belongs to the
                                    --   device.
                  → Size            -- ^ Chunk size. A good value for this would
                                    --   be the @'maxPacketSize' . 'endpointMaxPacketSize'@.
                  → Timeout         -- ^ Timeout (in milliseconds) that this
                                    --   function should wait for each chunk
                                    --   before giving up due to no response
                                    --   being received.  For no timeout, use
                                    --   value 0.
                  → EnumeratorGM s Word8 m α
enumReadInterrupt = enumRead c'libusb_interrupt_transfer


--------------------------------------------------------------------------------

enumRead ∷ (ReadableChunk s Word8, MonadPeelIO m)
         ⇒ C'TransferFunc → ( DeviceHandle
                            → EndpointAddress
                            → Size
                            → Timeout
                            → EnumeratorGM s Word8 m α
                            )
enumRead c'transfer = \devHndl
                       endpoint
                       chunkSize
                       timeout → \iter →
    liftIOOp alloca $ \transferredPtr →
      liftIOOp (allocaBytes chunkSize) $ \dataPtr →
        let loop i1 = do
              err ← liftIO $ c'transfer (getDevHndlPtr devHndl)
                                        (marshalEndpointAddress endpoint)
                                        (castPtr dataPtr)
                                        (fromIntegral chunkSize)
                                        transferredPtr
                                        (fromIntegral timeout)
              if err ≢ c'LIBUSB_SUCCESS ∧
                 err ≢ c'LIBUSB_ERROR_TIMEOUT
                then enumErr (show $ convertUSBException err) i1
                else do
                  t ← liftIO $ peek transferredPtr
                  if t ≡ 0
                    then return i1
                    else do
                      s ← liftIO $ readFromPtr dataPtr $ fromIntegral t
                      r ← runIter i1 $ Chunk s
                      case r of
                        Done x _        → return $ return x
                        Cont i2 Nothing → loop i2
                        Cont _ (Just e) → return $ throwErr e
        in loop iter
#endif


-- The End ---------------------------------------------------------------------
