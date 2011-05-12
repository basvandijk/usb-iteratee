{-# LANGUAGE CPP, UnicodeSyntax, NoImplicitPrelude, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.IO.Iteratee
-- Copyright   :  (c) 2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Iteratee enumerators for endpoints.
--
--------------------------------------------------------------------------------

module System.USB.IO.Iteratee
    ( enumReadBulk
    , enumReadInterrupt
#ifdef HAS_EVENT_MANAGER
    , enumReadIsochronous
#endif
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude                          ( fromIntegral )
import Data.Function                    ( ($) )
import Data.Word                        ( Word8 )
import Data.Maybe                       ( Maybe(Nothing, Just) )
import Control.Monad                    ( (>>=), return )
import Foreign.Storable                 ( peek )
import Foreign.Ptr                      ( castPtr )
import Foreign.Marshal.Alloc            ( alloca, allocaBytes )

#if __GLASGOW_HASKELL__ < 700
import Prelude                          ( fromInteger )
import Control.Monad                    ( fail )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode                  ( (≡), (≢) )
import Data.Bool.Unicode                ( (∧) )

-- from bindings-libusb:
import Bindings.Libusb                  ( c'libusb_bulk_transfer
                                        , c'libusb_interrupt_transfer
                                        , c'LIBUSB_SUCCESS
                                        , c'LIBUSB_ERROR_TIMEOUT
                                        )

-- from transformers:
import Control.Monad.IO.Class           ( liftIO )

-- from monad-control:
import Control.Monad.IO.Control         ( MonadControlIO, liftIOOp )

-- from usb:
import System.USB.DeviceHandling        ( DeviceHandle )
import System.USB.Descriptors           ( EndpointAddress )
import System.USB.IO                    ( Timeout, Size )

import System.USB.Internal              ( C'TransferFunc
                                        , getDevHndlPtr
                                        , marshalEndpointAddress
                                        , convertUSBException
                                        )

#ifdef __HADDOCK__
import System.USB.Descriptors           ( maxPacketSize, endpointMaxPacketSize
                                        , TransferDirection(In)
                                        , TransferType(Bulk, Interrupt)
                                        )
#endif

-- from iteratee:
import Data.Iteratee.Base               ( Stream(EOF, Chunk), runIter, idoneM )
import Data.Iteratee.Iteratee           ( Enumerator, throwErr )
import Data.Iteratee.Base.ReadableChunk ( ReadableChunk(readFromPtr) )
import Data.NullPoint                   ( NullPoint(empty) )

-- from base:
import Control.Exception                ( toException )

-- from base-unicode-symbols:
import Data.Function.Unicode            ( (∘) )

--------------------------------------------------------------------------------
#ifdef HAS_EVENT_MANAGER
-- from base:
import Data.Bool         ( otherwise, not )
import Data.Int          ( Int )
import Data.Function     ( id )
import Data.Functor      ( (<$) )
import Data.List         ( (++), map )
import Foreign.Ptr       ( Ptr, nullPtr, plusPtr )
import Foreign.Storable  ( poke )
import System.IO         ( IO )
import Text.Show         ( show )
import Prelude           ( (*), error, String )
import Control.Exception ( onException, mask_, uninterruptibleMask_ )
import System.Event      ( registerTimeout, unregisterTimeout )

-- from iteratee:
import Data.Iteratee.Base ( Iteratee )

-- from bindings-libusb:
import Bindings.Libusb ( c'LIBUSB_TRANSFER_TYPE_BULK
                       , c'LIBUSB_TRANSFER_TYPE_INTERRUPT
                       , c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS

                       , c'LIBUSB_TRANSFER_COMPLETED
                       , c'LIBUSB_TRANSFER_TIMED_OUT
                       , c'LIBUSB_TRANSFER_ERROR
                       , c'LIBUSB_TRANSFER_NO_DEVICE
                       , c'LIBUSB_TRANSFER_OVERFLOW
                       , c'LIBUSB_TRANSFER_STALL
                       , c'LIBUSB_TRANSFER_CANCELLED

                       , C'libusb_iso_packet_descriptor(..)
                       , C'libusb_transfer(..)

                       , c'libusb_submit_transfer
                       , c'libusb_cancel_transfer
                       )

-- from usb:
import System.USB.DeviceHandling ( getDevice )
import System.USB.IO             ( Status(Completed, TimedOut) )
import System.USB.Exceptions     ( USBException(..), ioException )
#ifdef __HADDOCK__
import System.USB.Descriptors    ( TransferType(Isochronous), maxIsoPacketSize )
#endif
import System.USB.Internal       ( threaded
                                 , C'TransferType
                                 , allocaTransfer, withCallback
                                 , newLock, acquire, release
                                 , SumLength(..), sumLength
                                 , peekIsoPacketDescs
                                 , initIsoPacketDesc
                                 , getCtx, getEventManager
                                 )
#endif

--------------------------------------------------------------------------------
-- Enumerators
--------------------------------------------------------------------------------

-- | Iteratee enumerator for reading /bulk/ endpoints.
enumReadBulk ∷ (ReadableChunk s Word8, NullPoint s, MonadControlIO m)
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
                               --   due to no response being received.
             → Enumerator s m α
enumReadBulk
#ifdef HAS_EVENT_MANAGER
    | threaded  = enumReadBulkAsync
    | otherwise = enumReadBulkSync
#else
    = enumReadBulkSync
#endif

-- | Iteratee enumerator for reading /interrupt/ endpoints.
enumReadInterrupt ∷ (ReadableChunk s Word8, NullPoint s, MonadControlIO m)
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
                                    --   being received.
                  → Enumerator s m α
enumReadInterrupt
#ifdef HAS_EVENT_MANAGER
    | threaded  = enumReadInterruptAsync
    | otherwise = enumReadInterruptSync
#else
    = enumReadInterruptSync
#endif

--------------------------------------------------------------------------------
-- Asynchronous
--------------------------------------------------------------------------------

#ifdef HAS_EVENT_MANAGER
enumReadBulkAsync, enumReadInterruptAsync ∷
    (ReadableChunk s Word8, NullPoint s, MonadControlIO m)
  ⇒ DeviceHandle → EndpointAddress → Size → Timeout → Enumerator s m α

enumReadBulkAsync      = enumReadAsync c'LIBUSB_TRANSFER_TYPE_BULK
enumReadInterruptAsync = enumReadAsync c'LIBUSB_TRANSFER_TYPE_INTERRUPT

enumReadAsync ∷ (ReadableChunk s Word8, NullPoint s, MonadControlIO m)
              ⇒ C'TransferType
              → DeviceHandle → EndpointAddress → Size → Timeout
              → Enumerator s m α
enumReadAsync transType = \devHndl endpointAddr chunkSize timeout →
  enum transType
       0 []
       devHndl endpointAddr
       timeout
       chunkSize $ \_ bufferPtr n go k _ →
         if n ≡ 0
         then return ∘ k $ Chunk empty
         else liftIO (readFromPtr bufferPtr n) >>= go ∘ k ∘ Chunk

enum ∷ MonadControlIO m
     ⇒ C'TransferType
     → Int → [C'libusb_iso_packet_descriptor]
     → DeviceHandle → EndpointAddress
     → Timeout
     → Size
     → ( Ptr C'libusb_transfer
       → Ptr Word8 → Size
       → Enumerator s m α
       → (Stream s → Iteratee s m α)
       → Status
       → m (Iteratee s m α)
       )
     → Enumerator s m α
enum transType
     nrOfIsoPackets isoPackageDescs
     devHndl endpointAddr
     timeout
     chunkSize
     convertResults = \iter → do
  liftIOOp (allocaBytes chunkSize) $ \bufferPtr →

     liftIOOp (allocaTransfer nrOfIsoPackets) $ \transPtr → do
       lock ← liftIO newLock
       liftIOOp (withCallback (\_ → release lock)) $ \cbPtr → do

         liftIO $ poke transPtr $ C'libusb_transfer
           { c'libusb_transfer'dev_handle      = getDevHndlPtr devHndl
           , c'libusb_transfer'flags           = 0 -- unused
           , c'libusb_transfer'endpoint        = marshalEndpointAddress endpointAddr
           , c'libusb_transfer'type            = transType
           , c'libusb_transfer'timeout         = fromIntegral timeout
           , c'libusb_transfer'status          = 0  -- output
           , c'libusb_transfer'length          = fromIntegral chunkSize
           , c'libusb_transfer'actual_length   = 0 -- output
           , c'libusb_transfer'callback        = cbPtr
           , c'libusb_transfer'user_data       = nullPtr -- unused
           , c'libusb_transfer'buffer          = castPtr bufferPtr
           , c'libusb_transfer'num_iso_packets = fromIntegral nrOfIsoPackets
           , c'libusb_transfer'iso_packet_desc = isoPackageDescs
           }

         let go i = runIter i idoneM on_cont

             on_cont _ (Just e) = return $ throwErr e
             on_cont k Nothing  = do
               -- Submit the transfer:
               mbE ← liftIO $ mask_ $ do
                       err ← c'libusb_submit_transfer transPtr
                       if err ≢ c'LIBUSB_SUCCESS
                         then return $ Just $ convertUSBException err
                         else Nothing <$ do
                           -- Wait for the transfer to terminate:
                           let Just (evtMgr, mbHandleEvents) = getEventManager $
                                                                 getCtx $
                                                                   getDevice devHndl
                           case mbHandleEvents of
                             Nothing → acquire lock
                                         `onException`
                                           (uninterruptibleMask_ $ do
                                              _err ← c'libusb_cancel_transfer transPtr
                                              acquire lock)
                             Just handleEvents → do
                               tk ← registerTimeout evtMgr (timeout * 1000) handleEvents
                               acquire lock
                                 `onException`
                                   (uninterruptibleMask_ $ do
                                      unregisterTimeout evtMgr tk
                                      _err ← c'libusb_cancel_transfer transPtr
                                      acquire lock)

               let ex = return ∘ k ∘ EOF ∘ Just ∘ toException

               case mbE of
                 Just e → ex e
                 Nothing → do
                   trans ← liftIO $ peek transPtr

                   let n = fromIntegral $ c'libusb_transfer'actual_length trans
                       c = convertResults transPtr bufferPtr n go k

                   case c'libusb_transfer'status trans of
                     ts | ts ≡ c'LIBUSB_TRANSFER_COMPLETED → c Completed
                        | ts ≡ c'LIBUSB_TRANSFER_TIMED_OUT → c TimedOut

                        | ts ≡ c'LIBUSB_TRANSFER_ERROR     → ex ioException
                        | ts ≡ c'LIBUSB_TRANSFER_NO_DEVICE → ex NoDeviceException
                        | ts ≡ c'LIBUSB_TRANSFER_OVERFLOW  → ex OverflowException
                        | ts ≡ c'LIBUSB_TRANSFER_STALL     → ex PipeException

                        | ts ≡ c'LIBUSB_TRANSFER_CANCELLED →
                            moduleError "transfer status can't be Cancelled!"

                        | otherwise → moduleError $ "Unknown transfer status: "
                                                      ++ show ts ++ "!"
         go iter

moduleError ∷ String → error
moduleError msg = error $ thisModule ++ ": " ++ msg

thisModule ∷ String
thisModule = "System.USB.IO.Iteratee"

needThreadedRTSError ∷ String → error
needThreadedRTSError msg = moduleError $ msg ++
  " is only supported when using the threaded runtime. " ++
  "Please build your program with -threaded."

--------------------------------------------------------------------------------
-- Isochronous
--------------------------------------------------------------------------------

-- | Iteratee enumerator for reading /isochronous/ endpoints.
--
-- /WARNING:/ You need to enable the threaded runtime (@-threaded@) for this
-- function to work correctly. It throws a runtime error otherwise!
enumReadIsochronous ∷ (ReadableChunk s Word8, MonadControlIO m)
                    ⇒ DeviceHandle    -- ^ A handle for the device to communicate with.
                    → EndpointAddress -- ^ The address of a valid 'In' and 'Isochronous'
                                      --   endpoint to communicate with. Make sure the
                                      --   endpoint belongs to the current alternate
                                      --   setting of a claimed interface which belongs
                                      --   to the device.
                    → [Size]          -- ^ Sizes of isochronous packets. A good
                                      --   value for these would be the
                                      --   'maxIsoPacketSize'
                    → Timeout         -- ^ Timeout (in milliseconds) that this
                                      --   function should wait for each chunk
                                      --   before giving up due to no response
                                      --   being received.
                    → Enumerator [s] m α
enumReadIsochronous devHndl endpointAddr sizes timeout
    | not threaded = needThreadedRTSError "enumReadIsochronous"
    | otherwise    = enum c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
                          nrOfIsoPackets (map initIsoPacketDesc sizes)
                          devHndl endpointAddr
                          timeout
                          totalSize
                          convertResults
    where
      SumLength totalSize nrOfIsoPackets = sumLength sizes

      convertResults transPtr bufferPtr _ go k Completed =
          liftIO (convertIsosToChunks nrOfIsoPackets transPtr bufferPtr)
            >>= go ∘ k ∘ Chunk

      convertResults _ _ _ _ k TimedOut =
          return ∘ k ∘ EOF ∘ Just ∘ toException $ TimeoutException

convertIsosToChunks ∷ (ReadableChunk s Word8)
                    ⇒ Int → Ptr C'libusb_transfer → Ptr Word8 → IO [s]
convertIsosToChunks nrOfIsoPackets transPtr bufferPtr =
    peekIsoPacketDescs nrOfIsoPackets transPtr >>= go bufferPtr id
      where
        go _   ss [] = return $ ss []
        go ptr ss (C'libusb_iso_packet_descriptor l a _ : ds) = do
          s ← readFromPtr ptr (fromIntegral a)
          go (ptr `plusPtr` fromIntegral l) (ss ∘ (s:)) ds
#endif

--------------------------------------------------------------------------------
-- Synchronous
--------------------------------------------------------------------------------

enumReadBulkSync, enumReadInterruptSync ∷
    (ReadableChunk s Word8, NullPoint s, MonadControlIO m)
  ⇒ DeviceHandle → EndpointAddress → Size → Timeout → Enumerator s m α
enumReadBulkSync      = enumReadSync c'libusb_bulk_transfer
enumReadInterruptSync = enumReadSync c'libusb_interrupt_transfer

enumReadSync ∷ (ReadableChunk s Word8, NullPoint s, MonadControlIO m)
             ⇒ C'TransferFunc → ( DeviceHandle
                                → EndpointAddress
                                → Size
                                → Timeout
                                → Enumerator s m α
                                )
enumReadSync c'transfer = \devHndl
                           endpoint
                           chunkSize
                           timeout → \iter →
    liftIOOp alloca $ \transferredPtr →
     liftIOOp (allocaBytes chunkSize) $ \dataPtr →
        let go i = runIter i idoneM on_cont
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
                    else liftIO (readFromPtr dataPtr $ fromIntegral t)
                           >>= go ∘ k ∘ Chunk
        in go iter
