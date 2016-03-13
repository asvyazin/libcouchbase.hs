{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Couchbase.Raw where


import qualified Data.ByteString as B
import Foreign
import Foreign.C.String
import Foreign.C.Types


#define __int128 int


#include <libcouchbase/couchbase.h>


{# context lib="libcouchbase" #}


{# enum lcb_type_t as LcbType {underscoreToCase} deriving (Eq, Show) #}


{# pointer *lcb_create_st as LcbCreateSt #}


{# pointer lcb_t as Lcb foreign finalizer lcb_destroy newtype #}


{# enum lcb_error_t as LcbError {underscoreToCase} deriving (Eq, Show) #}


{# fun lcb_get_errtype as ^ {`LcbError'} -> `Int' #}


{# fun lcb_strerror as ^ {`Lcb', `LcbError'} -> `String' #}


-- lcb_errmap_default
-- lcb_set_errmap_callback


newLcb :: Ptr Lcb -> IO Lcb
newLcb ptr =
  Lcb <$> newForeignPtr lcb_destroy ptr


peekLcb :: Ptr (Ptr Lcb) -> IO Lcb
peekLcb ptr =
  peek ptr >>= newLcb


{# fun lcb_create as lcbCreateRaw {alloca- `Lcb' peekLcb*, `LcbCreateSt'} -> `LcbError' #}


data ConnectionParams =
  ConnectionParams
  { connectionString :: String
  , password :: Maybe String
  , lcbType :: LcbType
  } deriving (Show)


lcbCreate :: ConnectionParams -> IO (LcbError, Lcb)
lcbCreate params = do
  allocaBytes {# sizeof lcb_create_st #} $ \st -> do
    fillBytes st 0 {# sizeof lcb_create_st #}
    {# set lcb_create_st.version #} st 3
    {# set lcb_create_st.v.v3.type #} st $ fromIntegral $ fromEnum $ lcbType params
    withCString (connectionString params) $ \connstr -> do
      {# set lcb_create_st.v.v3.connstr #} st connstr
      case password params of
        Just pwd ->
          withCString pwd $ \passwd -> do
            {# set lcb_create_st.v.v3.passwd #} st passwd
            lcbCreateRaw st
        Nothing ->
          lcbCreateRaw st


{# fun lcb_connect as ^ {`Lcb'} -> `LcbError' #}


-- lcb_set_bootstrap_callback


{# fun lcb_get_bootstrap_status as ^ {`Lcb'} -> `LcbError' #}


{# enum lcb_CALLBACKTYPE as LcbCallbackType {underscoreToCase} deriving (Eq, Show) #}


type LcbCallbackRaw =
  Ptr Lcb -> CInt -> Ptr () -> IO ()


foreign import ccall "wrapper"
  mkLcbCallbackFunPtr :: LcbCallbackRaw -> IO (FunPtr LcbCallbackRaw)


type LcbCallback =
  LcbCallbackType -> Ptr () -> IO ()


withLcbCallback :: LcbCallback -> (FunPtr LcbCallbackRaw -> IO a) -> IO a
withLcbCallback callback f =
  mkLcbCallbackFunPtr (\ _ cb p -> callback (toEnum (fromIntegral cb)) p) >>= f


type OldCallbackPtr = FunPtr LcbCallbackRaw


{# fun lcb_install_callback3 as ^ {`Lcb', `LcbCallbackType', withLcbCallback* `LcbCallback'} -> `OldCallbackPtr' id #}


-- lcb_get_callback3
-- lcb_strcbtype
-- lcb_get3
-- lcb_rget3


{# enum lcb_KVBUFTYPE as LcbKvBufType {underscoreToCase} deriving (Eq, Show) #}


_lcbKreqSimple :: Ptr () -> B.ByteString -> IO ()
_lcbKreqSimple p bs =
  B.useAsCStringLen bs $ \(pv, len) -> do
    {# set lcb_KEYBUF.type #} p $ fromIntegral $ fromEnum LcbKvCopy
    {# set lcb_KEYBUF.contig.bytes #} p $ castPtr pv
    {# set lcb_KEYBUF.contig.nbytes #} p $ fromIntegral len


_lcbCmdStoreSetKey :: Ptr () -> B.ByteString -> IO ()
_lcbCmdStoreSetKey p bs =
  _lcbKreqSimple (plusPtr p {# offsetof lcb_CMDSTORE.key #}) bs


_lcbCmdStoreSetValue :: Ptr () -> B.ByteString -> IO ()
_lcbCmdStoreSetValue p bs =
  B.useAsCStringLen bs $ \(pv, len) -> do
    {# set lcb_CMDSTORE.value.vtype #} p $ fromIntegral $ fromEnum LcbKvCopy
    {# set lcb_CMDSTORE.value.u_buf.contig.bytes #} p $ castPtr pv
    {# set lcb_CMDSTORE.value.u_buf.contig.nbytes #} p $ fromIntegral len


{# enum lcb_storage_t as LcbStorage {underscoreToCase} deriving (Eq, Show) #}


data LcbCmdStore =
  LcbCmdStore
  { operation :: LcbStorage
  , key :: B.ByteString
  , value :: B.ByteString
  } deriving (Show)


{# fun lcb_store3 as lcbStore3Raw {`Lcb', `Ptr ()', `Ptr ()'} -> `LcbError' #}


type LcbCookie = Ptr ()


lcbStore3 :: Lcb -> LcbCookie -> LcbCmdStore -> IO LcbError
lcbStore3 lcb cookie cmd =
  allocaBytes {# sizeof lcb_CMDSTORE #} $ \st -> do
    fillBytes st 0 {# sizeof lcb_CMDSTORE #}
    {# set lcb_CMDSTORE.operation #} st $ fromIntegral $ fromEnum $ operation cmd
    _lcbCmdStoreSetKey st $ key cmd
    _lcbCmdStoreSetValue st $ value cmd
    lcbStore3Raw lcb cookie st


{# pointer *lcb_RESPSTORE as LcbRespStorePtr #}


lcbRespStoreGetOp :: LcbRespStorePtr -> IO LcbStorage
lcbRespStoreGetOp p = (toEnum . fromIntegral) <$> {# get lcb_RESPSTORE.op #} p


_lcbRespBaseGetCookie :: Ptr () -> IO LcbCookie
_lcbRespBaseGetCookie p = {# get lcb_RESPBASE.cookie #} p


lcbRespStoreGetCookie :: LcbRespStorePtr -> IO LcbCookie
lcbRespStoreGetCookie = _lcbRespBaseGetCookie . castPtr


_lcbRespBaseGetKey :: Ptr () -> IO B.ByteString
_lcbRespBaseGetKey p = do
  pv <- {# get lcb_RESPBASE.key #} p
  len <- {# get lcb_RESPBASE.nkey #} p
  B.packCStringLen (castPtr pv, fromIntegral len)


lcbRespStoreGetKey :: LcbRespStorePtr -> IO B.ByteString
lcbRespStoreGetKey = _lcbRespBaseGetKey . castPtr


_lcbRespBaseGetStatus :: Ptr () -> IO LcbError
_lcbRespBaseGetStatus p = (toEnum . fromIntegral) <$> {# get lcb_RESPBASE.rc #} p


lcbRespStoreGetStatus :: LcbRespStorePtr -> IO LcbError
lcbRespStoreGetStatus = _lcbRespBaseGetStatus . castPtr


type LcbCas = {# type lcb_CAS #}


_lcbRespBaseGetCas :: Ptr () -> IO LcbCas
_lcbRespBaseGetCas p = {# get lcb_RESPBASE.cas #} p


lcbRespStoreGetCas :: LcbRespStorePtr -> IO LcbCas
lcbRespStoreGetCas = _lcbRespBaseGetCas . castPtr


type LcbRespStoreCallback =
  LcbRespStorePtr -> IO ()


lcbInstallRespStoreCallback :: Lcb -> LcbRespStoreCallback -> IO OldCallbackPtr
lcbInstallRespStoreCallback lcb callback =
  lcbInstallCallback3 lcb LcbCallbackStore $ \ _ p -> callback (castPtr p)


-- lcb_remove3
-- lcb_endure3_ctxnew
-- lcb_storedur3
-- lcb_durability_validate
-- lcb_observe3_ctxnew
-- lcb_observe_seqno3
-- lcb_resp_get_mutation_token
-- lcb_get_mutation_token
-- lcb_counter3
-- lcb_unlock3
-- lcb_touch3
-- lcb_stats3
-- lcb_server_versions3
-- lcb_server_verbosity3
-- lcb_cbflush3
-- lcb_flush3
-- lcb_http3
-- lcb_cancel_http_request
-- lcb_set_cookie
-- lcb_get_cookie


{# fun lcb_wait as ^ {`Lcb'} -> `LcbError' #}


-- lcb_tick_nowait


{# enum lcb_WAITFLAGS as LcbWaitFlags {underscoreToCase} deriving (Eq, Show) #}


{# fun lcb_wait3 as ^ {`Lcb', `LcbWaitFlags'} -> `()' #}


-- lcb_breakout
-- lcb_is_waiting
-- lcb_refresh_config
-- lcb_sched_enter
-- lcb_sched_leave
-- lcb_sched_fail
-- lcb_sched_flush


{# fun lcb_destroy as ^ {`Lcb'} -> `()' #}


-- lcb_set_destroy_callback
-- lcb_destroy_async
-- lcb_get_node
-- lcb_get_keynode
-- lcb_get_num_replicas
-- lcb_get_num_nodes
-- lcb_get_server_list
-- lcb_dump
-- lcb_cntl
-- lcb_cntl_string
-- lcb_cntl_setu32
-- lcb_cntl_getu32
-- lcb_cntl_exists
-- lcb_enable_timings
-- lcb_disable_timings
-- lcb_get_timings
-- lcb_get_version
-- lcb_supports_feature
-- lcb_mem_alloc
-- lcb_mem_free
-- lcb_run_loop
-- lcb_stop_loop
-- lcb_nstime
-- lcb_histogram_create
-- lcb_histogram_destroy
-- lcb_histogram_record
-- lcb_histogram_read
-- lcb_histogram_print
-- lcb_subdoc3
-- lcb_sdresult_next
