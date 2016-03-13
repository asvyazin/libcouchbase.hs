{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Couchbase.Raw where


import Foreign
import Foreign.C.String
import Foreign.C.Types


#define __int128 int


#include <libcouchbase/couchbase.h>


{# context lib="couchbase" #}


{# enum lcb_type_t as LcbType {underscoreToCase} deriving (Eq, Show) #}


{# pointer *lcb_create_st as LcbCreateSt #}


{# pointer lcb_t as Lcb foreign finalizer lcb_destroy newtype #}


{# enum lcb_error_t as LcbError {underscoreToCase} deriving (Eq, Show) #}


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
  , password :: String
  , lcbType :: LcbType
  } deriving (Show)


lcbCreate :: ConnectionParams -> IO (LcbError, Lcb)
lcbCreate params =
  allocaBytes {# sizeof lcb_create_st #} $ \st -> do
    {# set lcb_create_st.version #} st 3
    withCString (connectionString params) $ \connstr -> do
      {# set lcb_create_st.v.v3.connstr #} st connstr
      withCString (password params) $ \passwd -> do
        {# set lcb_create_st.v.v3.passwd #} st passwd
        {# set lcb_create_st.v.v3.type #} st $ fromIntegral $ fromEnum $ lcbType params
        lcbCreateRaw st


{# fun lcb_connect as ^ {`Lcb'} -> `LcbError' #}


-- lcb_set_bootstrap_callback


{# fun lcb_get_bootstrap_status as ^ {`Lcb'} -> `LcbError' #}


-- lcb_install_callback3
-- lcb_get_callback3
-- lcb_strcbtype
-- lcb_get3
-- lcb_rget3
-- lcb_store3
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
