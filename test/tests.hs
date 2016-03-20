{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Monad
import Couchbase.Raw
import qualified Data.ByteString as B
import Foreign.Ptr
import Test.Hspec


assertLcbSuccess :: Monad m => String -> LcbError -> m ()
assertLcbSuccess msg err =
  when (err /= LcbSuccess) $ error msg


defaultParams :: ConnectionParams
defaultParams =
  ConnectionParams
  { connectionString = "couchbase://localhost/default"
  , password = Nothing
  , lcbType = LcbTypeBucket
  }


withConnection :: (Lcb -> IO ()) -> IO ()
withConnection f = do
  (err, lcb) <- lcbCreate defaultParams
  assertLcbSuccess "lcbCreate() failed" err
  lcbConnect lcb >>= assertLcbSuccess "lcbConnect() failed"
  lcbWait lcb >>= assertLcbSuccess "lcbWait() failed"
  lcbGetBootstrapStatus lcb >>= assertLcbSuccess "lcbGetBootstrapStatus() failed"
  f lcb


setValue :: B.ByteString -> B.ByteString -> IO ()
setValue k v =
  withConnection $ \lcb -> do
    let cmd = LcbCmdStore LcbSet k v Nothing
    lcbStore3 lcb nullPtr cmd >>= assertLcbSuccess "lcbStore3() failed"
    lcbWait lcb >>= assertLcbSuccess "lcbWait() failed"


removeKey :: B.ByteString -> IO ()
removeKey k =
  withConnection $ \lcb -> do
    lcbRemove3 lcb nullPtr k >>= assertLcbSuccess "lcbRemove3() failed"
    lcbWait lcb >>= assertLcbSuccess "lcbWait() failed"


main :: IO ()
main = hspec $ do
  describe "connect" $
    it "to localhost" $ do
      (err, lcb) <- lcbCreate defaultParams
      err `shouldBe` LcbSuccess
      lcbConnect lcb `shouldReturn` LcbSuccess
      lcbWait lcb `shouldReturn` LcbSuccess
      lcbGetBootstrapStatus lcb `shouldReturn` LcbSuccess

  describe "lcbGet3" $ do
    it "succeeds" $ do
      setValue "key" "value"
      withConnection $ \lcb ->
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess

    it "calls callback" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \resp -> do
          lcbRespGetGetStatus resp `shouldReturn` LcbSuccess
          lcbRespGetGetValue resp `shouldReturn` "value"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

  describe "lcbRemove3" $ do
    it "succeeds" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbRemove3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "calls callback" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbInstallRemoveCallback lcb $ \resp ->
          lcbRespRemoveGetStatus resp `shouldReturn` LcbSuccess
        lcbRemove3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "actually removes key" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbRemove3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetStatus resp `shouldReturn` LcbKeyEnoent
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when there is nothing to remove" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        lcbInstallRemoveCallback lcb $ \resp ->
          lcbRespRemoveGetStatus resp `shouldReturn` LcbKeyEnoent
        lcbRemove3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

  describe "lcbStore3" $ do
    it "replaces existing key with LcbReplace" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        let cmd = LcbCmdStore LcbReplace "key" "value2" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value2"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback with LcbReplace when there is no key to replace" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        lcbInstallStoreCallback lcb $ \resp ->
          lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEnoent
        let cmd = LcbCmdStore LcbReplace "key" "value" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "replaces existing key with LcbSet" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        let cmd = LcbCmdStore LcbSet "key" "value2" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value2"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "sets value with LcbSet even when there is no existing key" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        let cmd = LcbCmdStore LcbSet "key" "value" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "adds value with LcbAdd" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        let cmd = LcbCmdStore LcbAdd "key" "value" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback with LcbAdd when there is the key in database already" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallStoreCallback lcb $ \resp ->
          lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
        let cmd = LcbCmdStore LcbAdd "key" "value2" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "calls callback" $
      withConnection $ \lcb -> do
        lcbInstallStoreCallback lcb $ \resp -> do
          lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
          lcbRespStoreGetOp resp `shouldReturn` LcbSet
        let cmd = LcbCmdStore LcbSet "key" "value" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "appends value with LcbAppend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        let cmd = LcbCmdStore LcbAppend "key" "value2" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value1value2"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "prepends value with LcbPrepend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        let cmd = LcbCmdStore LcbPrepend "key" "value2" Nothing
        lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value2value1"
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "changes CAS with LcbReplace" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` oldCas
          let cmd = LcbCmdStore LcbReplace "key" "value2" (Just oldCas)
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "changes CAS with LcbSet" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` oldCas
          let cmd = LcbCmdStore LcbSet "key" "value2" (Just oldCas)
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "changes CAS with LcbAppend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` oldCas
          let cmd = LcbCmdStore LcbAppend "key" "value2" (Just oldCas)
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "changes CAS with LcbPrepend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` oldCas
          let cmd = LcbCmdStore LcbPrepend "key" "value2" (Just oldCas)
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbReplace with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbReplace "key" "value2" (Just (oldCas - 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbSet with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbSet "key" "value2" (Just (oldCas - 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbAppend with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbAppend "key" "value2" (Just (oldCas - 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbPrepend with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbPrepend "key" "value2" (Just (oldCas - 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbReplace with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbReplace "key" "value2" (Just (oldCas + 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbSet with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbSet "key" "value2" (Just (oldCas + 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbAppend with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbAppend "key" "value2" (Just (oldCas + 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess

    it "returns error in callback when you try LcbPrepend with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          oldCas <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbKeyEexists
          let cmd = LcbCmdStore LcbPrepend "key" "value2" (Just (oldCas + 1))
          lcbStore3 lcb nullPtr cmd `shouldReturn` LcbSuccess
          lcbWait lcb `shouldReturn` LcbSuccess
        lcbGet3 lcb nullPtr "key" `shouldReturn` LcbSuccess
        lcbWait lcb `shouldReturn` LcbSuccess
