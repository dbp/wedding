{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Data.Traversable
import Control.Monad
import Database.PostgreSQL.Simple.FromRow
import Data.Time.Clock
import Data.Maybe
import           Network.Wai.Handler.Warp          (run)
import Network.Wai (Response, pathInfo)
import Web.Fn
import qualified Web.Larceny as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Data.Monoid
import           System.Environment                (lookupEnv)
import Web.Fn.Extra.Digestive
import Text.Digestive.Larceny
import Text.Digestive.Form
import           Database.PostgreSQL.Simple        (ConnectInfo (..),
                                                    Connection, close, Only(..),
                                                    connectPostgreSQL, query, query_, execute)
import           Data.Pool                         (Pool, createPool, withResource)
import           Web.Heroku                        (parseDatabaseUrl)


data Ctxt = Ctxt { _req :: FnRequest
                 , db      :: Pool Connection
                 , library :: Library
                 }

instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

render :: Ctxt -> Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do t <- L.renderWith (library ctxt) subs () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'


initializer :: IO Ctxt
initializer =
  do lib <- L.loadTemplates "templates" L.defaultOverrides
     u <- fmap parseDatabaseUrl <$> lookupEnv "DATABASE_URL"
     let ps = fromMaybe [("host", "localhost")
                        ,("port", "5432")
                        ,("user", "wedding")
                        ,("password", "111")
                        ,("dbname", "wedding")]
              u
     pgpool <- createPool (connectPostgreSQL $ T.encodeUtf8 $ T.intercalate " " $ map (\(k,v) -> k <> "=" <> v) ps)
                        close 1 60 20

     return (Ctxt defaultFnRequest pgpool lib)

main :: IO ()
main = do
  setLocaleEncoding utf8
  ctxt <- initializer
  port <- maybe 3000 read <$> lookupEnv "PORT"
  putStrLn $ "Listening on port " <> show port <>  "..."
  run port $ toWAI ctxt site

larcenyServe :: Ctxt -> IO (Maybe Response)
larcenyServe ctxt = do
  let pth' = pathInfo (fst . getRequest $ ctxt)
  let pth = T.intercalate "/" pth'
  let idx = if T.length pth > 0 then pth <> "/index" else "index"
  if ((length pth' > 1) && "_" `T.isPrefixOf` (last pth')) || ".." `T.isInfixOf` pth
     then return Nothing
     else route ctxt [ anything ==> \ctxt -> render ctxt pth
                     , anything ==> \ctxt -> render ctxt idx
                     ]

data RsvpData = RsvpData Text Bool Bool Text [(Int, Bool)] [(Int, Text, Bool)]


data Rsvp = Rsvp { rId :: Int
                 , rK :: Text
                 , rLodging :: Maybe Text
                 , rFriday :: Maybe Bool
                 , rSaturday :: Maybe Bool
                 , rFood :: Maybe Text
                 , rConfirmedAt :: Maybe UTCTime
                 }
data Person = Person { pId :: Int
                     , pName :: Text
                     , pLocked :: Bool
                     , pRsvpId :: Int
                     , pInclude :: Bool
                     }

instance FromRow Rsvp where
  fromRow = Rsvp <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
instance FromRow Person where
  fromRow = Person <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field


joinPersons :: Connection -> Rsvp -> IO (Rsvp, [Person])
joinPersons c x = do p <- query c "select id, name, locked, rsvp_id, include from people where rsvp_id = ?" (Only (rId x))
                     return (x, p)

getRsvp :: Ctxt -> Text -> IO (Maybe (Rsvp, [Person]))
getRsvp ctxt k = withResource (db ctxt) $ \c ->
  do r <- query c "select id, k, lodging, friday, saturday, food, confirmed_at from rsvps where k = ?" (Only k)
     case r of
       [x] -> Just <$> joinPersons c x
       _ -> return Nothing

getRsvpById :: Ctxt -> Int -> IO (Maybe (Rsvp, [Person]))
getRsvpById ctxt i = withResource (db ctxt) $ \c ->
  do r <- query c "select id, k, lodging, friday, saturday, food, confirmed_at from rsvps where id = ?" (Only i)
     case r of
       [x] -> Just <$> joinPersons c x
       _ -> return Nothing


getAllRsvps :: Ctxt -> IO [(Rsvp, [Person])]
getAllRsvps ctxt = withResource (db ctxt) $ \c ->
  do rs <- query_ c "select id, k, lodging, friday, saturday, food, confirmed_at from rsvps order by id asc"
     mapM (\r -> joinPersons c r) rs

saveRsvp :: Ctxt -> Rsvp -> RsvpData -> IO ()
saveRsvp ctxt r (RsvpData l fri sat f locked unlocked) =
  withResource (db ctxt) $ \c ->
    do execute c "update rsvps set lodging = ?, friday = ?, saturday = ?, food = ?, confirmed_at = now() where id = ? and k = ?" (l, fri, sat, f, rId r, rK r)
       mapM_ (\(pid, conf) -> execute c "update people set include = ? where id = ?" (conf, pid)) locked
       mapM_ (\(pid, name, conf) -> execute c "update people set include = ?, name = ? where id = ?" (conf, name, pid)) unlocked
       return ()

personSubs :: Person -> Substitutions
personSubs p = L.subs
  [("id", L.textFill $ tshow $ pId p)
  ,("name", L.textFill $ pName p)
  ,("locked", if pLocked p then L.fillChildren else L.textFill "")
  ,("not-locked", if pLocked p then L.textFill "" else L.fillChildren)
  ,("include", if pInclude p then L.fillChildren else L.textFill "")
  ,("not-include", if pInclude p then L.textFill "" else L.fillChildren)]

rsvpSubs :: (Rsvp, [Person]) -> Substitutions
rsvpSubs (r, ps) = L.subs
  [("id", L.textFill $ tshow $ rId r)
  ,("k", L.textFill $ rK r)
  ,("lodging", L.textFill $ fromMaybe "" $ rLodging r)
  ,("confirmed", if isJust (rConfirmedAt r) then L.fillChildren else L.textFill "")
  ,("not-confirmed", if isNothing (rConfirmedAt r) then L.fillChildren else L.textFill "")
  ,("confirmed-class", L.textFill $ if isJust (rConfirmedAt r) then "confirmed" else "")
  ,("friday-checked", if rFriday r == Just True then L.fillChildren else L.textFill "")
  ,("friday-not-checked", if rFriday r /= Just True then L.fillChildren else L.textFill "")
  ,("saturday-checked", if rSaturday r == Just True then L.fillChildren else L.textFill "")
  ,("saturday-not-checked", if rSaturday r /= Just True then L.fillChildren else L.textFill "")
  ,("food", L.textFill $ fromMaybe "" $ rFood r)
  ,("people", L.mapSubs personSubs ps)
  ]

rsvpForm :: (Rsvp, [Person]) -> Form Text IO RsvpData
rsvpForm (r,ps) = RsvpData <$> "lodging" .: choice [("hlroom", "Main Building, Highland Lodge")
                                            ,("hlcabin", "Cabin, Highland Lodge")
                                            ,("off", "Arrange our own housing")] (rLodging r)
                    <*> "friday" .: bool (rFriday r `mplus` Just True)
                    <*> "saturday" .: bool (rSaturday r `mplus` Just True)
                    <*> "food" .: text (rFood r)
                    <*> sequenceA (map (\p -> (pId p, ) <$> ("person-" <> tshow (pId p) <> "-include" .: bool (Just $ pInclude p))) (filter pLocked ps))
                    <*> sequenceA (map (\p -> (pId p, , )
                                         <$> ("person-" <> tshow (pId p) <> "-name" .: text (Just $ pName p))
                                         <*> ("person-" <> tshow (pId p) <> "-include" .: bool (Just $ pInclude p))) (filter (not.pLocked) ps))


rsvpH :: Ctxt -> Text -> IO (Maybe Response)
rsvpH ctxt k' = do
  let k = T.toUpper $ T.strip k'
  r <- getRsvp ctxt k
  case r of
    Nothing -> return Nothing
    Just rsvp -> do
      runForm ctxt "rsvp" (rsvpForm rsvp) $ \(v,a) ->
        case a of
          Just dat -> do saveRsvp ctxt (fst rsvp) dat
                         redirect $ "/rsvp?k=" <> k
          Nothing -> renderWith ctxt (formFills v <> rsvpSubs rsvp) "rsvp"

rsvpDataH :: Ctxt -> Text -> IO (Maybe Response)
rsvpDataH ctxt k = if k /= "crup" then return Nothing else do
  rs <- getAllRsvps ctxt
  renderWith ctxt (L.subs [("rsvps", L.mapSubs rsvpSubs rs)]) "rsvp_data"

rsvpMergeH :: Ctxt -> Text -> [Int] -> IO (Maybe Response)
rsvpMergeH ctxt k m = if k /= "crup" then return Nothing else do
  case m of
    x:xs -> withResource (db ctxt) $ \c -> do r <- getRsvpById ctxt x
                                              case r of
                                                Nothing -> return ()
                                                Just (r,_) ->
                                                  mapM_ (\old -> do execute c "update people set rsvp_id = ? where rsvp_id = ?" (rId r, old)
                                                                    execute c "delete from rsvps where id = ?" (Only old)) xs
    _ -> return ()
  redirect "/rsvp_data?s=crup"

rsvpPersonAddH :: Ctxt -> Int -> IO (Maybe Response)
rsvpPersonAddH ctxt i = do
  withResource (db ctxt) $ \c -> execute c "insert into people (name, locked, rsvp_id) values ('Guest',false, ?)" (Only i)
  redirect "/rsvp_data?s=crup"

rsvpPersonLockH :: Ctxt -> Int -> IO (Maybe Response)
rsvpPersonLockH ctxt i = do
  withResource (db ctxt) $ \c -> execute c "update people set locked = true where id = ?" (Only i)
  redirect "/rsvp_data?s=crup"

rsvpPersonUnlockH :: Ctxt -> Int -> IO (Maybe Response)
rsvpPersonUnlockH ctxt i = do
  withResource (db ctxt) $ \c -> execute c "update people set locked = false where id = ?" (Only i)
  redirect "/rsvp_data?s=crup"

rsvpPersonDeleteH :: Ctxt -> Int -> IO (Maybe Response)
rsvpPersonDeleteH ctxt i = do
  withResource (db ctxt) $ \c -> execute c "delete from people where id = ?" (Only i)
  redirect "/rsvp_data?s=crup"


site :: Ctxt -> IO Response
site ctxt = route ctxt [ path "static" ==> staticServe "static"
                       , path "rsvp" // param "k" ==> rsvpH
                       , path "rsvp" ==> \_ -> render ctxt "rsvp_lookup"
                       , path "rsvp_data" // param "s" ==> rsvpDataH
                       , path "rsvp_data_merge" // param "s" // param "merge" ==> rsvpMergeH
                       , path "rsvp_person_add" // param "i" ==> rsvpPersonAddH
                       , path "rsvp_person_lock" // param "i" ==> rsvpPersonLockH
                       , path "rsvp_person_unlock" // param "i" ==> rsvpPersonUnlockH
                       , path "rsvp_person_delete" // param "i" ==> rsvpPersonDeleteH
                       , anything ==> larcenyServe
                       ]
            `fallthrough` do r <- render ctxt "404"
                             case r of
                               Just r' -> return r'
                               Nothing -> notFoundText "Page not found"
