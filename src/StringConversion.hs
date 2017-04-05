-----------------------------------------------------------------------------
--
-- Module      :  Strings
-- Copyright   :
--
-- | a module to convert between character string encodings
--  require more systematic checks what are permited characters for reps
-- (especially for input to urlEncoding)

-- the latin encoding is produced by show ...
-- t2u is nearly invertible...

-- strings remain here, to be used when constructing the wrappers for
-- functions used from other packages (with String interfaces)

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module StringConversion (
    ByteString
    -- , s2b, b2s, b2t,   t2b, t2u,  s2u
    , s2t, t2s
    -- uses UTF8 as encoding in ByteString
    -- urlencode is always represented the same as the input
    -- , Text, BSUTF
    -- , b2bu, bu2b, bu2s, bu2t, t2bu, s2bu
    -- , s2bl -- lazy bytestring
    , htf_thisModulesTests
    )   where

import           Data.Text.Arbitrary  ()
import           Test.Framework
import           Test.Invariant
import           Safe
-- import           Data.Maybe   -- from fay-base

--import Data.ByteString.Arbitrary
-- does not produce a simple Arbitrary for Bytestring, could be converted?

-- import "monads-tf" Control.Monad.State (MonadIO, liftIO)
-- import GHC.Exts( IsString(..) )

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as Lazy
-- An efficient compact, immutable byte string type (both strict and lazy)
-- suitable for binary or 8-bit character data.
-- import qualified Data.ByteString.UTF8 as BSUTF (toString, fromString)
-- toString replaces invalid chars with '\xFFFD'
import           Data.Text.Encoding   (decodeUtf8, decodeUtf8', encodeUtf8)
-- decode bytestring to text (exception if not valid)

-- URL encode (escaped)
--import qualified Network.HTTP as HTTP (urlDecode, urlEncode)
import qualified Network.URI          as URI
import qualified Snap.Core            as SN

-- Text (UTF8) -- String
-- trivial conversion,   the same set of values

s2t :: String -> Text
-- ^ String to Text (invertable)
s2t = T.pack

t2s :: Text -> String
-- ^ String to Text (invertable)
t2s = T.unpack

prop_t2s :: Text -> Bool
prop_t2s = inverts s2t t2s  -- inversts: from test-invariants
prop_s2t :: String -> Bool
prop_s2t = inverts t2s s2t

-- second step
-- ByteString -- Text
-- bytestring can contain any bitcombinations (binary)

-- bytestring with utf encoded characters
newtype BSUTF = BSUTF ByteString deriving (Show, Eq)
unBSUTF (BSUTF a) = a
instance Arbitrary ByteString where arbitrary = ByteString.pack <$> arbitrary
instance Arbitrary BSUTF where
    arbitrary =  do
                    c :: ByteString <- arbitrary
                    let t = testByteStringUtf8 c
                    if t then return (BSUTF c) else arbitrary

t2bu :: Text ->  BSUTF
-- ^ Text to Bytestring (invertable)
t2bu = BSUTF . encodeUtf8

bu2t ::  BSUTF -> Text
-- ^ ByteString to Text -- not inverse (not any arbitrary input)
bu2t = decodeUtf8 . unBSUTF

prop_t2bu :: Text -> Bool
prop_t2bu = inverts bu2t t2bu
prop_bu2t :: BSUTF -> Bool
prop_bu2t = inverts t2bu bu2t

-- conversion ByteString BSUTF
b2bu :: ByteString -> Maybe BSUTF
b2bu a = if testByteStringUtf8 a then Just (BSUTF a) else Nothing

bu2b :: BSUTF -> ByteString
bu2b = unBSUTF

bu2s :: BSUTF -> String
bu2s = t2s . bu2t

testByteStringUtf8 :: ByteString -> Bool
-- ^ test whether a byte string is valid utf8 encoded
-- used for avoiding problems with the quickcheck conversions
testByteStringUtf8 b =
    case decodeUtf8' b of
                -- :: ByteString -> Either UnicodeException Text
                    Left s  -> False
                    Right t -> True

t2b :: Text -> ByteString
t2b = bu2b . t2bu

b2t :: ByteString -> Maybe Text
b2t = fmap bu2t . b2bu

prop_t2b ::Text -> Bool
prop_t2b a = maybe False (a ==)  (b2t . t2b $ a)
-- exceptions in decoding cannot occur, because starts with UTF8

prop_b2t :: ByteString -> Bool
prop_b2t a = maybe True ((a==) . t2b) mb
    where   mb = b2t a
-- bytestring -- string (just a composition of t2s . b2t and reverse)
-- works only if the bytestring is UTF8

s2bu :: String ->  BSUTF
-- ^ String to Bytestring (invertable)
s2bu = BSUTF . encodeUtf8 . s2t

--bu2s ::  BSUTF -> String
---- ^ ByteString to String -- not inverse (not any arbitrary input)
--bu2s = t2s . decodeUtf8 . unBSUTF

s2b :: String -> ByteString
s2b = t2b . s2t

s2bl :: String -> Lazy.ByteString
s2bl = Lazy.fromStrict . s2b

b2s :: ByteString -> Maybe String
b2s = fmap t2s . b2t

-- url encoding - two different kinds: url and form data
-- url - in url encode space as %20, as done in the network-uri library (for strings)
--  better name: escape?
-- urlForm - in form as + , as done in the snap core librar (for bytestrings in utf8 endocode)

newtype URL = URL String deriving (Show, Eq)
unURL (URL t) = t
instance Arbitrary URL where
    arbitrary = do
            a <- arbitrary
            return . s2url $ a


s2url :: String -> URL
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2url =   URL . URI.escapeURIString URI.isUnescapedInURIComponent
--s2url =   URL . HTTP.urlEncode

url2s :: URL -> String
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
url2s  =   URI.unEscapeString . unURL
--url2s a =   HTTP.urlDecode . unURL $ a

prop_s2url = inverts url2s s2url
prop_url2s = inverts s2url url2s

testUrlEncodingURI :: String -> Bool
testUrlEncodingURI a = a == (unURL . s2url . url2s . URL $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
-- input is URL encoded string

test_httpEncode = assertEqual "x%20x" (s2u "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

url2u = unURL
u2url a = if testUrlEncodingURI a then Just (URL a) else Nothing


s2u :: String -> String
-- ^ convert string to url   (uses code from Network.HTTP, which converts space into %20)
s2u = url2u . s2url

u2s :: String -> Maybe String     --not inverse
-- ^ convert url to string   (uses code from Network.HTTP, which converts space into %20)
u2s  =   fmap url2s . u2url

prop_s2u :: String -> Bool
prop_s2u a = maybe False (a==) (u2s . s2u $ a)

test_s2uA = assertEqual (Just "A") (u2s . s2u $ "A")
test_s2uB = assertEqual (Just " ") (u2s . s2u $ " ")
test_s2uB1 = assertEqual ("%20") (s2u $ " ")
test_s2uC1 = assertEqual False (testUrlEncodingURI "%a ")
--test_s2uC2 = assertEqual ("%20") (HTTP.urlDecode "%a ")
test_s2uC2 = assertEqual ("%a ") (URI.unEscapeString "%a ")
test_s2uC3 = assertEqual Nothing (u2s $ "%a ")

prop_u2s :: String -> Bool
prop_u2s a = maybe True ((a==) . s2u) mb
    where   mb = u2s a

-- case for encoding of form content (with + for space)

newtype URLform = URLform ByteString deriving (Show, Eq)
unURLform (URLform t) = t
instance Arbitrary URLform where
    arbitrary = do
            a :: ByteString <- arbitrary
            return . b2urlf $ a


b2urlf :: ByteString -> URLform
-- ^ convert string to url   (uses code from SNAP, which converts space into +)
b2urlf =   URLform . SN.urlEncode

urlf2b :: URLform -> ByteString
-- ^ convert url to string   (uses code from SNAP, which converts space into +)
urlf2b = fromJustNote "urlf2b nothing" . SN.urlDecode . unURLform

prop_b2urlf = inverts urlf2b b2urlf
prop_urlf2b = inverts b2urlf urlf2b

testUrlEncodingSNAP :: ByteString -> Bool
testUrlEncodingSNAP a =  ( maybe False ((a ==). SN.urlEncode) . SN.urlDecode $ a)
--testUrlEncodingSNAP a = a == (unURLform . b2urlf . urlf2b . URLform $ a)
-- checks if reencoding with HTTP gives the same URL, thus ok encoding
--testUrlEncodingSNAP a = isJust . SN.urlDecode $ a
-- checks if reencoding with SNAP gives the same URLform, thus ok encoding
-- this test allows control in url encoded strings ...

test_SNAPEncode = assertEqual "x+x" (b2uf .s2b $ "x x")
--test_snapEncode = assertEqual "x+x" (b2s . b2u . s2b $  "x x")

urlf2u = unURLform
u2urlf a = if testUrlEncodingSNAP a then Just (URLform a) else Nothing
-- this test allows control in url encoded strings ...


b2uf :: ByteString -> ByteString
-- ^ convert ByteString to url   (uses code from SNAP which converts space into +)
b2uf = urlf2u . b2urlf

uf2b :: ByteString -> Maybe ByteString     --not inverse
-- ^ convert url to ByteString   (uses code from SNAP, which converts space into +)
uf2b  =   fmap urlf2b . u2urlf

prop_b2uf :: ByteString -> Bool
prop_b2uf a = maybe False (a==) (uf2b . b2uf $ a)

test_b2uA = assertEqual (Just "A") (uf2b . b2uf . s2b $ "A")
test_b2uB = assertEqual (Just " ") (uf2b . b2uf . s2b $ " ")
test_b2uB1 = assertEqual ("+") (b2uf . s2b $ " ")

prop_uf2b :: ByteString -> Bool
prop_uf2b a = maybe True ((a==) . b2uf) mb
    where   mb = uf2b a

test_u2b1 = assertEqual (s2b "%01") (b2uf   $ "\SOH")
test_u2b2 = assertEqual (s2b "%02") (b2uf   $ "\STX")
test_u2b3 = assertEqual (s2b "%00") (b2uf   $ "\NUL")

test_u2bx1 = assertEqual (Just "\SOH") (uf2b . s2b $ "%01")
test_u2bx2 = assertEqual (Just "\STX")(uf2b . s2b $ "%02")
test_u2bx3 = assertEqual (Just "\NUL") (uf2b . s2b $ "%00")

--test_u2by1 = assertEqual (Just "\SOH") (u2b  "\SOH")
-- with specific test for url:
test_u2by1 = assertEqual Nothing (uf2b  "\SOH")
test_u2sy1 = assertEqual Nothing (u2s  "\SOH")


-- check if snap and network-uri decode the same
-- the difference is only in the encoding

--prop_snap_uri :: String -> Bool
--prop_snap_uri =  (testUrlEncodingSNAP . s2b) <=>  (testUrlEncodingURI )
-- except "!" "$"(see test_u2by2 u2sy2)

--prop_snap_uri_decode :: String -> Bool
--prop_snap_uri_decode =  ( uf2b . s2b) <=>  (fmap s2b . u2s )
-- the actual decoding is different for some chars, e.g.:

test_u2by2 = assertEqual (Just "!") (uf2b  "!")
test_u2sy2 = assertEqual Nothing (u2s  "!")
test_u2by3 = assertEqual (Just "$") (uf2b  "$")
test_u2sy3 = assertEqual Nothing (u2s  "$")
test_u2by4 = assertEqual Nothing (uf2b  "~")
test_u2sy4 = assertEqual (Just "~") (u2s  "~")
test_u2by5 = assertEqual (Just " ")  (uf2b  "+")
test_u2sy5 = assertEqual Nothing(u2s  "+")
test_u2by6 = assertEqual (Just ")")  (uf2b  ")")
test_u2sy6 = assertEqual Nothing(u2s  ")")
test_u2by6a = assertEqual (Just "(")  (uf2b  "(")
test_u2sy6a = assertEqual Nothing(u2s  "(")
test_u2by7 = assertEqual Nothing  (uf2b  "\"")
test_u2sy7 = assertEqual Nothing (u2s  "\"")
test_u2by8 = assertEqual (Just ",")  (uf2b  ",")
test_u2sy8 = assertEqual Nothing (u2s  ",")


t2u :: Text -> Text
t2u = s2t . s2u . t2s
u2t :: Text -> Maybe Text
u2t = fmap s2t . u2s . t2s

b2u :: ByteString -> Maybe ByteString
b2u a = (fmap s2b) .  fmap s2u . b2s $ a
u2b :: ByteString -> Maybe ByteString
u2b = fmap s2b . joinMaybe . fmap u2s . b2s

prop_t2u :: Text -> Bool
prop_t2u a = maybe True (a==) (u2t . t2u $ a)

prop_b2u :: ByteString -> Bool
prop_b2u a = maybe True (a==) (joinMaybe . fmap u2b . b2u $ a)

prop_u2t :: Text -> Bool
prop_u2t a = maybe True ((a==) . t2u) mb
    where   mb = u2t a

prop_u2b :: ByteString -> Bool
prop_u2b a = maybe True ( (a==)   ) mb
    where   mb = joinMaybe . fmap b2u . u2b $ a

-- copied from fay-base - because fay-base introduces a lot of new dependencies
-- | Join for Maybe.
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just x)) = Just x
joinMaybe _ = Nothing
