{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
import qualified Data.ByteString.Lazy as BS
import Codec.Candid
import Data.Row as R hiding (switch)
import qualified Data.Vector as Vec
import qualified Data.Text as T
import IC.Utils
import IC.Management

dummyResponse :: HttpResponse
dummyResponse = R.empty
  .+ #status .== 202
  .+ #headers .== Vec.empty
  .+ #body .== (toUtf8 $ T.pack s)
  where s = (replicate 10000000 'x') :: String

main :: IO ()
main =
    putStrLn $ show $ BS.length $ Codec.Candid.encode dummyResponse
