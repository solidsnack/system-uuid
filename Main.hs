

import System.UUID.V1

import Numeric
import Text.Printf


main                         =  do
  uuid                      <-  uuid
  putStrLn $ concatMap (printf "%02.2x") uuid

