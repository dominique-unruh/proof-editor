{-# OPTIONS_GHC -F -pgmF htfpp #-}

module UserError.UserErrorTest where

import Test.Framework
import UserError.UserError

{-# ANN module "HLint: ignore Use camelCase" #-}

test_usererror_show :: IO ()
test_usererror_show = do
    err <- userErrorFromFile "usererrortest" "src/UserError/usererrortest.xml"
    print (shortDescription err)
    print err
    assertEqual "This is a <i>short</i> description" $ show err

test_usererror_show_data :: IO ()
test_usererror_show_data = do
    err <- userErrorFromFile "usererrortest" "src/UserError/usererrortest.xml"
    let err' = addErrorData "test" (123::Integer) err
    print (errorData err')
    print err'
    assertEqual "This is a <i>short</i> description\n    test = 123" $ show err'

test_usererror_from_db :: IO ()
test_usererror_from_db = do
    let err = userErrorDB "resources/errors" "CommutativityNotCommutative"
    print err
