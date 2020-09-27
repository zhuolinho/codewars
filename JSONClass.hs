-- file: ch06/JSONClass.hs
{-# LANGUAGE FlexibleInstances #-}

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "not a JSON boolean"

instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

instance {-# OVERLAPPABLE #-} (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined

result :: JValue
result =
  JObject
    [ ("query", JString "awkward squad haskell"),
      ("estimatedCount", JNumber 3920),
      ("moreResults", JBool True),
      ( "results",
        JArray
          [ JObject
              [ ("title", JString "Simon Peyton Jones: papers"),
                ("snippet", JString "Tackling the awkward ..."),
                ("url", JString "http://.../marktoberdorf/")
              ]
          ]
      )
    ]

abc = (fromJValue (toJValue [("foo", "bar")])) :: Either JSONError JValue