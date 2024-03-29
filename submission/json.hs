-- | Implement a JSON parser.
module JSON where

import Data.Char

import Parser
import Parser.Instances
import Specials

-- | Associative container type.
type Assoc = [(String, JsonValue)]

-- | JSON value representation.
data JsonValue =
     JsonString String
   | JsonRational !Rational
   | JsonObject Assoc
   | JsonArray [JsonValue]
   | JsonTrue
   | JsonFalse
   | JsonNull
  deriving (Show, Eq)

-- | Write a function that applies the first parser, runs the third parser
-- keeping the result, then runs the second parser and produces the obtained
-- result.
--
-- This answer is precompleted to show you an example.
--
-- >>> parse (between (is '[') (is ']') character) "[a]"
-- Result >< 'a'
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "abc]")
-- True
between :: Parser o -> Parser c -> Parser a -> Parser a
between p1 p2 p3 = do
  _ <- p1
  x <- p3
  _ <- p2
  return x

-- | Write a function that applies the given parser in between the two given
-- characters.
--
-- /Hint/: Use 'between' and 'charTok'.
--
-- >>> parse (betweenCharTok '[' ']' character) "[a]"
-- Result >< 'a'
--
-- >>> parse (betweenCharTok '[' ']' (charTok 'a')) "[ a ] "
-- Result >< 'a'
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
-- True
--
-- >>> isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
-- True
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok c1 c2 = between (charTok c1) (charTok c2)

-- | Write a function that parses 4 hex digits and return the character value.
--
-- This question is completed for you. Make sure understand this before moving on
--
-- >>> parse hex "0010"
-- Result >< '\DLE'
--
-- >>> parse hex "0a1f"
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hex "001")
-- True
--
-- >>> isErrorResult (parse hex "0axf")
-- True
hex :: Parser Char
hex = thisMany 4 (satisfy isHexDigit) >>= (\d -> case readHex d of
  Just (x,_) -> pure $ chr x
  Nothing -> failed $ UnexpectedString d)

-- | Write a function that parses the character @u@ followed by 4 hex digits
-- and returns the character value.
--
-- /Hint/: Use 'is' and 'hex'.
--
-- >>> parse hexu "u0010"
-- Result >< '\DLE'
--
-- >>> parse hexu "u0a1f"
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hexu "0010")
-- True
--
-- >>> isErrorResult (parse hexu "u001")
-- True
--
-- >>> isErrorResult (parse hexu "u0axf")
-- True
hexu :: Parser Char
hexu = is 'u' >> hex

-- | Return a parser that produces the given special character.
--
-- /Hint/: use 'toSpecialCharacter' and 'fromSpecialCharacter'.
--
-- /Hint/: Look at hex above
--
-- >>> parse specialChar "b"
-- Result >< '\b'
--
-- >>> parse specialChar "\""
-- Result >< '"'
--
-- >>> isErrorResult (parse specialChar "a")
-- True
specialChar :: Parser Char
specialChar = character >>= (\c -> case toSpecialCharacter c of
  Just x -> pure $ fromSpecialCharacter x
  Nothing -> failed $ UnexpectedChar c)

-- | Parse a special character or a hexadecimal in JSON, has to start with
-- @\\@. See <http://json.org> for the full list of control characters in JSON.
--
-- /Hint/: Use 'hexu' and 'specialChar'.
--
-- >>> parse jsonSpecial "\\u0af3"
-- Result >< '\2803'
--
-- >>> parse jsonSpecial "\\b"
-- Result >< '\b'
--
-- >>> isErrorResult (parse jsonSpecial "\\a")
-- True
jsonSpecial :: Parser Char
jsonSpecial = is '\\' >> (hexu ||| specialChar)

-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal
-- characters.
--
-- /Hint/: Use 'between', 'is', 'charTok', 'noneof', 'jsonSpecial'.
--
-- /Hint/: The inner parser needs to /fail/ in order to trigger the second
-- delimiter.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"abc\"   def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString :: Parser String
jsonString = between (is '\"') (charTok '\"') (list (jsonSpecial ||| noneof "\"\\"))

-- | Parse a JSON rational.
--
-- /Hint/: Use 'readFloats', 'tok'.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber :: Parser Rational
jsonNumber = P f
  where
    f s = case readFloats s of
     Just (x, rest) -> Result rest x
     Nothing -> Error UnexpectedEof

-- | Parse a JSON true literal.
--
-- /Hint/: Use 'stringTok'.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue :: Parser String
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- /Hint/: Use 'stringTok'.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse :: Parser String
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- /Hint/: Use 'stringTok'.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull :: Parser String
jsonNull = stringTok "null"

-- | Write a parser that parses between the two given characters, separated by
-- a comma character ','.
--
-- /Hint/: Use 'betweenCharTok', 'sepby' and 'commaTok'.
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a]"
-- Result >< "a"
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[]"
-- Result >< ""
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a,b,c]"
-- Result >< "abc"
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
-- True
betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma c1 c2 p = betweenCharTok c1 c2 (sepby p commaTok)

-- | Parse a JSON array.
--
-- /Hint/: Use 'betweenSepbyComma' and 'jsonValue'.
--
jsonArray :: Parser [JsonValue]
jsonArray = betweenSepbyComma '[' ']' (jsonValue)

-- | Parse a JSON object.
--
-- /Hint/: Use 'jsonString', 'charTok', 'betweenSepbyComma' and 'jsonValue'.
--
-- /Hint/: Remember the type of 'Assoc' = @[(String, JsonValue)]@.
--
-- /Hint/: Use anonymous apply '<*' to omit tokens.
--

jsonObject :: Parser Assoc
jsonObject = betweenSepbyComma '{' '}' jsonObjectHelper

jsonObjectHelper :: Parser (String, JsonValue)
jsonObjectHelper = do
  key <- jsonString <* charTok ':'
  value <- jsonValue
  pure (key, value)

-- | Parse a JSON value.
--
-- /Hint/: Use 'spaces', 'jsonNull', 'jsonTrue', 'jsonFalse', 'jsonArray',
-- 'jsonString', 'jsonObject' and 'jsonNumber'.
-- This function is half done. Please add the rest of the cases.
--
-- /Hint/: Use anonymous map '<$' to "type-hint" your literals.
--
-- /Hint/: Use fmap '<$>' to "type-hint" your values.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
jsonValue :: Parser JsonValue
jsonValue = JsonTrue <$ jsonTrue
  ||| JsonFalse <$ jsonFalse
  ||| JsonNull <$ jsonNull
  ||| JsonString <$> jsonString
  ||| JsonRational <$> jsonNumber
  ||| JsonObject <$> jsonObject
  ||| JsonArray <$> jsonArray

-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]

-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]

--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]

-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]

-- | Read a file into a JSON value.
--
-- /Hint/: Use 'readFile' and 'jsonValue'.
readJsonValue :: FilePath -> IO (ParseResult JsonValue)
readJsonValue f = do
        text <- readFile f
        return $ parse jsonValue text