import Text.Parsec
import Control.Monad (liftM)
import Data.List (intersperse, replicate, find)
import Data.Char (isSpace)

data Ast = Ast String String [String] String Char Char

trim = f . f where f = reverse . (dropWhile isSpace)

instance Show Ast where
  show (Ast indent begin args end openChar closeChar) =
    let indent2 = replicate ((length indent) + (length begin)) ' ' in
      indent ++ begin ++ [openChar, ' ']
        ++ (concat $ intersperse ("\n" ++ indent2 ++ ", ") (map trim $ args)) ++ "\n"
        ++ indent2 ++ [closeChar] ++ end

p1 <++> p2 = do
 a <- p1
 b <- p2
 return $ a ++ b

pIndent = many $ char ' '

pInnerJunk openChar closeChar = many1 $ noneOf [openChar, closeChar]
pOuterJunk openChar closeChar = many1 $ noneOf [openChar, closeChar, ',']

pParen openChar closeChar =
  string [openChar]
  <++> (option "" $ pInner openChar closeChar)
  <++> string [closeChar]

pJunkWithParen openChar closeChar =
  (option "" $ pInnerJunk openChar closeChar)
  <++> (pParen openChar closeChar)
  <++> (option "" $ pInnerJunk openChar closeChar)

pArgWithParen openChar closeChar  =
  (option "" $ pOuterJunk openChar closeChar)
  <++> (pParen openChar closeChar)
  <++> (option "" $ pOuterJunk openChar closeChar)

pInner openChar closeChar = liftM concat $ many $ try (pJunkWithParen openChar closeChar)
                                                  <|> pInnerJunk openChar closeChar
pItem openChar closeChar = liftM concat $ many $ try (pArgWithParen openChar closeChar)
                                                     <|> pOuterJunk openChar closeChar

pItems openChar closeChar = (pItem openChar closeChar) `sepBy` (string ",")

pWholething openChar closeChar = do
  indent <- pIndent
  begin <- many $ noneOf [openChar]
  char openChar
  args <- pItems openChar closeChar
  char closeChar
  end <- many anyChar
  return $ Ast indent begin args end openChar closeChar

main = do
  c <- getContents
  case find (flip elem "([{") c of
    Nothing -> putStr c
    Just openChar ->
      let closeChar = case openChar of
                        '('       -> ')'
                        '['       -> ']'
                        otherwise -> '}'
      in
        case parse (pWholething openChar closeChar) "(stdin)" c of
          Left err -> print err
          Right ast -> putStr $ show ast
