import Text.Parsec
import Control.Monad (liftM)
import Data.List (intersperse, replicate, find)
import Data.Char (isSpace)

data Ast = Ast String String [String] String Char Char

data Options = Options { openChar::Char, closeChar::Char }

trim = f . f where f = reverse . (dropWhile isSpace)

instance Show Ast where
  show (Ast indent begin items end open close) =
    let indent2 = replicate ((length indent) + (length begin)) ' ' in
      indent ++ begin ++ [open, ' ']
        ++ (concat $ intersperse ("\n" ++ indent2 ++ ", ") (map trim $ items)) ++ "\n"
        ++ indent2 ++ [close] ++ end

p1 <++> p2 = do
 a <- p1
 b <- p2
 return $ a ++ b

pIndent = many $ char ' '

pInnerJunk opts = many1 $ noneOf [openChar opts, closeChar opts]
pOuterJunk opts = many1 $ noneOf [openChar opts, closeChar opts, ',']

pParen opts =
  string [openChar opts]
  <++> (option "" $ pInner opts)
  <++> string [closeChar opts]

pJunkWithParen opts =
  (option "" $ pInnerJunk opts)
  <++> (pParen opts)
  <++> (option "" $ pInnerJunk opts)

pArgWithParen opts  =
  (option "" $ pOuterJunk opts)
  <++> (pParen opts)
  <++> (option "" $ pOuterJunk opts)

pInner opts = liftM concat $ many $ try (pJunkWithParen opts)
                                    <|> pInnerJunk opts
pItem opts = liftM concat $ many $ try (pArgWithParen opts)
                                   <|> pOuterJunk opts

pItems opts = (pItem opts) `sepBy` (string ",")

pWholething opts = do
  indent <- pIndent
  begin <- many $ noneOf [openChar opts]
  char $ openChar opts
  items <- pItems opts
  char $ closeChar opts
  end <- many anyChar
  return $ Ast indent begin items end (openChar opts) (closeChar opts)

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
        case parse (pWholething (Options { openChar = openChar, closeChar = closeChar })) "(stdin)" c of
          Left err -> print err
          Right ast -> putStr $ show ast
