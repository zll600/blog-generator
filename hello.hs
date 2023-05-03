import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( -- append_
      (h_ 1 "Heading")
        <> ( (p_ "Paragraph #1")
               <> (p_ "Paragraph #2")
           )
    )
