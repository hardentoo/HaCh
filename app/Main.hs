module Main where

import App (runApp)

main :: IO ()
main = runApp


-- curl -H "Content-Type: application/json" -X POST -d '{"heading":"3","text":"333"}' http://localhost:8080/msg/new
