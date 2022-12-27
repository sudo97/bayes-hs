import DocsGen (md)

main :: IO ()
main = writeFile "./ENDPOINTS.md" md