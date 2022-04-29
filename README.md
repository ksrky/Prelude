# Prelude

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ksrky/paff/Haskell%20CI)](https://github.com/ksrky/prelude/actions/workflows/haskell.yml)

## Run parser
```command
$ stack ghci
> import Data.Text (pack)
> import Text.Megaparsec (parseTest)
> parseTest pProg (pack "1+2;3*4")
Prog [Add (Int 1) (Int 2),Mul (Int 3) (Int 4)]
```

## REPL
```command
$ stack runghc src/REPL.hs
```

## Build & Run
```command
$ stack run examples/ex.prl
$ llc examples/ex.ll
$ gcc examples/ex.s -o examples/ex
$ ./examples/ex
```

### Check output easily
```command
$ stack run examples/ex.prl
$ lli examples/ex.ll
```