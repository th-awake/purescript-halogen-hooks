let conf = ./test.dhall
in conf //
  { backend = "../purescript-backend-optimizer/backend-es/index.js build"
  }
