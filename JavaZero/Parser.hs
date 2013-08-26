module JavaZero.Parser where

import JavaZero.Types as J
import JavaZero.Parser.Rules as R

parse filename = do
    src <- readFile filename
    let result = R.parse src filename
    return result
