module PMerge.Merge (merge) where
import Data.String.Utils

-- given "Foo [_1] Baz" and ["Bar"], return "Foo Bar Baz"

merge' :: Int -> String -> [String] -> String
merge' _ base [] = base
merge' i base (x:xs) =
    let lookFor = "[_" ++ (show i) ++ "]"
        newStr = replace lookFor x base in
    merge' (i+1) newStr xs

merge :: [String] -> String
merge (base:xs) = merge' 1 base xs
