module Split where

mySplit ::  Char -> String -> [String]
mySplit _ "" = []
mySplit marker xs = mySplit' xs []
            where
                mySplit' :: String -> [String] -> [String]
                mySplit' "" ys = ys
                mySplit' (c:cs) ys = if c == marker
                    then ys ++ mySplit' cs []
                    else mySplit' cs (safeInit ys ++ [getNext (safeLast ys) c])
                      where
                        getNext :: Maybe String -> Char -> String
                        getNext Nothing ch = [ch]
                        getNext (Just lastEl) ch = lastEl ++ [ch]

                        -- Safe version of `last`
                        safeLast :: [a] -> Maybe a
                        safeLast [] = Nothing
                        safeLast zs = Just $ last zs
                        
                        -- Safe version of `init`
                        safeInit :: [a] -> [a] 
                        safeInit [] = []
                        safeInit zs = init zs