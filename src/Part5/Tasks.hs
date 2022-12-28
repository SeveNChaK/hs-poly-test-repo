module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (h:t) = myFoldl f (f acc h) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (h:t) = f h $ myFoldr f acc t

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f lst = (f h) : myFoldr (\hl al -> (f hl) : al) [] t
    where
        h = head lst
        t = tail lst

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = notImplementedYet

concatHeadFoldr [] acc = acc
concatHeadFoldr lst acc = (head lst) : myFoldr (\hl al -> hl:al) acc (tail lst)

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat lst = h `concatHeadFoldr` (myFoldr (\hl al -> hl `concatHeadFoldr` al) [] t)
    where
        h = head lst
        t = tail lst

myReverse :: [a] -> [a]
myReverse [] = []
myReverse lst = myFoldl (\al hl -> hl : al) (h:[]) t
    where
        h = head lst
        t = tail lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p lst = if (p h) then h:callFoldr else callFoldr
    where
        h = head lst
        t = tail lst
        callFoldr = myFoldr (\hl al -> if (p hl) then hl:al else al) [] t

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p [] = ([], [])
myPartition p lst = if (p h) then (h : fst callFoldr, snd callFoldr) else (fst callFoldr, h : snd callFoldr)
    where
        h = head lst
        t = tail lst
        callFoldr = myFoldr (\hl (all, alr) -> if (p hl) then (hl:all, alr) else (all, hl:alr)) ([],[]) t

