module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show (ReverseList a) where
    showsPrec = notImplementedYet
    show = notImplementedYet
instance (Eq a) => Eq (ReverseList a) where
    (==) = notImplementedYet
    (/=) = notImplementedYet
instance Semigroup (ReverseList a) where
instance Monoid (ReverseList a) where
instance Functor ReverseList where
instance Applicative ReverseList where
instance Monad ReverseList where
