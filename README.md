# AVL-tree

Implementating and using an avl tree defined by 

empty           -- an empty tree

(node-left t)   -- left subtree of non-empty tree

(node-right t)  -- right subtree of non-empty tree

(node-key t)    -- number labelling the root of t 





with functions such as 

(insertavl t n) -- add number n to t, if not present

(deleteavl t n) -- delete n from t, if present

(listavl t)     -- ordered list of elements in t

(sizeavl t)     -- the number of elements in t





set functions done by implementing avl-trees

emptyset              -- the empty set

(emptyset? s)         -- returns true is set is empty, false otherwise

(singleton n)         -- a set containing the number n

(union s1 s2)         -- a set containing the union of sets s1 and s2

(intersection s1 s2)  -- a set containing the intersection of sets s1 and s2

(difference s1 s2)    -- a set containing elements in s1 but not in s2

(size s)              -- the number of elements in set s

(nth s i)             -- the ith element of s with 0 being the smalles index
