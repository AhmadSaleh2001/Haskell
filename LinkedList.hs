data Node = MyNothing | Val Int | Nxt Node Node

getNodeValue :: Node -> Int
getNodeValue (Val x) = x

addLast :: Node -> Int -> Node
addLast MyNothing x = Nxt (Val x) MyNothing
addLast (Nxt n1 n2) x = Nxt n1 (addLast n2 x)

addFirst :: Node -> Int -> Node
addFirst (Nxt n1 n2) x = Nxt (Val x) (Nxt n1 n2)

deleteSpecific :: Node -> Int -> Node
deleteSpecific MyNothing x = MyNothing
deleteSpecific (Nxt n1 n2) x
 | getNodeValue(n1) == x = n2
 | otherwise = Nxt n1 (deleteSpecific n2 x)
  
  
  
sumLinkedList :: Node -> Int
sumLinkedList MyNothing = 0
sumLinkedList (Val x) = x
sumLinkedList (Nxt n1 n2) = (sumLinkedList n1) + (sumLinkedList n2)

main :: IO ()
main =  do

let linkedListHead = Nxt (Val 1) MyNothing
let newHead1 = addLast linkedListHead 2
let newHead2 = addLast newHead1 3
let newHead3 = addLast newHead2 4

let newHeadAfterDelete = deleteSpecific newHead3 3
let afterAddFirst = addFirst newHeadAfterDelete 10


print(sumLinkedList linkedListHead)
print(sumLinkedList newHead1)
print(sumLinkedList newHead2)
print(sumLinkedList newHead3)
putStr "after delete: "
print(sumLinkedList newHeadAfterDelete)
print(sumLinkedList afterAddFirst)
