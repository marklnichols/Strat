import Data.Tree
import Data.Tree.Zipper

deld = toTree $ childByMove 2 (fromTree aTree)

aTree = Node TreeItem {move = 0, value = 0} [
    Node TreeItem {move = 1, value = -80} [
        Node TreeItem {move = 3, value = 20} [
            Node TreeItem {move = 8, value = 10} []], 
        Node TreeItem {move = 4, value = -40} [
            Node TreeItem {move = 9, value = 5} [], 
            Node TreeItem {move = 10, value = 50} []]], 
    Node TreeItem {move = 2, value = 70} [
        Node TreeItem {move = 5, value = 45} [
            Node TreeItem {move = 11, value = 0} [], 
            Node TreeItem {move = 12, value = -10} []], 
        Node TreeItem {move = 6, value = -60} [
            Node TreeItem {move = 13, value = -20} [], 
            Node TreeItem {move = 14, value = 0} []], 
        Node TreeItem {move = 7, value = 30} [
            Node TreeItem {move = 15, value= 80} [], 
            Node TreeItem {move = 16, value= -90} [], 
            Node TreeItem {move = 17, value = 10} []]]] 