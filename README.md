# Red-black trees

Implementation in Prolog.

(c) 2009-2014 [Roman Piták](http://pitak.net) roman@pitak.net

Red-black trees are self-balancing binary search trees. The implementation is
complex, but it has a good worst-case running time for its operations and is
effective in practice: it can search, insert and delete in O(log n) time, where n
is the total number of elements in the tree.

## USAGE

    rb_check( +Tree )
        Is red-black tree valid?
    rb_empty( ?Tree )
        Is empty?
    rb_insert( +Tree, +Key, -NewTree )
        Inserts a new node with the given Key into the given Tree,
        preserving all the required red-black tree properties.
    rb_build( +List, -Tree )
        Takes a list of elements and builds a red black tree from scratch.
    rb_delete( +Tree, +Key, -NewTree )
        Deletes the node with the given Key from the given Tree.
    rb_search( +Tree, +Element )
        Searches in the given Tree for the given Element.
    rb_maximum( +Tree, -Maximum )
        Finds the maximal Element of the given Tree.
    rb_minimum( +Tree, -Minimum )
        Finds the minimal Element of the given Tree.
    rb_inOrder( +Tree, -List )
        Performs an in-order tree traversal of the given tree and outputs 
        a list of it’s elements as defined the rules of an in-order tree walk.
    rb_print( +Tree )
        The rb print predicate prints the given tree on the standard output 
        in a human readable format. An example of the rb print output:
        
    [ ] Black node
    ( ) Red node
    
          |—(19)
      |—[16]
      |   |—(15)
    [12]
      |      |—(11)
      |  |—[10]
      |  |   |—(9)
      |—(5)
         |  |—(3)
         |—[2]
            |—(1)
    
Full documentation can be found in the [documentation.pdf](/documentation.pdf) file.
