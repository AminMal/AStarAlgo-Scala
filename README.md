## A* algorithm visualized in Scala

This project runs the A* algorithm on a graph, based on the given nodes and connections, weather given as input file
with the accepted syntax (which are further shown) or instantiated manually.

Nodes and connections syntax are like so:

Nodes must be given in a file with this syntax:
```
name = [a-zA-Z0-9] | heuristic = [:number:]
For instance:
name = A | heuristic = 10
.
.
.
```

Connections must be given in a file like this:
```
[source node name] | [destinatino node name] | [distance or weight]
For instance:
A | B | 6   (connection from node A to node B with weight of 6)
```

Each package notes and help is written inside the package, but in general the algorithm runner has 2 modes:
* Verbose mode ( prints each choice and information about candidates)
* Normal mode ( Just returns an IndexedSeq of the result path)

Node insertion and placement can be done with either
* nQueens algorithm ( in some cases may cause conflicts or linear
node placement )
* Circlic insertion ( places nodes in a circle which cannot have conflicts - default project insertion algorithm)

#Run
Running the default project Run file will create a screen showing nodes with connections and all the other required 
information, also shows the result path in red, BUT you can use models and algorithm package
and run it as you wish!

