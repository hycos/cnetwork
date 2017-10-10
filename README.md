# cnetwork

cnetwork enables you to manage constraints by providing a simple Java API for
constraint network construction and constraint network manipulation. 

# Status

[![License: EUPL](https://img.shields.io/badge/License-EUPL-yellow.svg)][licence]
[![Language](http://img.shields.io/badge/language-java-brightgreen.svg)][language]
[![Maven](https://maven-badges.herokuapp.com/maven-central/com.github.hycos/cnetwork/badge.svg)][maven]
[![Linux Build Status](https://img.shields.io/travis/hycos/cnetwork/master.svg?label=Linux%20build)][travis]
[![Test Coverage](https://codecov.io/gh/hycos/cnetwork/branch/master/graph/badge.svg)][coverage]

[licence]: https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf
[language]: https://www.java.com
[maven]: https://maven-badges.herokuapp.com/maven-central/com.github.hycos/cnetwork
[travis]: https://travis-ci.org/hycos/cnetwork
[coverage]: https://codecov.io/gh/hycos/cnetwork


# Usage

## Constraint Network Construction

The example below illustrates how to use the cnetwork API. The
`ConstraintNetworkBuilder` manages the constraint network construction. The
`addOperand()`, `addOperation()` and `addConstraint()` methods add
variables/operands, operations (i.e., a function with a non-boolean return
value) and constraints to the network, respectively. All of these methods
create `Node` instances in the constraint network. The `addOperand()` method
takes as argument the type and the identifier as input, whereas the
`addOperation()` and `addConstraint()` methods takes as input the kind of
operation/constraint and the involved parameter nodes.

```java
ConstraintNetworkBuilder cn = new ConstraintNetworkBuilder();
Node zero = cn.addOperand(NodeKind.NUMLIT, "0");
Node one = cn.addOperand(NodeKind.NUMLIT, "1");
Node three = cn.addOperand(NodeKind.NUMLIT, "3");
Node dot = cn.addOperand(NodeKind.STRLIT, ".");
Node add = cn.addOperation(NodeKind.ADD, one, zero);
cn.addConstraint(NodeKind.EQUALS, add, zero);
```

## Slicing 

In case you would like to do an impact analysis in a constraint network, you
can use the `CnetworkSlicerForward` object or `CnetworkSlicerBackward` classes.
The slicer takes as input a constraint network instance. After invoking the
`slice()` method with the slicing criterion as input, the method returns a set
of the nodes which appear in the slice. The following code snippet illustrates
a backward slicing example where the set `nodes` contains the two constraint
network nodes `tostr` and `var`.

```java
Node five = cn.addOperand(NodeKind.NUMLIT, "5");
Node s = cn.addOperand(NodeKind.STRLIT, "s");
Node var = cn.addOperand(NodeKind.NUMVAR, "v");
Node tostr = cn.addOperation(NodeKind.TOSTR, var);

Node concat = cn.addOperation(NodeKind.CONCAT, tostr, s);

CnetworkSlicerBackward b = new CnetworkSlicerBackward(cn
        .getConstraintNetwork());
```

## Visualization

You can visualize a constraint network by invoking the `.toDot()` method which
serializes the constraint network in the `.dot` output format. From this input
format, you print a graph by means of [graphviz](http://www.graphviz.org) (for
example by invoking `dot -T pdf <infile> -o <ofile>`). In case you would like
to automate this task, this
[script](https://gist.github.com/julianthome/66a31203b9b25493fa2a43889f948212)
might be useful. The constraint network for the first example would look as
follows.

![Screenshot](img/cnetwork.png)


# License

Copyright (C) 2017 Julian Thome <julian.thome.de@gmail.com>

cnetwork is licensed under the EUPL, Version 1.1 or – as soon
they will be approved by the European Commission - subsequent versions of the
EUPL (the "Licence"); You may not use this work except in compliance with the
Licence. You may obtain a copy of the Licence at:

https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdf

Unless required by applicable law or agreed to in writing, software distributed
under the Licence is distributed on an "AS IS" basis, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the Licence for the
specific language governing permissions and limitations under the Licence.

