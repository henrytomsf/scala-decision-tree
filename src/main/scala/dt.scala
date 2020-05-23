package ml.dt



// removing variances in types to make [Int, Double] work
trait DecisionTree[A,B] {
    def predict(sample: A): B
}


case class Leaf[A,B](decision: B) extends DecisionTree[A,B] {
    def predict(sample: A): B = decision
}


case class Node[A,B](splitTest: A => Boolean, left: DecisionTree[A,B], right: DecisionTree[A,B]) extends DecisionTree[A,B] {
    def predict(sample: A): B = splitTest(sample) match {
        case true => right.predict(sample)
        case false => left.predict(sample)
    }
}


case class LabelCombiner[B](combine: Vector[B] => B) {
    // using type B since B represents the label types
    // wrote own function as the original one was incorrect
    def combineWrapper(setLabels: Vector[B]): B = combine(setLabels)
}


case class Id(number: Int, depth: Int) {
    require(number >= 0, "Number should be nonnegative to use as an ID.")

    def nextIdLeft: Id = Id(number*2+1, depth+1)

    def nextIdRight: Id = Id(number*2+2, depth+1)
}


case class DecisionTreeBuilder[A,B](dataSet: LabeledData[A,B])(combiner: LabelCombiner[B]) {
    //testBuilder is the function that determines a split
    def build(testBuilder: (LabeledData[A,B], Id) => Either[String, A => Boolean]): DecisionTree[A,B] = {
        val rootId = Id(0,0)
        buildStep(dataSet, rootId, testBuilder)
    }

    private def buildStep(
        subSet: LabeledData[A,B],
        id: Id,
        testBuilder: (LabeledData[A,B], Id) => Either[String, A => Boolean]): DecisionTree[A,B] = {

        testBuilder(subSet, id) match {
            // if matches string, predict the parent leaf node
            case Left(_) => Leaf[A,B](subSet.combineLabels(combiner))
            case Right(test) =>
                val Seq(leftSubset, rightSubset) = nextSubsets(subSet, test)
                // check that during the split if left or right is empty, predict on the parent leaf node
                if (leftSubset.isEmpty || rightSubset.isEmpty) {
                    println("Warning: left or right node is empty.")
                    Leaf[A,B](subSet.combineLabels(combiner))
                }
                // since buildStep returns a DecisionTree
                else Node(test,
                    buildStep(leftSubset, id.nextIdLeft, testBuilder),
                    buildStep(rightSubset, id.nextIdRight, testBuilder)
                )
        }
    }

    private def nextSubsets(subSet: LabeledData[A,B], test: A => Boolean): Seq[LabeledData[A,B]] = {
        // adding default value so that the hashmap in the end will have "some" value and does not throw an error
        val groups = subSet.groupBy(test) withDefaultValue subSet.emptySet
        Seq(groups(false), groups(true))
    }
}


class LabeledData[A,B](
        val referenceSamples: Vector[A],
        val referencesLabels: Vector[B],
        val indices: Vector[Int]) {

    def size: Int = indices.length

    def isEmpty: Boolean = size == 0

    def subset(indices: Vector[Int]): LabeledData[A,B] = {
        new LabeledData(referenceSamples, referencesLabels, indices)
    }

    def emptySet: LabeledData[A,B] = subset(Vector())

    def groupBy[C](f: A => C): Map[C, LabeledData[A,B]] = {
        // group by some function and creates a map data structure from the 
        // result of function, C -> LabeledData
        // will be used to split node into left and right depending on condition
        // ex. indices groupBy {x => if (x < 5) "right" else "left"}
        //     val indices = Vector(1,2,3,4,5,5,5)
        //     HashMap(left -> Vector(5, 5, 5), right -> Vector(1, 2, 3, 4))
        ((indices groupBy {idx => f(referenceSamples(idx))}) mapValues subset).toMap
    }

    def partition(f: A => Boolean): (LabeledData[A,B], LabeledData[A,B]) = {
        // returning right and left nodes
        val groups = groupBy(f)
        (groups(true), groups(false))
    }

    def union(that: LabeledData[A,B]): LabeledData[A,B] = {
        // using that since it references the current class
        // need to check that the same superset is used
        //    since referenceSamples and referencesLabels do not change, just the indices!
        require(
            referenceSamples == that.referenceSamples &&
            referencesLabels == that.referencesLabels,
            "Union is only allowed for subsets of the same superset.")
        //return non duplicate indices, since don't want to reference same event more than once
        subset((indices ++ that.indices).distinct)
    }

    def countDistinctLabels: Int = {
        // slices referencesLabels and then distincts them and takes its length
        (indices map referencesLabels).distinct.length
    }

    def mapSamples[C](f: A => C): Vector[C] = {
        // mapping the indices (Ints) to function output
        // ex. will map to boolean if function f returns a boolean
        indices map {idx => f(referenceSamples(idx))}
    }

    def combineLabels(labelCombiner: LabelCombiner[B]): B = {
        labelCombiner.combineWrapper(indices map referencesLabels)
    }
} 


object LabeledData {
    // define usual apply function
    def apply[A,B](samples: Vector[A], labels: Vector[B]): LabeledData[A,B] = {
        require(
            samples.length == labels.length,
            "Should be the same number of samples as their labels!")
        new LabeledData(samples, labels, samples.indices.toVector)
    }
}
