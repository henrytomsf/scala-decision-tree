package main


import ml.dt._


object Main extends App {
    val url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data"
    val data = scala.io.Source.fromURL(url).getLines.withFilter(_.nonEmpty).toVector
    val splitData = (data map { _.split(',').toVector.splitAt(4) }).unzip
    val (samples, labels) = (splitData._1 map { v => v map (_.toDouble) }, splitData._2 map {_ (0) })
    val dataSet = LabeledData[Vector[Double], String](samples, labels)
    val dataSetSize = dataSet.size

    println(s"Size of dataset is $dataSetSize.")
}