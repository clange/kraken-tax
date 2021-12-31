import com.github.tototoshi.csv._

@main def hello: Unit = 
  val reader = CSVReader.open("trades.csv")
  println("Hello world!")
  reader.close()
