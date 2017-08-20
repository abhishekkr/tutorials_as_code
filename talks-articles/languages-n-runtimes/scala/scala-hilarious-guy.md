## Why Scala
youtube title: 'Why Scala... by a hilarious Indian guy' from '2009'


```
class Car(val yaear:Int, var miles: Int)
```


```
class Car(val yaear:Int, var miles: Int) {
  def drive(dist: Int) = {
    miles += dist
  }
}

var car = new Car(2010, 0)
println(car.Year)
println(car.miles)
car.drive(100)
println(car.miles)
```


```
class Car(val yaear:Int, var miles: Int) {
  println("creating car")
}

var car = new Car(2010, 0)
```
---

* Scala got Primary and Auxillary Constructors, all the calls to super can only go through primary

```
class Car(val yaear:Int, var miles: Int) {
  println("creating car")

  def this(val year: Int) = {
    this(year, 0)
  }
}

var car = new Car(2010, 0)
```
---

* singleton, a language feature

```
object Car {
}
```
---

* Companion object for use-cases of static methods, separation of concern

```
class Car {
}

object Car {
}
```
---

* ceremonious syntax

```
class Car {
  def turn(direction: String) = {
    println("turning: " + direction)
  }
}

val car = new Car();
car.turn("left");
```

same

```
class Car {
  def turn(direction: String) = {
    println("turning: " + direction)
  }
}

val car = new Car
car turn "left"
```
same
```
class Car {
  def turn(direction: String) = {
    println("turning: " + direction)
  }
}
object Car {
  def apply() = new Car
}

val car = Car()
car turn "left"
```

if you don't mention var/val, it assumes final
---

* imperative vs functional
imperative:
```
def total(list: List[Int]) = {
  var sum = 0
  for(i <- list) {  /* list.foreach {e => sum += e} */
    sum += 1
  }
  sum
}

println(total(List(1,2,3,4,5)))
```

functional:
```
def total(list: List[Int]) = {
  list.foldLeft(0) { (carryover, e) =>
    carryover + e
  }
}

println(total(List(1,2,3,4,5)))
```
> assignment less
> no side effect with functions
> referentials transparency with functions as first class citizens
> functions are higher order, can be used to compose system

```
def totalEven(list: List[Int]) = {
  var sum = 0
  list.foreach { e =>
    if (e % 2 == 0) sum += e
  }
  sum
}
```
create abstraction if need more, 
```
def totalSelectValues(list: List[Int],
  selector: Int => Boolean) = {
  var sum = 0
  list.foreach { e =>
    if (selector(e)) sum += e
  }
  sum
}

println(totalSelectValues(List(1,2,3,4,5), {e => true}))
println(totalSelectValues(List(1,2,3,4,5), {e => e % 2 == 0}))
println(totalSelectValues(List(1,2,3,4,5), {e => e % 2 != 0}))
println(totalSelectValues(List(1,2,3,4,5), {e => e > 5}))
println(totalSelectValues(List(1,2,3,4,5), {_ > 5}))
```

---

* Inferential Typing

```
var str = "hello"
println(str)
```

---

* Pattern Matching

---

* Traits

> Multiple Inheritance
```
class Human {
  def listen = println("I'm " + name " your friend.")
}

val peter = new Human("Peter")
peter.listen
```
as
```
trait Friend {
  val name: String
  def listen = println("I'm " + name " your friend.")
}

class Human(val name: String) extends Friend

val peter = new Human("Peter")
peter.listen

def seekHelp(friend: Friend) = friend.listen
seekHelp(peter)

class Animal(val name: String)

class Dog(override val name:String) extends Animal(name) with Friend
val rover = new Dog("Rover")
rover.listen
seekHelp(rover)

class Cat(override val name:String) extends Animal(name)
val snow = new Cat("Snow") with Friend
snow.listen
seekHelp(snow)
```

---

* XML

a first class citizen
```
import scala.io._
import scala.xml._

var xml = <hello></hello>
println(xml)

def getWeatherInfo(woeid: String) = {
  val url = "http://weather.yahooapis.com/forecastrss?w=" + woeid + "&u=f" //2391271
  val response = Source .fromURL(url).mkString
  println(response)

  val xmlResponse = XML.loadString(response)
  println(xmlResponse \\ "location" \\ "@city", xmlResponse \\ "condition" \\ "@temp")
  (xmlResponse \\ "location" \\ "@city",
   xmlResponse \\ "location" \\ "@region"
   xmlResponse \\ "condition" \\ "@temp")
}

getWeatherInfo("2391271")

var start = System.nanoTime
for(id <- 2391271 to 2391279) {
  println(getWeatherInfo(id.toString()))
}
var end = System.nanoTime

println("Time " + (end - start)/1000000000.0)
```
---

* Concurrency

improve with actors (threads with own msg-queue)
```
import scala.io._
import scala.xml._
import scala.actor._
import Actor._

var xml = <hello></hello>
println(xml)

def getWeatherInfo(woeid: String) = {
  val url = "http://weather.yahooapis.com/forecastrss?w=" + woeid + "&u=f" //2391271
  val response = Source .fromURL(url).mkString
  println(response)

  val xmlResponse = XML.loadString(response)
  println(xmlResponse \\ "location" \\ "@city", xmlResponse \\ "condition" \\ "@temp")
  (xmlResponse \\ "location" \\ "@city",
   xmlResponse \\ "location" \\ "@region"
   xmlResponse \\ "condition" \\ "@temp")
}

var start = System.nanoTime
val caller = self
for(id <- 2391271 to 2391279) {
  actor {
    caller ! getWeatherInfo(id.toString())
  }
}

for(id <- 2391271 to 2391279) {
  receiveWithin(3000) {
    case msg => println(msg)
  }
}
var end = System.nanoTime

println("Time " + (end - start)/1000000000.0)
```

---
---
