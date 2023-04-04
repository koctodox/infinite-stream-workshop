import scala.collection.WithFilter

object LazyEvaluation extends App {

  def zeroStep: Unit = {
    val strictVal: Int = {
      println("In the strictVal")
      1
    }

    def strictDef: Int = {
      println("In the strictDef")
      2
    }

    lazy val lazyVal: Int = {
      println("In the lazyVal")
      3
    }
    strictVal
    //    strictDef
    println(lazyVal)

    lazy val exception = throw new RuntimeException("Chert!")
    1
    //    exception
  }
  //  zeroStep

  def _1: Unit = {
    println("_1() starts")

    val i: (Int, Int) => Int = { (i, _) =>
      println("in the i function literal ")
      i * 2
    }

    def power(x: Int, y: Int)(i: (Int, Int) => Int): Int = {
      println(" in the power! ")
      i(x, y)
    }

    power(1, 2)(i)
  }
  //_1

  def _2: Unit = {
    if (
      true || {
        println("before false");
        false
      }
    ) {
      println("In the if scope")
    } else {
      println("In the else scope")
    }

    def if2[A](predicate: => Boolean, onTrue: => A, onFalse: => A): A = {
      println("before if")
      if (predicate) onTrue else onFalse
    }

    def booleanFunc: Boolean = {
      println("beforeTrue");
      true
    }

    val ifResult = if2(
      booleanFunc,
      1,
      0
    )

    println(ifResult)
  }
  //  _2

  def _4: Unit = {
    def graterThan20(i: Int): Boolean = {
      println(s"[graterThan20] (side effect!) $i")
      i > 20
    }

    def lessThan30(i: Int): Boolean = {
      println(s"[lessThan30] (side effect!) $i")
      i < 30
    }

    val list = List(22, 18, 54, 27, 24, 1, 100)
    val gt20 = list.filter(graterThan20)
    println(gt20)
    val gt20_lt30 = gt20.filter(lessThan30)
    println(gt20_lt30)
    println()
    println()

    val lazy_gt20 = list.withFilter(graterThan20)
    val lazy_gt20_lt30: WithFilter[Int, List] = lazy_gt20.withFilter(lessThan30)

    println("This may not make any sense! reference of with filter" + lazy_gt20_lt30)
    println()
    lazy_gt20_lt30.foreach(println) // run by need!

  }
  //_4

  def _5: Unit = { //for-comperhentions use with filter with gards
    val list = List(22, 18, 54, 27, 24, 1, 100)

    for {
      a <- list if a % 2 == 0 //this use lazy vals
    } yield a + 1
    //its equal to list(1,2,3) withFilter (_ % 2 == 0).map(_ +1)
  }
  //  _5

}
