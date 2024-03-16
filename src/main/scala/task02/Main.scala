package task02

object Main extends App:

    def getTheSignMethodStyle(x: Int): String = x match
        case x if x >= 0 => "positive"
        case _ => "negative"

    val getTheSignLambdaStyle: Int => String = _ match
        case x if x >= 0 => "positive"
        case _ => "negative"