package task02

object Main extends App:

// a) 
// Using match-cases, implement the following function from Int to
// String: positive(x) =
// (“positive” if x ≥ 0 “negative” if x < 0
// in both of the following styles: (i) val assigned to function literal
// (lambda) and (ii) method syntax.

    def getTheSignMethodStyle(x: Int): String = x match
        case x if x >= 0 => "positive"
        case _ => "negative"

    val getTheSignLambdaStyle: Int => String = _ match
        case x if x >= 0 => "positive"
        case _ => "negative"


// b)
// Implement a neg function that accepts a predicate on strings (i.e., a
// function from strings to Booleans) and returns another predicate on strings, namely, one that does the exact opposite;
// write the type first, and then define the function both as a val lambda and with method syntax val empty:
// String => Boolean = _ == "" // predicate on strings
// val notEmpty = neg(empty) // which type of notEmpty?
// notEmpty("foo") // true
// notEmpty("") // false
// notEmpty("foo") && !notEmpty("") // true.. a comprehensive test

    val emptyString: String => Boolean = _ == ""

    println(emptyString(""))

    def negMethodStyle (s: String => Boolean) : String => Boolean = !s(_)

    val negLambdaStyle: (String => Boolean) => String => Boolean = s => !s(_)




    