package task02

object Main extends App:

// 3a) 
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


// 3b)
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

//3c)
//Make neg work for generic predicates, and write tests to check it (therefore, neg will be generic: def neg[X]...).
    
    def negMethodStyleGeneric[A] (s: A => Boolean) : A => Boolean = !s(_)



//4) Currying
// Implement a predicate that checks whether its arguments x,y,z respect
// the relation x ≤ y = z, in 4 variants (curried/non-curried × val/def)

    def xyzStandard(x: Double, y: Double, z: Double): Boolean = x <= y && y == z 

    def xyzCurryed(x: Double) (y: Double) (z: Double): Boolean = x <= y && y == z 


    val xyzLambdaStandard: (Double, Double, Double) => Boolean = (x, y, z) => x <= y && y == z

    val xyzLambdaCurryed: Double => Double => Double => Boolean = x => y => z => x <= y && y == z 

    
//5)
//Create a function that implements functional compositions (f ◦ g)(x) = f (g(x))
// ▶ Signature: compose(f: Int => Int, g: Int => Int): Int => Int ▶ Example: compose(_ - 1, _ * 2)(5) // 9
// ▶ Create a generic version of compose
// What signature? Is there any constraint?

    def compose(f: Int => Int, g: Int => Int): Int => Int = i => f(g(i))
    
    def genericCompose[A, B, C](f: B => C, g: A => B): A => C = i => f(g(i))



    