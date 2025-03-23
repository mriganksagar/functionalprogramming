package module1

trait Generator[+T]: 
    def generate(): T
    def map[S](f: T => S) = new Generator[S]:
        def generate() = f(Generator.this.generate())

    def flatMap[S](f: T => Generator[S]) = f(Generator.this.generate())


object Generator{
    val integers: Generator[Integer] = new Generator[Integer] {
        val random = java.util.Random()
        def generate(): Integer = random.nextInt()
    }

    val booleans_0: Generator[Boolean] = new Generator[Boolean] {
        def generate(): Boolean = integers.generate() > 0
    }

    val booleans = for i <- integers yield i > 0
    
    val pairs_0: Generator[(Int, Int)] = new Generator[(Int, Int)] {
        def generate(): (Int, Int) = (integers.generate(), integers.generate())
    }

    def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
        x <- t
        y <- u
    } yield (x, y)
}


// we should avoid boilerplate
// lets add flatmap and map methods
