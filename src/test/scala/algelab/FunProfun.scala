package algelab

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dani on 16/12/2016.
  */
class FunProfun extends FlatSpec with Matchers {

  object cats {
    // https://github.com/purescript/purescript-prelude/blob/v2.1.0/src/Control/Semigroupoid.purs#L12-L13
    trait Semigroupoid[P[_, _]] {
      def >>>[A, B, C](f: P[A, B], g: P[B, C]): P[A, C]
    }

    // https://github.com/purescript/purescript-prelude/blob/v2.1.0/src/Control/Category.purs#L16-L17
    trait Category[P[_, _]] extends Semigroupoid[P] {
      def id[A]: P[A, A]
    }

    object instances {
      implicit val arrSemigroupoid = new Semigroupoid[Function1] {
        def >>>[A, B, C](f: A => B, g: B => C): A => C =
          f andThen g
      }

      implicit val arrCategory = new Category[Function1] {
        def id[A]: (A => A) = x => x
        def >>>[A, B, C](f: A => B, g: B => C): A => C =
          implicitly[Semigroupoid[Function1]].>>>(f, g)
      }
    }
  }

  object optics {

    // Adapted from: https://gist.github.com/tel/ccfb747f93b748a9a6ec3cc957886ac3
    import scala.language.higherKinds
    import scala.language.implicitConversions

    trait Optic[C[_[_, _]], S, T, A, B] {
      def apply[P[_, _]](ex: C[P])(p: P[A, B]): P[S, T]
    }

    trait Profunctor[P[_, _]] {
      def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: P[A, B]): P[X, Y]

      def lmap[X, A, B](f: X => A)(p: A P B): P[X, B] =
        dimap[X, B, A, B](f, identity)(p)

      def rmap[Y, A, B](g: B => Y)(p: P[A, B]): P[A, Y] =
        dimap[A, Y, A, B](identity, g)(p)
    }

    object instances {
      implicit val ArrProfunctor = new Profunctor[Function1] {
        def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A => B): (X => Y) =
          g.compose(p.compose(f))
      }
    }

    trait Strong[P[_, _]] extends Profunctor[P] {
      def first[X, A, B](p: P[A, B]): P[(A, X), (B, X)]
      def second[X, A, B](p: P[A, B]): P[(X, A), (X, B)]
    }

    trait Choice[P[_, _]] extends Profunctor[P] {
      def right[X, A, B](p: P[A, B]): P[Either[X, A], Either[X, B]]
      def left[X, A, B](p: P[A, B]): P[Either[A, X], Either[B, X]]
    }

    type xLens[S, T, A, B] = Optic[Strong, S, T, A, B]
    type xSLens[S, A] = xLens[S, S, A, A]

    trait Lens[S, T, A, B] { self =>
      def get(s: S): A
      def put(s: S, b: B): T
      def over(f: A => B)(s: S): T

      def compose[C, D](l2: Lens[A, B, C, D]): Lens[S, T, C, D] = new Lens[S, T, C, D] {
        def get(s: S): C = l2.get(self.get(s))

        def put(s: S, d: D): T = self.put(s, l2.put(self.get(s), d))

        def over(f: C => D)(s: S): T = self.put(s, l2.over(f)(self.get(s)))
      }
    }

    object Lens {
      object instances {
        import cats.Semigroupoid

        implicit val lensSemigroupoid = new Semigroupoid[xSLens] {
          override def >>>[A, B, C](f: xSLens[A, B], g: xSLens[B, C]) = ???
        }
      }

      def id[A]: xSLens[A, A] = new Optic[Strong, A, A, A, A] {
        def apply[P[_, _]](ex: Strong[P])(psa: P[A, A]) = psa
      }

      /*
      From packages:
        https://github.com/purescript/purescript-profunctor/blob/master/src/Data/Profunctor.purs
        https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/src/Data/Lens/Lens.purs
      */
      def lens[S, T, A, B](getter: S => A, setter: (S, B) => T): xLens[S, T, A, B] =
        new Optic[Strong, S, T, A, B] {
          def apply[P[_, _]](ex: Strong[P])(pab: P[A, B]): P[S, T] =
            ex.dimap[S, T, (A, B => T), (B, B => T)](s => (getter(s), b => setter(s, b)), {case (b, f) => f(b)})(ex.first[B => T, A, B](pab))
        }
     
      class Get[S] {
        trait ~>[A, B] extends (A => S)

        object ~> {
          def ofFunction[A, B](f: A => S): A ~> B = new ~>[A, B] {
            def apply(v: A) = f(v)
          }
        }

        val isStrong = new Strong[~>] {
          def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A ~> B): X ~> Y =
            ~>.ofFunction(p compose f)

          def first[X, A, B](p: A ~> B): (A, X) ~> (B, X) =
            ~>.ofFunction { case (a, x) => p(a) }

          def second[X, A, B](p: A ~> B): (X, A) ~> (X, B) =
            ~>.ofFunction { case (x, a) => p(a) }
        }
      }

      object Over {
        type ~>[A, B] = A => B

        val isStrong = new Strong[~>] {
          def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A ~> B): X ~> Y =
            f andThen p andThen g

          def first[X, A, B](p: A ~> B): (A, X) ~> (B, X) = {
            case (a, x) => (p(a), x)
          }

          def second[X, A, B](p: A ~> B): (X, A) ~> (X, B) = {
            case (x, a) => (x, p(a))
          }
        }
      }

      implicit def ofX[S, T, A, B](x: xLens[S, T, A, B]): Lens[S, T, A, B] =
        new Lens[S, T, A, B] {
          val G = new Get[A]
          val getter = (pab: G.~>[A, B]) => x(G.isStrong)(pab): G.~>[S, T]  // equivalent x(G.isStrong)(_)
          val overer = (pab: Over.~>[A, B]) => x(Over.isStrong)(pab): Over.~>[S, T] // equivalent x(Over.isStrong)(_)

          def get(s: S): A = getter(identity[A])(s)

          def put(s: S, b: B): T = over(_ => b)(s)

          def over(f: A => B)(s: S): T = overer(f)(s)
        }
    }

    type xPrism[S, T, A, B] = Optic[Choice, S, T, A, B]
    type xSPrism[S, A] = xPrism[S, S, A, A]

    type \/[T, A] = Either[T, A]

    trait Prism[S, A] {
      def getOrModify[T](s: S): T \/ A
      // review :: Prism' s a -> a -> s
      def reverseGet[B, T](b: B): T
      // preview :: Prism' s a -> s -> Maybe a
      def getOption(s: S): Option[A]
    }

    object Prism {
      /*
        prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
        prism to fro pab = dimap fro (either id id) (right (rmap to pab))

        // https://pursuit.purescript.org/packages/purescript-either/2.0.0/docs/Data.Either#v:either
        either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c

        // https://pursuit.purescript.org/packages/purescript-profunctor/2.0.0/docs/Data.Profunctor.Choice
        right :: forall a b c. p b c -> p (Either a b) (Either a c)
      */
      def prism[S, T, A, B](to: B => T, fro: S => Either[T, A]): xPrism[S, T, A, B] = new Optic[Choice, S, T, A, B] {
        def apply[P[_, _]](ex: Choice[P])(pab: P[A, B]): P[S, T] =
          ex.dimap[S, T, Either[T, A], Either[T, T]](fro, _.fold(identity, identity))(ex.right[T, A, T](ex.rmap[T, A, B](to)(pab)):P[Either[T, A], Either[T, T]])
      }
    }

    trait Applicative[S[_]] {
    }

    trait Traversal[S, A] {
      def modifyF[F[_]: Applicative, B, T](f: A => F[B])(s: S): F[T]
    }

    trait Monoid[S] {
    }

    trait Fold[S, A] {
      def foldMap[M: Monoid](f: A => M)(s: S): M
    }
  }

  "Testing implicit Function1 Category instance" should "work" in {
    import cats._
    import cats.instances._

    val f: (Int => String) = _.toString()
    val g: (String => String) = _ + "+1"

    val _arrCat = implicitly[Category[Function1]]

    val z = _arrCat.>>> (f, g)

    assert(z(1) === "1+1")
  }

  "Testing implicit Function1 Profunctor instance" should "work" in {
    import optics._
    import optics.instances._

    val f: (String => Int) = _.toInt
    val p: (Int => Int) = _+1
    val g: (Int => String) = _.toString

    val _arrPro = implicitly[Profunctor[Function1]]

    val z = _arrPro.dimap(f, g)(p)

    assert(z("1") === "2")
  }

  "Testing hand-made Lens[Person]" should "work" in {
    import optics._
    import optics.Lens._

    case class Age(age: Int)
    case class Person(name: String, age: Age)

    val p1 = Person("Dani", Age(40))
    val p2 = Person("Ana", Age(34))

    val pName = lens[Person, Person, String, String](_.name, { case (p, v) => p.copy(name = v) })
    val pAge = lens[Person, Person, Age, Age](_.age, { case (p, v) => p.copy(age = v) })
    val aAge = lens[Age, Age, Int, Int](_.age, { case (a, v) => a.copy(age = v) })

    assert(pName.get(p1) === "Dani")
    assert(pName.get(p2) === "Ana")
    assert(aAge.get(Age(69)) === 69)

    assert(pName.put(p1, "Ana") === Person("Ana", Age(40)))

    val personAge = ((pAge.get(_)) andThen (aAge.get(_)))(_)
    val setPerAge = (p: Person, a:Int) => pAge.put(p, aAge.put(pAge.get(p), a))

    assert(personAge(p1) === 40)
    assert(personAge(p2) === 34)
    assert(personAge(setPerAge(p1, 69)) === 69)
  }


  "Testing hand-made Lens.id[Person]" should "work" in {
    import optics._
    import optics.Lens._

    case class Age(age: Int)
    case class Person(name: String, age: Age)

    val p1 = Person("Dani", Age(40))
    val p2 = Person("Ana", Age(34))

    val iLens = Lens.id[Person]

    assert(iLens.get(p1) === p1)
    assert(iLens.put(p1, p2) === p2)
  }

  "Testing Lens composition" should "work" in {
    import optics._
    import optics.Lens._

    case class Age(years: Int)
    case class Person(name: String, age: Age)

    val p1 = Person("Dani", Age(40))
    val p2 = Person("Ana", Age(34))

    val pAge = lens[Person, Person, Age, Age](_.age, { case (p, v) => p.copy(age = v) })
    val aYears = lens[Age, Age, Int, Int](_.years, { case (a, v) => a.copy(years = v) })

    val pYears = pAge compose aYears

    assert(pYears.get(p1) === 40)

    val p11 = pYears.put(p1, 41)

    assert(pYears.get(p11) === 41)

    val p12 = pYears.over(_+2)(p1)

    assert(pYears.get(p12) == 42)
  }
}
