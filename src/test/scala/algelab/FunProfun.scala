package algelab

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dani on 16/12/2016.
  */
class FunProfun extends FlatSpec with Matchers {

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

    type xPrism[S, T, A, B] = Optic[Choice, S, T, S, B]
    type xSPrism[S, A] = xPrism[S, S, A, A]

    /*
    prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
    prism to fro pab = dimap fro (either id id) (right (rmap to pab))

    prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
    prism' to fro = prism to (\s -> maybe (Left s) Right (fro s))



    object Prism {
      def prism[S, T, A, B](getter: B => T, setter: S => Either[T, A]): xPrism[S, T, A, B] = new Optic[Choice, S, T, A, B] {
        def apply[~>[_, _]](ex: Choice[~>])(pab: A ~> B): S ~> T =
          ex.dimap[S, T, S => Either[T, A], _](setter, getter(p)), t => t._1(t._2))(ex.[A => S, A, A](pab))
      }
    }*/

    trait Lens[S, T, A, B] {
      def get(s: S): A

      def put(s: S, b: B): T

      def over(f: A => B)(s: S): T
    }

    object Lens {
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
            ex.dimap[S, T, (A, B => T), (B, B => T)](p => (getter(p), (s => setter(p, s))), t => t._2(t._1))(ex.first[B => T, A, B](pab))
        }
     
      class Get[S] {

        trait ~>[A, B] extends (A => S)

        object ~> {
          implicit def ofFunction[A, B](f: A => S): A ~> B = new ~>[A, B] {
            def apply(v: A) = f(v)
          }
        }


        val isStrong = new Strong[~>] {

          def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A ~> B): X ~> Y =
            p compose f

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
          val getter = x(G.isStrong)(_)
          val overer = x(Over.isStrong)(_)

          def get(s: S): A = getter(identity[A])(s)

          def put(s: S, b: B): T = over(_ => b)(s)

          def over(f: A => B)(s: S): T = overer(f)(s)
        }
    }

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
}
