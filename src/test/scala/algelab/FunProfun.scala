package algelab

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dani on 16/12/2016.
  */
class FunProfun extends FlatSpec with Matchers {

  object gist {

    // Adapted from: https://gist.github.com/tel/ccfb747f93b748a9a6ec3cc957886ac3
    import scala.language.higherKinds
    import scala.language.implicitConversions

    trait Optic[C[_[_, _]], S, T, A, B] {
      def apply[~>[_, _]](ex: C[~>])(p: A ~> B): S ~> T
    }

    trait Profunctor[~>[_, _]] {
      def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A ~> B): X ~> Y

      def lmap[X, A, B](f: X => A)(p: A ~> B): X ~> B =
        dimap[X, B, A, B](f, identity)(p)

      def rmap[Y, A, B](g: B => Y)(p: A ~> B): A ~> Y =
        dimap[A, Y, A, B](identity, g)(p)
    }

    trait Strong[~>[_, _]] extends Profunctor[~>]  {
      def first[X, A, B](p: A ~> B): (X, A) ~> (X, B)
    }

    trait Choice[~>[_, _]] extends Profunctor[~>]  {
      def left[X, A, B](p: A ~> B): Either[X, A] ~> Either[X, B]
    }

    type xLens[S, T, A, B] = Optic[Strong, S, T, A, B]
    type xSLens[S, A] = xLens[S, S, A, A]

    trait Lens[S, T, A, B] {
      def get(s: S): A

      def put(s: S, b: B): T

      def over(f: A => B)(s: S): T
    }

    object Lens {
      def id[A]: xSLens[A, A] = new Optic[Strong, A, A, A, A] {
        def apply[~>[_, _]](ex: Strong[~>])(psa: ~>[A, A]) = psa
      }

      class Get[S] {

        trait ~>[A, B] extends (A => S)

        object ~> {
          implicit def ofFunction[A, B](f: A => S): ~>[A, B] = new ~>[A, B] {
            def apply(v: A) = f(v)
          }
        }


        val isStrong: Strong[~>] = new Strong[~>] {

          def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A ~> B): X ~> Y =
            p compose f

          def first[X, A, B](p: A ~> B): (X, A) ~> (X, B) =
            ~>.ofFunction { case (x, a) => p(a) }

        }
      }

      object Over {
        type ~>[A, B] = A => B

        val isStrong: Strong[~>] = new Strong[~>] {
          def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A ~> B): X ~> Y =
            f andThen p andThen g

          def first[X, A, B](p: A ~> B): (X, A) ~> (X, B) = {
            case (x, a) => (x, p(a))
          }
        }
      }

      implicit def ofX[S, T, A, B](x: xLens[S, T, A, B]): Lens[S, T, A, B] =
        new Lens[S, T, A, B] {
          val G = new Get[A]
          type ~>[X, Y] = G.~>[X, Y]
          val getter = x.apply(G.isStrong)(_)
          val overer = x.apply(Over.isStrong)(_)

          def get(s: S): A = getter(identity[A])(s)

          def put(s: S, b: B): T = over(_ => b)(s)

          def over(f: A => B)(s: S): T = overer(f)(s)
        }
    }

  }

  "Testing hand-made Lens[Person]" should "work" in {
    import gist._
    import gist.Lens._

    case class Age(age: Int)
    case class Person(name: String, age:Age)

    val p1 = Person("Dani", Age(40))
    val p2 = Person("Ana", Age(34))

    /*
    From packages:
      https://github.com/purescript/purescript-profunctor/blob/master/src/Data/Profunctor.purs
      https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/src/Data/Lens/Lens.purs

    dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d

    dimap (\s -> ( (get s), \b -> set s b) (\(b, f) -> f b) (first pab)

    lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
    lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

    -- | Create a `Lens` from a getter/setter pair.
    lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
    lens get set = lens' \s -> Tuple (get s) \b -> set s b
    */
    def pLens(getter: Person=>String, setter:(Person, String)=>Person): xSLens[Person, String] =
      new Optic[Strong, Person, Person, String, String] {
        def apply[~>[_, _]](ex: Strong[~>])(pab: ~>[String, String]) =
          ex.dimap[Person, Person, (String=>Person, String), (String=>Person, String)](p => (((s:String) => setter(p, s)), getter(p)), t => t._1(t._2))(ex.first[String=>Person, String, String](pab))
    }

    val plen = pLens(_.name, { case (p: Person,s: String)=> p.copy(name=s) })

    assert(plen.get(p1) === "Dani")
    assert(plen.get(p2) === "Ana")

    assert(plen.put(p1, "Ana") === Person("Ana", Age(40)))
  }


  "Testing hand-made Lens.id[Person]" should "work" in {
    import gist._
    import gist.Lens._

    case class Age(age: Int)
    case class Person(name: String, age:Age)

    val p1 = Person("Dani", Age(40))
    val p2 = Person("Ana", Age(34))

    val iLens = Lens.id[Person]

    println(p1)
    assert(iLens.get(p1) === p1)

    assert(iLens.put(p1, p2) === p2)
    println(iLens.put(p1, p2))
  }
}
