package Algelab

import cats.Id
import org.scalatest._

class Classic extends FlatSpec with Matchers{

  "A test" should "be true" in {
    assert(true)
  }

  type Url = String
  type Content = String
  type Status = Int

  object REST_Trait {
  trait REST {
    def get(u: Url): (Status, Content)
    def post(u: Url, c: Content): Status
  }

  object MyRest extends REST{
    def get(u: Url): (Status, Content) = return (200, u + "-content")
    def post(u: Url, c: Content): Status = return 200
  }
  }

  "With classic impl" should "work" in {
    import REST_Trait._

    assert(MyRest.get("url1")==(200, "url1-content"))
  }


  object REST_GADT {
    sealed abstract class REST[_]
    case class Get(u: Url) extends REST[(Status, Content)]
    case class Post(u: Url, c: Content) extends REST[Status]
    case class FlatMap[A,B](fa: REST[A], f: A=>REST[B]) extends REST[B]
    case class Pure[A](a:A) extends REST[A]


    trait NatTrans[-F[_], +G[_]]{
      def apply[A](fa: F[A]): G[A]
    }

    def fold[G[_], F[_], A](fa: G[A])(nat: NatTrans[G,F]): F[A] =
      nat(fa)


    val rest2IO = new NatTrans[REST, Id]{
      def apply[A](fa: REST[A]): Id[A] =
        fa match {
          case Get(u) => println(s"Get($u)")
            (200, s"content-$u")
          case Post(u, c) => println(s"Put($u, $c)")
            200
          case FlatMap(fa, f) => println(s"FlatMap($fa, $f)")
            fold(f(fold(fa)(this)))(this)
          case Pure(a) => println(s"Pure($a)")
            a
        }
    }
  }

  "test1" should "work" in {
    import REST_GADT._

    val e = FlatMap[(Status, Content), Status](Get("myurl"), { case (s,c) => Post("myurl2", c)} )

    fold(e)(rest2IO)
  }

  // It's not possible to encode this as an ADT. How to express the return value¿?
  // object REST_ADT {
    // sealed abstract class REST
    // case class Get(u: Url): (Status, Content) extends REST
    // case class Post(u: Url, c: Content): Status extends REST
  // }
}
