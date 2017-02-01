package algelab

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dani on 08/01/2017.
  */
object FunTransformer {

  trait MyIO[F[_]] {
    def put[A](v: A): F[Unit]

    def get(): F[String] // TODO: what about to make get polymorphic¿?
  }

  object MyIO {
  }

  type Id[A] = scalaz.Id.Id[A]

  trait Fu[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Mo[F[_]] {
    def bind[A, B](fa: F[A])(f: (A => F[B])): F[B]
  }
}

class FunTransformerSpec extends FlatSpec with Matchers {

  "scala for-comprehention flatMap" should "be ok" in {
    class C[+A](val a:A) {
      def flatMap[B](f:(A) => C[B]):C[B] = f(a)
      def map[B](f:(A)=>B):B = f(a)
    }

    val t: C[Int] = for {
      k <- new C(3)
      r <- new C(2)
    } yield new C(r)
  }

  "MyIO must accept for-comprehentions" should "work with scalaz.Monad" in {
    import FunTransformer._

    import scalaz.Monad
    import scalaz.Id.Id, scalaz.syntax.monad._

    val _mio = new MyIO[Id] {
      override def get(): Id[String] = "hola"
      override def put[A](v: A): Id[Unit] = println(v)
    }

    def _program[F[_]: MyIO : Monad](): F[String] =
      for {
        v <- implicitly[MyIO[F]].get()
        _ <- implicitly[MyIO[F]].put(v)
      } yield v

    _program()(_mio, Monad[Id])
  }

  "MyIO must accept for-comprehentions" should "work with custom monad" in {
    import FunTransformer._

    import scalaz.Id.Id
    import scala.language.reflectiveCalls

    implicit val _mio = new MyIO[Id] {
      override def get() = "hola"
      override def put[A](v: A): Id[Unit] = println(v)
    }

    class MID[F[_], A](ia: Id[A]) extends Mo[Id] with Fu[Id] {
      override def fmap[B, C](fa: Id[B])(f: (B) => C): Id[C] = f(fa)

      def map[B](f: A => B): Id[B] = fmap(ia)(f)

      override def bind[B, C](fb: Id[B])(fbc: (B) => Id[C]): Id[C] = fbc(fb)

      def flatMap[B](fab: (A) => Id[B]): Id[B] = bind(ia)(fab)
    }

    implicit def mid[A](ia: Id[A]) = new MID(ia)

    def _program[F[_]: MyIO]: F[String] = {
      def F = implicitly[MyIO[F]]
      for {
        v <- F.get()
        _ <- F.put(v)
      } yield v
    }

    mid(_mio.put("hola")).flatMap(identity(_))

    _program[Id](_mio)
  }

  "herding cats — Monad transformers example" should "work" in {
    case class User(id: Long, parentId: Long, name: String, email: String)

    import java.net.URI

    trait HttpService {
      def get(uri: URI): String
    }

    trait UserRepo {
      def get(id: Long): Option[User]
      def find(name: String): Option[User]
    }
    trait Config {
      def userRepo: UserRepo
      def httpService: Option[HttpService]
    }

    import _root_.cats._
    import _root_.cats.data._
    import _root_.cats.implicits._
    import _root_.cats.instances.all._
    import _root_.cats.syntax.all._
    import _root_.cats.instances.all.catsStdInstancesForOption

    type ReaderTOption[A, B] = Kleisli[Option, A, B] // A => Option[B]

    object ReaderTOption {
      def ro[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
    }

    trait Users {
      def getUser(id: Long): ReaderTOption[Config, User] =
        ReaderTOption.ro {
          case config => config.userRepo.get(id)
        }
      def findUser(name: String): ReaderTOption[Config, User] =
        ReaderTOption.ro {
          case config => config.userRepo.find(name)
        }
    }
    trait Https {
      def getHttp(uri: URI): ReaderTOption[Config, String] =
        ReaderTOption.ro {
          case config => config.httpService map {_.get(uri)}
        }
    }

    trait Program extends Users with Https {
      def userSearch(id: Long): ReaderTOption[Config, String] =
        for {
          u <- getUser(id)
          r <- getHttp(new URI(s"http://www.google.com/?q=${u.name}"))
        } yield r
    }

    type StateTReaderTOption[C, S, A] = StateT[ReaderTOption[C, ?], S, A]

    object StateTReaderTOption {
      def state[C, S, A](f: S => (S, A)): StateTReaderTOption[C, S, A] =
        StateT[ReaderTOption[C, ?], S, A] {
          s: S => Monad[ReaderTOption[C, ?]].pure(f(s))
        }
      def get[C, S]: StateTReaderTOption[C, S, S] =
        state { s => (s, s) }
      def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
        state { _ => (s, ()) }
      def ro[C, S, A](f: C => Option[A]): StateTReaderTOption[C, S, A] =
        StateT[ReaderTOption[C, ?], S, A] {
          s: S =>
            ReaderTOption.ro[C, (S, A)]{
              c: C => f(c) map {(s, _)}
            }
        }
    }

    type Stack = List[String]

    val pop: StateTReaderTOption[Config, Stack, String] =
      for {
        s <- StateTReaderTOption.get[Config, Stack]
        (x :: xs) = s
        _ <- StateTReaderTOption.put(xs)
      } yield x

    def push(x: String): StateTReaderTOption[Config, Stack, Unit] =
      for {
        xs <- StateTReaderTOption.get[Config, Stack]
        r <- StateTReaderTOption.put(x :: xs)
      } yield r

    def stackManip: StateTReaderTOption[Config, Stack, String] =
      for {
        _ <- push("Fredo")
        a <- pop
        b <- pop
      } yield(b)


    val dummyConfig: Config = new Config {
      val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
        User(1, 0, "Michael", "michael@example.com"),
        User(2, 0, "Fredo", "fredo@example.com"))
      def userRepo: UserRepo = new UserRepo {
        def get(id: Long): Option[User] =
          testUsers find { _.id === id }
        def find(name: String): Option[User] =
          testUsers find { _.name === name }
      }
      def httpService: Option[HttpService] = None
    }

    stackManip.run(List("Hyman Roth")).run(dummyConfig) shouldBe Some((List(), "Hyman Roth"))

    // Simplifyied MonadTransformer concept:
    // TypeT[F[_], A] wraps F[Type[A]]
    // For example, OptionT[List, Int] wraps List[Option[Int]]
  }

  "Applicative composition" should "work" in {
    import _root_.cats._
    import _root_.cats.implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import scala.util.Try
    import scala.util.Success

    val _appl1 = Applicative[Try] compose Applicative[Option]

    _appl1.map2(Success(Some(1)), Success(Some(2))){_ + _} shouldBe Success(Some(3))

    val _appl2 = Applicative[Future] compose Applicative[Option]

    _appl2.map2(Future(Some(1)), Future(Some(2))){_ + _} map { _ shouldEqual Some(3) }
  }
}
