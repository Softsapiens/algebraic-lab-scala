package algelab

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dani on 08/01/2017.
  */
object FunTransformer {

  type Id[A] = scalaz.Id.Id[A]

  trait MyIO[F[_]] {
    def put[A](v: A): F[Unit]

    def get(): F[String] // TODO: what about to make get polymorphic¿?
  }

  trait Fu[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Mo[F[_]] {
    def bind[A, B](fa: F[A])(f: (A => F[B])): F[B]
  }

  object MyIO {
  }

}

class FunTransformerSpec extends FlatSpec with Matchers {

  "scala for-comprehention flatMap" should "be ok" in {
    class C[+A](val a: A) {
      def flatMap[B](f: (A) => C[B]): C[B] = f(a)

      def map[B](f: (A) => B): B = f(a)
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

    def _program[F[_] : MyIO : Monad](): F[String] =
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

    def _program[F[_] : MyIO]: F[String] = {
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
          case config => config.httpService map {
            _.get(uri)
          }
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
            ReaderTOption.ro[C, (S, A)] {
              c: C => f(c) map {
                (s, _)
              }
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
      } yield (b)


    val dummyConfig: Config = new Config {
      val testUsers = List(User(0, 0, "Vito", "vito@example.com"),
        User(1, 0, "Michael", "michael@example.com"),
        User(2, 0, "Fredo", "fredo@example.com"))

      def userRepo: UserRepo = new UserRepo {
        def get(id: Long): Option[User] =
          testUsers find {
            _.id === id
          }

        def find(name: String): Option[User] =
          testUsers find {
            _.name === name
          }
      }

      def httpService: Option[HttpService] = None
    }

    stackManip.run(List("Hyman Roth")).run(dummyConfig) shouldBe Some((List(), "Hyman Roth"))

    // Simplifyied MonadTransformer concept:
    // TypeT[F[_], A] wraps F[Type[A]]
    // For example, OptionT[List, Int] wraps List[Option[Int]]
  }

  /*
  Based on https://rubenpieters.github.io/monadtransformer/cats/eff/2017/01/27/monadtransformer-vs-effmonad-1.html
   */
  "Another example of monad transformers by rubenpieters" should "work" in {
    import _root_.cats._
    import _root_.cats.data._
    import _root_.cats.implicits._

    type MonadStackStateTEither[ErrorType, StateType, ReturnType] =
      StateT[Either[ErrorType, ?], StateType, ReturnType]

    def decrMonadStackStateTEither: MonadStackStateTEither[String, Int, Unit] = for {
      x <- StateT.get[Either[String, ?], Int]
      _ <- if (x > 0) {
        StateT.set[Either[String, ?], Int](x - 1)
      }
      else {
        StateT.lift[Either[String, ?], Int, Unit](Left("error"))
      }
    } yield ()

    type MonadStackEitherTState[ErrorType, StateType, ReturnType] =
      EitherT[State[StateType, ?], ErrorType, ReturnType]

    def decrMonadStackEitherTState: MonadStackEitherTState[String, Int, Unit] = for {
      x <- EitherT.liftT[State[Int, ?], String, Int](State.get[Int])
      _ <- if (x > 0) {
        EitherT.liftT[State[Int, ?], String, Unit](State.set(x - 1))
      }
      else {
        EitherT.left[State[Int, ?], String, Unit](State.pure("error"))
      }
    } yield ()

    val resultStateTEither: Either[String, (Int, Unit)] =
      decrMonadStackStateTEither.run(0)

    resultStateTEither shouldBe Left("error")

    val resultEitherTState: (Int, Either[String, Unit]) =
      decrMonadStackEitherTState.value.run(0).value

    resultEitherTState shouldBe(0, Left("error"))
  }

  "The previous example bu using an MTL approx" should "work too" in {
    import _root_.cats._
    import _root_.cats.data._
    import _root_.cats.implicits._

    def decrMtlStateError[F[_]](implicit
                                ms: MonadState[F, Int],
                                me: MonadError[F, String]): F[Unit] = {
      ms.flatMap(ms.get) { x =>
        if (x > 0) {
          ms.set(x - 1)
        }
        else {
          me.raiseError("error")
        }
      }
    }

    val resultMtlStateTEither: Either[String, (Int, Unit)] =
      decrMtlStateError[StateT[Either[String, ?], Int, ?]].run(0)

    resultMtlStateTEither shouldBe Left("error")

    implicit def monadStateEitherT[F[_], E, S](implicit ms: MonadState[F, S]): MonadState[EitherT[F, E, ?], S] =
      new MonadState[EitherT[F, E, ?], S] {
        val F = Monad[F]

        override def get: EitherT[F, E, S] = EitherT.liftT(ms.get)

        override def set(s: S): EitherT[F, E, Unit] = EitherT.liftT(ms.set(s))

        // copied from cats EitherTMonad
        override def pure[A](a: A): EitherT[F, E, A] = EitherT(F.pure(Either.right(a)))

        override def flatMap[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] = fa flatMap f

        override def tailRecM[A, B](a: A)(f: A => EitherT[F, E, Either[A, B]]): EitherT[F, E, B] =
          EitherT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
            case Left(l) => Right(Left(l))
            case Right(Left(a1)) => Left(a1)
            case Right(Right(b)) => Right(Right(b))
          }))
      }

    val resultMtlEitherTState: (Int, Either[String, Unit]) =
      decrMtlStateError[EitherT[State[Int, ?], String, ?]].value.run(0).value

    resultMtlEitherTState shouldBe(0, Left("error"))
  }

  "Functor composition" should "work" in {
    import _root_.cats._
    import _root_.cats.implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import scala.util.Try
    import scala.util.Success

    val _compo1 = Functor[Try] compose Functor[Option]

    _compo1.map(Success(Some(1))) {
      _ + 1
    } shouldBe Success(Some(2))

    val _compo2 = Functor[Future] compose Functor[Option]

    _compo2.map(Future(Some(1))) {
      _ + 1
    } map {
      _ shouldEqual Some(2)
    }
  }

  "Applicative composition" should "work" in {
    import _root_.cats._
    import _root_.cats.implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import scala.util.Try
    import scala.util.Success

    val _appl1 = Applicative[Try] compose Applicative[Option]

    _appl1.map2(Success(Some(1)), Success(Some(2))) {
      _ + _
    } shouldBe Success(Some(3))

    val _appl2 = Applicative[Future] compose Applicative[Option]

    _appl2.map2(Future(Some(1)), Future(Some(2))) {
      _ + _
    } map {
      _ shouldEqual Some(3)
    }
  }

  "Kleisli composition" should "work" in {
    import _root_.cats._
    import _root_.cats.implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    import scala.util.Try
    import scala.util.Success

    val _compo1 = Applicative[Try] compose Applicative[Option]
  }

  /*
  Based on http://www.slideshare.net/TomaszKogut/bestiary-of-functional-programming-with-cats
   */
  "A simple pure RPC service as Kleisli structure" should "work" in {
    import _root_.cats._
    import _root_.cats.data._
    import _root_.cats.implicits._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    object http {

      sealed class Method

      final object Get extends Method

      final object Post extends Method

      type URI = String

      sealed case class Request(method: Method, uri: URI)

      sealed class HttpCode

      final object Ok extends HttpCode

      final object NotFound extends HttpCode

      type Service[A, B] = Kleisli[Future, A, B]
      type HttpService = Service[Request, Response] //Future[Either[A, B]] type DecodeResult[T] = EitherT[Future, DecodeFailure, T]
    }

    import http._

    trait Message

    case class Response(code: HttpCode) extends Message {
      def body[A](implicit decoder: EntityDecoder[A]): DecodeResult[A] = decoder.decode(this)
    }

    // Decoding
    trait EntityDecoder[T] {
      self =>
      def decode(msg: Message): DecodeResult[T]

      def map[T2](f: T => T2): EntityDecoder[T2] = new EntityDecoder[T2] {
        override def decode(msg: Message): DecodeResult[T2] = self.decode(msg).map(f)
      }
    }

    type DecodeFailure = List[String]
    type DecodeResult[T] = EitherT[Future, DecodeFailure, T]

    object EntitiyDecoder {

      import Json._

      implicit def stringInstance = new EntityDecoder[String] {
        def decode(msg: Message): DecodeResult[String] = EitherT.pure[Future, DecodeFailure, String]("SomeString")
      }

      implicit def jsonInstance: EntityDecoder[Json] = stringInstance.map(_.toJson)
    }

    trait Json

    object Json {
      implicit def fromString(s: String): JsonOps = JsonOps(s)

      case class JsonOps(s: String) {
        def toJson = new Json {}
      }

    }

    object Service {
      def lift[A, B](f: A => Future[B]): Service[A, B] = Kleisli(f)
    }

    object HttpService {
      def apply(f: PartialFunction[Request, Response]): HttpService = Service.lift(liftToAsync(f))

      def liftToAsync[A, B](f: A => B): A => Future[B] = (a: A) => Future(f(a))
    }

    val httpService = HttpService {
      case r1@Request(Get, "/") => Response(Ok)
      case r2@Request(Post, "/") => Response(NotFound)
    }

    // Http.runService(httpService) // Server

    import EntitiyDecoder._

    val jsonResponseFromPipeline = httpService.map(_.body[Json])
    val jsonFut: Future[DecodeResult[Json]] = jsonResponseFromPipeline(Request(Get, "/"))

    class AHClient

    class AHClientWrapper(realClient: AHClient) extends (Request => Future[Response]) {
      def apply(req: Request): Future[Response] = ??? //call realClient and return response
    }

    val httpClient: HttpService = Kleisli(new AHClientWrapper(new AHClient))

    // Client

    httpService.map(_.body[Json]) // Kleisli[Future, Request, Json]
  }
}
