package wikigraph

import com.sun.net.httpserver.Authenticator.Success

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

object Async:

  /**
    * Transforms a successful future value of type `Int` into a
    * successful future value of type `Boolean`, indicating whether
    * the number was even or not.
    *
    * In case the given future value failed, this method should
    * return a failed future with the same error.
    */
  def transformSuccess(eventuallyX: Future[Int]): Future[Boolean] =
    eventuallyX.map(n => n % 2 == 0)

  /**
    * Transforms a failed future value of type `Int` into a successful
    * one returning `-1`.
    *
    * Any non-fatal failure should be recovered.
    *
    * In case the given future value was successful, this method should
    * return a successful future with the same value.
    */
  def recoverFailure(eventuallyX: Future[Int]): Future[Int] =
    eventuallyX.recover { case NonFatal(_) => -1 }

  /**
    * Performs two asynchronous computation, one after the other.
    * `asyncComputation2` should start ''after'' the `Future` returned
    * by `asyncComputation1` has completed.
    *
    * In case the first asynchronous computation failed, the second one
    * should not even be started.
    *
    * The returned `Future` value should contain the successful result
    * of the first and second asynchronous computations, paired together.
    */
  def sequenceComputations[A, B](
    asyncComputation1: () => Future[A],
    asyncComputation2: () => Future[B]
  ): Future[(A, B)] =
    asyncComputation1().zip(asyncComputation1().flatMap(_ => asyncComputation2()))

  /**
    * Concurrently performs two asynchronous computations and pair their
    * successful results together.
    *
    * The two computations should be started independently of each other.
    *
    * If one of them fails, this method should return the failure.
    */
  def concurrentComputations[A, B](
    asyncComputation1: () => Future[A],
    asyncComputation2: () => Future[B]
  ): Future[(A, B)] =
    asyncComputation1().zip(asyncComputation2())

  /**
    * Makes a chocolate cake.
    *
    * Combine the ingredients (`butter`, `eggs`, `chocolate`, and
    * `sugar`) by using the actions `meltButterWithChocolate`,
    * `mixEverything`, and `bake` to return a `Future[Cake]`.
    */
  def makeCake[Butter, Eggs, Chocolate, Sugar, MeltedButterAndChocolate, CakeDough, Cake](
    butter: Butter,
    eggs: Eggs,
    chocolate: Chocolate,
    sugar: Sugar,
    meltButterWithChocolate: (Butter, Chocolate) => Future[MeltedButterAndChocolate],
    mixEverything: (MeltedButterAndChocolate, Eggs, Sugar) => Future[CakeDough],
    bake: CakeDough => Future[Cake]
  ): Future[Cake] =
    meltButterWithChocolate(butter, chocolate)
      .flatMap(m => mixEverything(m, eggs, sugar))
      .flatMap(cd => bake(cd))

  /**
    * Attempts to perform an asynchronous computation at most
    * `maxAttempts` times.
    *
    * In case of failure this method should try again to run the
    * asynchronous computation so that at most `maxAttempts` are
    * eventually performed.
    *
    * Hint: recursively call `insist` in the failure handler.
    */
  def insist[A](asyncComputation: () => Future[A], maxAttempts: Int): Future[A] =
    if maxAttempts == 0 then
      Future.failed(Exception("Failed after maxAttempts"))
    else
      asyncComputation().recoverWith{ case NonFatal(_) =>
        insist(asyncComputation, maxAttempts -1)
      }
  
end Async
