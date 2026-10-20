package triage4s.javaapi

import java.util.Optional

/** The outcome of a triage call from Java (#97): a value, or an error with its kind and message. It never
  * throws while it is being produced; only [[get]] throws, and only when called on a failure.
  */
final class JResult[T] private (value: Option[T], failure: Option[(JErrorKind, String)]):

  /** Whether there is a value. */
  def isSuccess(): Boolean = value.isDefined

  /** The value; on a failure, throws `IllegalStateException` with the error's message. */
  def get(): T = value.getOrElse(
    throw new IllegalStateException(s"${failure.map(_._1).orNull}: ${failure.map(_._2).orNull}")
  )

  /** The value, or `null` on a failure. */
  def getOrNull(): T = value.getOrElse(null.asInstanceOf[T])

  /** The error's kind, empty on a success. */
  def errorKind(): Optional[JErrorKind] = failure.fold(Optional.empty[JErrorKind]())(f => Optional.of(f._1))

  /** The error's message, empty on a success. */
  def error(): Optional[String] = failure.fold(Optional.empty[String]())(f => Optional.of(f._2))

  override def toString: String =
    value.fold(s"JResult.failure(${failure.map(_._1).orNull}: ${failure.map(_._2).orNull})")(v =>
      s"JResult.success($v)"
    )

private[triage4s] object JResult:
  def success[T](value: T): JResult[T] = new JResult(Some(value), None)
  def failure[T](kind: JErrorKind, message: String): JResult[T] = new JResult(None, Some(kind -> message))
