package exceptions

case class ThereAreNoPreviousTransformationsException(private val message: String = "",
                                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)
