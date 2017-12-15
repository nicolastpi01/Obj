package exceptions

case class NoRollbackException(private val message: String = "",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)


