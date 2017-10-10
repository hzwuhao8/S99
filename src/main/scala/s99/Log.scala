package s99

import com.typesafe.scalalogging.Logger

trait Log {
  val logger: Logger = Logger(this.getClass)
  
  def info(arg: String)(implicit line: sourcecode.Line, name: sourcecode.FullName) = {
    logger.info(s"${name.value}:${line.value} ${arg}")
  }
  
  def debug(arg: String)(implicit line: sourcecode.Line, name: sourcecode.FullName) = {
    logger.debug(s"${name.value}:${line.value} ${arg}")
  }

  def trace(arg: String)(implicit line: sourcecode.Line, name: sourcecode.FullName) = {
    logger.trace(s"${name.value}:${line.value} ${arg}")
  }
}