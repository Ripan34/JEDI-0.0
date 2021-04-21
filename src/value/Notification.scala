package value

/**
 * Notification class
 * @param msg
 */
class Notification(val msg: String) extends Value{
  def OK = Notification("OK")
  def DONE = Notification("DONE")
  def UNSPECIFIED = Notification("UNSPECIFIED")
}
//companion
object Notification{
  def apply(msg: String= "") = new Notification(msg)

}
