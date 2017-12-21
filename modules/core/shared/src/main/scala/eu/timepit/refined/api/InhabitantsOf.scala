package eu.timepit.refined.api

trait InhabitantsOf[FTP] {
  def all: Stream[FTP]
}
object InhabitantsOf {
  def apply[FTP](implicit ev: InhabitantsOf[FTP]): InhabitantsOf[FTP] = ev
}
trait Min[FTP] {
  def min: FTP
}
object Min {
  def apply[FTP](implicit ev: Min[FTP]): Min[FTP] = ev
}
