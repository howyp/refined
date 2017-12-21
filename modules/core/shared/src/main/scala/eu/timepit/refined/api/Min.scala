package eu.timepit.refined.api

trait Min[FTP] {
  def min: FTP
}
object Min {
  def apply[FTP](implicit ev: Min[FTP]): Min[FTP] = ev
  def instance[FTP](m: FTP): Min[FTP] = new Min[FTP] { def min: FTP = m }
}
