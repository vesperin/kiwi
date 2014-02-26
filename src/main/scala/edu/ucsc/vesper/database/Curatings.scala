package edu.ucsc.vesper.database

import scala.slick.driver.MySQLDriver.simple._
import edu.ucsc.vesper.domain.Curating

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 *
 */
object Curatings extends Table[Curating]("curating") {
  def id          = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def sessionname = column[String]("name")
  def username    = column[String]("username")
  def password    = column[String]("password")
  def gistid      = column[Long]("gistid",  O.Nullable)
  def url         = column[String]("url")
  def birthday    = column[java.util.Date]("birthday", O.NotNull)

  def * = id.? ~ sessionname ~ username ~ password ~ gistid.? ~ url ~ birthday.? <>(Curating, Curating.unapply _)

  implicit val dateTypeMapper = MappedTypeMapper.base[java.util.Date, java.sql.Date](
    {
      ud => new java.sql.Date(ud.getTime)
    },
    {
      sd => new java.util.Date(sd.getTime)
    }
  )

  val findById = for {
    id <- Parameters[Long]
    c <- this if c.id is id
  } yield c

}
