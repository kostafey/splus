package controllers

import javax.inject._
import scala.util.Try
import scala.concurrent.duration._

import play.api._
import play.api.mvc._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import com.github.blemale.scaffeine.{ Cache, Scaffeine }

case class Output (
  val success: Boolean,
  val result: Option[BigDecimal],
  val errors: Seq[String]
)

case class RequestCacheKey(
  val a: Option[String],
  val b: Option[String])

@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents)
    extends BaseController {

  val logger: Logger = Logger(this.getClass())

  val cache: Cache[RequestCacheKey, Output] =
    Scaffeine()
      .recordStats()
      .expireAfterAccess(1.minute)
      .maximumSize(1000)
      .build[RequestCacheKey, Output]()

  def index() = Action { implicit request: Request[AnyContent] =>
      val cacheKey: RequestCacheKey = RequestCacheKey(
          a = param(request, "a"),
          b = param(request, "b"))

      val output: Output = cache.getIfPresent(cacheKey) match {
	        case Some(output) => {
            logger.info(s"${cacheKey.asJson.noSpaces} <- cache")
            output
          }
          case None => {
            logger.info(s"${cacheKey.asJson.noSpaces} <- process")
            var errors: Seq[String] = Vector()

            val a: Option[BigDecimal] = parseParamLong(request, "a") match {
                case Right(value) => Some(value)
                case Left(err) => {
                  errors ++= err
                  None
                }
              }
            val b: Option[BigDecimal] = parseParamLong(request, "b") match {
                case Right(value) => Some(value)
                case Left(err) => {
                  errors ++= err
                  None
                }
              }
            val result = for {
                x <- a
                y <- b
              } yield x*y
            val output: Output = Output(
              success = errors.isEmpty && result.isDefined,
              result = result,
              errors = errors)
            cache.put(cacheKey, output)
            output
          }
      }
      getResponseWithStatus(output)
    }

  def getResponseWithStatus(output: Output): Result = {
    if (!output.errors.isEmpty) {
      BadRequest(output.asJson.spaces2)
    } else if (!output.success) {
      InternalServerError(output.asJson.spaces2)
    } else {
      Ok(output.asJson.spaces2)
    }
  }

  // Either seq of erorr/warning messages OR actual result
  def parseParamLong(request: Request[AnyContent], paramName: String):
      Either[Seq[String], Long] = {
    param(request, paramName) match {
      case Some(value) => toLongOpt(value) match {
        case Some(num) => Right(num)
        case None => Left(Seq(s"Can't parse ${paramName} parameter.") )
      }
      case None => {
        Left(Seq(s"Can't read ${paramName} parameter from request."))
      }
    }
  }

  def param(request: Request[AnyContent], field: String): Option[String] = {
    request.queryString.get(field).flatMap(_.headOption)
  }

  def toLongOpt(data: String): Option[Long] = {
    Try(data.toLong).toOption
  }
}
