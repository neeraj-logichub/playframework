package play.api.db.evolutions

import play.api.Environment
import play.api.libs.Collections


@Singleton
class CustomEvolutionReader @Inject()(environment: Environment) extends EvolutionsReader {

  def evolutions(db: String): Seq[Evolution] = {

    val upsMarker = """^#.*!Ups.*$""".r
    val downsMarker = """^#.*!Downs.*$""".r

    val UPS = "UPS"
    val DOWNS = "DOWNS"
    val UNKNOWN = "UNKNOWN"

    val mapUpsAndDowns: PartialFunction[String, String] = {
      case upsMarker() => UPS
      case downsMarker() => DOWNS
      case _ => UNKNOWN
    }

    val isMarker: PartialFunction[String, Boolean] = {
      case upsMarker() => true
      case downsMarker() => true
      case _ => false
    }

    val folder = environment.getFile(Evolutions.directoryName(db))
    val evolutions = Option(folder.listFiles()).toSeq.flatten
      .filter(file => file.getName.indexOf(".sql") > -1)
      .map(file => {
        val fileName = file.getName
        (BigDecimal(fileName.split("\\.sql")(0)), FileUtils.readFileToString(file, "UTF-8"))
      })
      .sortBy(_._1)
      .map {
        case (revision, script) => {
          val parsed = Collections
            .unfoldLeft(("", script.split('\n').toList.map(_.trim))) {
              case (_, Nil) => None
              case (context, lines) => {
                val (some, next) = lines.span(l => !isMarker(l))
                Some(
                  (
                    next.headOption.map(c => (mapUpsAndDowns(c), next.tail)).getOrElse("" -> Nil),
                    context -> some.mkString("\n")
                  )
                )
              }
            }
            .reverse
            .drop(1)
            .groupBy(i => i._1)
            .mapValues { _.map(_._2).mkString("\n").trim }

          Evolution(revision, parsed.getOrElse(UPS, ""), parsed.getOrElse(DOWNS, ""))
        }
      }
    evolutions
  }
}