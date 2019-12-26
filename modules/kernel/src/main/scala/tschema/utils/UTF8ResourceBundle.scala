package tschema.utils
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.util.{Locale, PropertyResourceBundle, ResourceBundle}

object UTF8ResourceBundle {
  private[this] object Control extends ResourceBundle.Control {
    override def newBundle(
      baseName  : String,
      locale: Locale,
      format    : String,
      loader: ClassLoader,
      reload    : Boolean
    ): ResourceBundle = {
      val bundleName = toBundleName(baseName, locale)
      val resourceName = toResourceName(bundleName, "properties")

      def reloadStream = for {
        url <- Option(loader.getResource(resourceName))
        connection <- Option(url.openConnection())
      } yield {
        connection.setUseCaches(false)
        connection.getInputStream
      }

      val stream = if (reload) reloadStream else Option(loader.getResourceAsStream(resourceName))

      stream.map { stream =>
        try {
          new PropertyResourceBundle(new InputStreamReader(stream, StandardCharsets.UTF_8))
        } finally {
          stream.close()
        }
      }.orNull
    }
  }

  def apply(bundleName: String, locale: Locale): ResourceBundle =
    ResourceBundle.getBundle(bundleName, locale, Control)
}
