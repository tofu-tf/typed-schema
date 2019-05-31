package ru.tinkoff.tschema.swagger

import scalatags.Text.all._

object SwaggerIndex {



  def apply(url: String, webjarsUri: String): String = {
    def cssref(s: String) = link(href := s, rel := "stylesheet")
    def js(s: String)     = script(src := s)
    val version           = BuildInfo.swaggerUIVersion
    def webjar(s: String) = s"$webjarsUri/swagger-ui-dist/$version/$s"
    def index =
      html(
        meta(charset := "UTF-8"),
        tag("title")("Typed Schema Swagger UI"),
        cssref("https://fonts.googleapis.com/css?family=Open+Sans:400,700|Source+Code+Pro:300,600|Titillium+Web:400,600,700"),
        cssref(webjar("swagger-ui.css")),
        tag("style")(indexStyle),
        body(div(id := "swagger-ui"),
             js(webjar("swagger-ui-bundle.js")),
             js(webjar("swagger-ui-standalone-preset.js")),
             script(onload))
      )

    def indexStyle =
      raw("""
            |html{
            |      box-sizing: border-box;
            |      overflow: -moz-scrollbars-vertical;
            |      overflow-y: scroll;
            |    }
            |    *,
            |    *:before,
            |    *:after
            |    {
            |      box-sizing: inherit;
            |    }
            |
            |    body {
            |      margin:0;
            |      background: #fafafa;
            |    }""".stripPrefix("|"))

    def onload =
      raw("""
            |window.onload = function() {
            |
            |  // Build a system
            |  const ui = SwaggerUIBundle({
            |    url: "/swagger",
            |    dom_id: '#swagger-ui',
            |    deepLinking: true,
            |    presets: [
            |      SwaggerUIBundle.presets.apis,
            |      SwaggerUIStandalonePreset
            |    ],
            |    plugins: [
            |      SwaggerUIBundle.plugins.DownloadUrl
            |    ],
            |    layout: "StandaloneLayout"
            |  })
            |
            |  window.ui = ui
            |}
          """.stripMargin)

    index.render
  }
}
