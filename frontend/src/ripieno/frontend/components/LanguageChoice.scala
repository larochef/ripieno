package ripieno.frontend.components

import ripieno.frontend.i18n.{Language, Tr}

import com.raquo.laminar.api.L.*

/** The language the page speaks, which the player chooses. */
class LanguageChoice extends ReactiveElement {
  override lazy val node: HtmlElement = div(
    cls := "select is-small language-choice",
    select(
      aria.label := Tr("Language", "Langue"),
      Language.values.toList.map(language =>
        option(
          value := language.code,
          selected := language == Language.current,
          language.name
        )
      ),
      onChange.mapToValue --> (code =>
        Language.values.find(_.code == code).foreach(Language.choose)
      )
    )
  )
}
