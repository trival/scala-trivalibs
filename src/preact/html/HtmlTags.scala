package trivalibs.preact.html

import trivalibs.preact.bindings.*
import trivalibs.preact.component.*
import trivalibs.utils.js.*

import scala.scalajs.js

// DO NOT EDIT - Generated from scala-dom-types
// See codegen/HtmlCodegen.scala

object HtmlTags:
  // Tag builder for void elements (no children)
  private def buildinTag(
      tag: String,
      modifiers: Seq[HtmlModifiers.Modifier]
  ): VNode =
    var jsAttribs = Obj.literal()
    modifiers.foreach:
      case am: AttributeModifier[?, ?] =>
        if am.key == "cls" then
          if am.value != js.undefined && am.value != "" then
            val existing = jsAttribs.selectDynamic("class")
            val newValue =
              if existing != js.undefined then s"$existing ${am.value}"
              else am.value
            jsAttribs.updateDynamic("class")(newValue.asInstanceOf[js.Any])
        else am(jsAttribs)
      case _: ChildModifier =>
        () // Void elements don't have children, ignore
    h(tag, jsAttribs, js.undefined)

  // Tag builder for elements with children
  private def buildinTagWithChildren(
      tag: String,
      modifiers: Seq[HtmlModifiers.Modifier]
  ): VNode =
    var jsAttribs = Obj.literal()
    var childrenArray = Arr[Child]()

    modifiers.foreach:
      case am: AttributeModifier[?, ?] =>
        if am.key == "cls" then
          if am.value != js.undefined && am.value != "" then
            val existing = jsAttribs.selectDynamic("class")
            val newValue =
              if existing != js.undefined then s"$existing ${am.value}"
              else am.value
            jsAttribs.updateDynamic("class")(newValue.asInstanceOf[js.Any])
        else am(jsAttribs)
      case cm: ChildModifier =>
        cm(childrenArray)

    h(tag, jsAttribs, childrenArray)

  // Generated tag functions
  inline def p(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("p", ms)
  inline def hr(ms: HtmlModifiers.Modifier*) = buildinTag("hr", ms)
  inline def pre(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("pre", ms)
  inline def blockQuote(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("blockquote", ms)
  inline def ol(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("ol", ms)
  inline def ul(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("ul", ms)
  inline def li(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("li", ms)
  inline def dl(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("dl", ms)
  inline def dt(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("dt", ms)
  inline def dd(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("dd", ms)
  inline def figure(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("figure", ms)
  inline def figCaption(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("figcaption", ms)
  inline def div(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("div", ms)
  inline def a(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("a", ms)
  inline def em(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("em", ms)
  inline def strong(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("strong", ms)
  inline def small(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("small", ms)
  inline def s(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("s", ms)
  inline def cite(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("cite", ms)
  inline def code(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("code", ms)
  inline def sub(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("sub", ms)
  inline def sup(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("sup", ms)
  inline def i(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("i", ms)
  inline def b(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("b", ms)
  inline def u(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("u", ms)
  inline def span(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("span", ms)
  inline def br(ms: HtmlModifiers.Modifier*) = buildinTag("br", ms)
  inline def wbr(ms: HtmlModifiers.Modifier*) = buildinTag("wbr", ms)
  inline def ins(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("ins", ms)
  inline def del(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("del", ms)
  inline def form(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("form", ms)
  inline def fieldSet(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("fieldset", ms)
  inline def legend(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("legend", ms)
  inline def label(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("label", ms)
  inline def input(ms: HtmlModifiers.Modifier*) = buildinTag("input", ms)
  inline def button(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("button", ms)
  inline def select(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("select", ms)
  inline def dataList(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("datalist", ms)
  inline def optGroup(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("optgroup", ms)
  inline def option(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("option", ms)
  inline def textArea(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("textarea", ms)
  inline def body(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("body", ms)
  inline def header(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("header", ms)
  inline def footer(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("footer", ms)
  inline def h1(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("h1", ms)
  inline def h2(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("h2", ms)
  inline def h3(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("h3", ms)
  inline def h4(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("h4", ms)
  inline def h5(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("h5", ms)
  inline def h6(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("h6", ms)
  inline def table(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("table", ms)
  inline def caption(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("caption", ms)
  inline def colGroup(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("colgroup", ms)
  inline def col(ms: HtmlModifiers.Modifier*) = buildinTag("col", ms)
  inline def tbody(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("tbody", ms)
  inline def thead(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("thead", ms)
  inline def tfoot(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("tfoot", ms)
  inline def tr(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("tr", ms)
  inline def td(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("td", ms)
  inline def th(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("th", ms)
  inline def area(ms: HtmlModifiers.Modifier*) = buildinTag("area", ms)
  inline def audio(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("audio", ms)
  inline def canvas(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("canvas", ms)
  inline def embed(ms: HtmlModifiers.Modifier*) = buildinTag("embed", ms)
  inline def iframe(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("iframe", ms)
  inline def img(ms: HtmlModifiers.Modifier*) = buildinTag("img", ms)
  inline def map(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("map", ms)
  inline def objectTag(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("object", ms)
  inline def param(ms: HtmlModifiers.Modifier*) = buildinTag("param", ms)
  inline def slot(ms: HtmlModifiers.Modifier*) = buildinTag("slot", ms)
  inline def source(ms: HtmlModifiers.Modifier*) = buildinTag("source", ms)
  inline def track(ms: HtmlModifiers.Modifier*) = buildinTag("track", ms)
  inline def video(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("video", ms)
  inline def htmlRoot(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("html", ms)
  inline def head(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("head", ms)
  inline def base(ms: HtmlModifiers.Modifier*) = buildinTag("base", ms)
  inline def link(ms: HtmlModifiers.Modifier*) = buildinTag("link", ms)
  inline def meta(ms: HtmlModifiers.Modifier*) = buildinTag("meta", ms)
  inline def script(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("script", ms)
  inline def noScript(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("noscript", ms)
  inline def title(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("title", ms)
  inline def style(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("style", ms)
  inline def section(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("section", ms)
  inline def nav(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("nav", ms)
  inline def article(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("article", ms)
  inline def aside(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("aside", ms)
  inline def address(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("address", ms)
  inline def main(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("main", ms)
  inline def q(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("q", ms)
  inline def dfn(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("dfn", ms)
  inline def abbr(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("abbr", ms)
  inline def data(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("data", ms)
  inline def time(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("time", ms)
  inline def varTag(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("var", ms)
  inline def samp(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("samp", ms)
  inline def kbd(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("kbd", ms)
  inline def mark(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("mark", ms)
  inline def ruby(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("ruby", ms)
  inline def rt(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("rt", ms)
  inline def rp(ms: HtmlModifiers.Modifier*) = buildinTagWithChildren("rp", ms)
  inline def bdi(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("bdi", ms)
  inline def bdo(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("bdo", ms)
  inline def keyGen(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("keygen", ms)
  inline def output(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("output", ms)
  inline def progress(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("progress", ms)
  inline def meter(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("meter", ms)
  inline def details(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("details", ms)
  inline def summary(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("summary", ms)
  inline def command(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("command", ms)
  inline def menu(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("menu", ms)
  inline def dialog(ms: HtmlModifiers.Modifier*) =
    buildinTagWithChildren("dialog", ms)
