/*  Title:      .../Template.props
    Author:     aifargonos

XHTML template for HTMLExportPlugin

and simple templating system.
*/


/* TODO .:
 *  make this a class with argument template
 *  those arguments for render can be more generall
 *  getVariables ??
 */

object Template
{

  //TODO .: in separate file ... build whole architecture
//  val template = """<?xml version="1.0" encoding="utf-8" ?>
//<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
//<html xmlns="http://www.w3.org/1999/xhtml">
//<head>
//  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
//  <title>Test of HTMLexport !!! TODO .: change this template !!!</title>
//  <style><!--
///* style file for Isabelle XHTML/XML output */
//
//body { background-color: #FFFFFF; }
//
//.head     { background-color: #FFFFFF; }
//.source   { background-color: #F0F0F0; padding: 10px; }
//
//.external_source { background-color: #F0F0F0; padding: 10px; }
//.external_footer { background-color: #FFFFFF; }
//
//.theories { background-color: #F0F0F0; padding: 10px; }
//.sessions { background-color: #F0F0F0; padding: 10px; }
//
//.name     { font-style: italic; }
//.filename { font-family: fixed; }
//
//
///* basic syntax markup */
//
//.hidden, hidden { font-size: 0.1pt; visibility: hidden; }
//
//.tclass, tclass               { color: red; }
//.tfree, tfree                 { color: #A020F0; }
//.tvar, tvar                   { color: #A020F0; }
//.free, free                   { color: blue; }
//.skolem, skolem               { color: #D2691E; }
//.bound, bound                 { color: green; }
//.var, var                     { color: #00009B; }
//.numeral, numeral             { }
//.literal, literal             { font-weight: bold; }
//.inner_string, inner_string   { color: #D2691E; }
//.inner_comment, inner_comment { color: #8B0000; }
//
//.bold, bold  { font-weight: bold; }
//.loc, loc  { color: #D2691E; }
//
//.keyword, keyword      { font-weight: bold; }
//.operator, operator    { }
//.command, command      { font-weight: bold; }
//.ident, ident          { }
//.string, string        { color: #008B00; }
//.altstring, altstring  { color: #8B8B00; }
//.verbatim, verbatim    { color: #00008B; }
//.comment, comment      { color: #8B0000; }
//.control, control      { background-color: #FF6A6A; }
//.malformed, malformed  { background-color: #FF6A6A; }
//
//.malformed_span, malformed_span { background-color: #FF6A6A; }
//
///* disables special decoration of links inside .source */
//.source a { color: inherit; text-decoration: inherit; }
//
////-->
//  </style>
//</head>
//
//<body>
//
//  <h1>Test of HTMLexport !!! TODO .: change this template !!!</h1>
//  <p>some text...</p>
//
//  <div class="source">
//    <pre>{{ source }}</pre>
//  </div>
//
//</body>
//</html>
//"""
  val template = """<?xml version="1.0" encoding="utf-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <title>Test of HTMLexport !!! TODO .: change this template !!!</title>
  <style><!--
/* style file for Isabelle XHTML/XML output */

body { background-color: #FFFFFF; }

.head     { background-color: #FFFFFF; }
.source   { background-color: #F0F0F0; padding: 10px; }

.external_source { background-color: #F0F0F0; padding: 10px; }
.external_footer { background-color: #FFFFFF; }

.theories { background-color: #F0F0F0; padding: 10px; }
.sessions { background-color: #F0F0F0; padding: 10px; }

.name     { font-style: italic; }
.filename { font-family: fixed; }


/* basic syntax markup */

.hidden, hidden { font-size: 0.1pt; visibility: hidden; }

.tclass, tclass               { color: red; }
.tfree, tfree                 { color: #A020F0; }
.tvar, tvar                   { color: #A020F0; }
.free, free                   { color: blue; }
.skolem, skolem               { color: #D2691E; }
.bound, bound                 { color: green; }
.var, var                     { color: #00009B; }
.numeral, numeral             { }
.literal, literal             { font-weight: bold; }
.inner_string, inner_string   { color: #D2691E; }
.inner_comment, inner_comment { color: #8B0000; }

.bold, bold  { font-weight: bold; }
.loc, loc  { color: #D2691E; }

.keyword, keyword      { font-weight: bold; }
.operator, operator    { }
.command, command      { font-weight: bold; }
.ident, ident          { }
.string, string        { color: #008B00; }
.altstring, altstring  { color: #8B8B00; }
.verbatim, verbatim    { color: #00008B; }
.comment, comment      { color: #8B0000; }
.control, control      { background-color: #FF6A6A; }
.malformed, malformed  { background-color: #FF6A6A; }

.malformed_span, malformed_span { background-color: #FF6A6A; }

/* disables special decoration of links inside .source */
.source a { color: inherit; text-decoration: inherit; }

//-->
  </style>
</head>

<body>

  {{ source }}

</body>
</html>
"""


  private val tem_variable_r = """\{\{\s*(\S+)\s*\}\}""".r


  def render(data: Map[String, String]): String =
  {
    /* TODO .:
     * for all tem_variables in tamplate
     *  replace them with what is bound to their identifiers in data
     */

////    try {// FIXME !!! it throws "IllegalArgumentException: Illegal group reference" this is probably error in JRE !!!
//      tem_variable_r.replaceAllIn(template, m => {
//        val iden = m.group(1)// TODO .: exceptional cases ??
//        data.getOrElse(iden, "")// TODO .: this should probably throw and exception ..
//      })
////    } catch {
////      case e: IllegalArgumentException => throw new Exception("Match has no groups !! POST !!", e)
////    }

    // FIXME .: workaround :.
    val s = tem_variable_r.split(template)
    s(0) + data.getOrElse("source", "") + s(1)

  }

  def apply(data: Map[String, String]): String = {
    render(data)
  }


}

