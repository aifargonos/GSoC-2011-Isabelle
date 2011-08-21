/*  Title:      .../HTMLExportPlugin.props
    Author:     aifargonos

HTML export jEdit plugin for Isabelle/jEdit.

Contains main logic: Conversion of a buffer content into XHTML and export into file.
*/


import java.io.File
import org.gjt.sp.jedit.EditPlugin
import org.gjt.sp.jedit.View
import org.gjt.sp.jedit.jEdit

import isabelle.Command
import isabelle.Markup
import isabelle.Markup_Tree
import isabelle.Pretty
import isabelle.Standard_System
import isabelle.Text
import isabelle.jedit.Isabelle
import isabelle.XML
import isabelle.jedit.Document_View
import scala.collection.mutable.ListBuffer
import subLaTeX.SubLaTeX



object HTMLExportPlugin
{

  /* extracting from XML */

  private val span_markup = Set(Markup.TCLASS, Markup.TFREE, Markup.TVAR, Markup.FREE,
    Markup.SKOLEM, Markup.BOUND, Markup.VAR, /*Markup.NUMERAL, ??*/Markup.LITERAL,
    Markup.INNER_STRING, Markup.INNER_COMMENT, Markup.KEYWORD, Markup.OPERATOR, Markup.COMMAND,
    Markup.IDENT, Markup.STRING, Markup.ALTSTRING, Markup.VERBATIM, Markup.COMMENT, Markup.CONTROL,
    Markup.MALFORMED)

  private val msg_markup = Set(Markup.WRITELN, Markup.WARNING, Markup.ERROR)

  private val Def = new Markup.Property(Markup.DEF)
  private val Ref = new Markup.Property(Markup.REF)
  private val Prop_File = new Markup.Property(Markup.FILE)

  private object Def_Entity
  {
    def unapply(markup: Markup): Option[(String, String, String)] =
      markup match {
        case Markup.Entity(kind, name) =>
          markup match {
            case Markup(_, Def(d)) => Some(kind, name, d)
            case _ => None
          }
        case _ => None
      }
  }

  private object Ref_Entity
  {
    def unapply(markup: Markup): Option[(String, String, String)] =
      markup match {
        case Markup.Entity(kind, name) =>
          markup match {
            case Markup(_, Ref(r)) => Some(kind, name, r)
            case _ => None
          }
        case _ => None
      }
  }

  // this is more specific that the previous one, so it should appear before it in matching
  private object Ref_File_Entity
  {
    def unapply(markup: Markup): Option[(String, String, String, String)] =
      markup match {
        case Ref_Entity(kind, name, ref) =>
          markup match {
            case Markup(_, Prop_File(file)) => Some(kind, name, ref, file)
            case _ => None
          }
        case _ => None
      }
  }

  
  private def xml_info_to_xhtml(body: XML.Body): PartialFunction[Any, XML.Tree] =
  {
    case XML.Elem(Def_Entity(kind, name, d), _) =>
      val title = kind + " \"" + name + "\""
      XML.Elem(Markup("a", List(("id", d), ("title", title))), body)
    case XML.Elem(Ref_File_Entity(kind, name, ref, file), _) =>
      val title = kind + " \"" + name + "\""
      val href = isa_file_to_xhtml_file(file) + "#" + ref
//      XML.Elem(Markup("a", List(("href", href), ("title", title))), body)// TODO ...
      XML.Elem(Markup("a", List(("title", title + ", TODO.: this will be link to: " + href))), body)
    case XML.Elem(Ref_Entity(kind, name, ref), _) =>
      val title = kind + " \"" + name + "\""
      val href = "#" + ref// TODO .: this should actually remember also position
      XML.Elem(Markup("a", List(("href", href), ("title", title))), body)

    case XML.Elem(Markup(name, _), _) if span_markup(name) =>
      XML.Elem(Markup("span", List(("class", name))), body)
    case msg @ XML.Elem(Markup(name, _), _) if msg_markup(name) =>
      val title = Pretty.string_of(List(msg), margin = Isabelle.Int_Property("tooltip-margin"))
      XML.Elem(Markup("span", List(("title", title))), body)
    // TODO .: default cases ???
  }


  /* file structure */

  // TODO ...
  private def isa_file_to_xhtml_file(isa_file: String): String =
  {
    isa_file
  }


  /* convert */

  val text_command_name = Set("header", "chapter", "section", "subsection", "subsubsection", "text",
    /*"text_raw", TODO??*/"sect", "subsect", "subsubsect", "txt"/*, "txt_raw"TODO??*/)

  // TODO .: input should be probably something more stringish ..
  def convert(view: View) =
  {

//    val buff = new ListBuffer[XML.Tree]
    val buff = new StringBuilder

    Document_View(view.getTextArea) match {
      case None => throw new Exception("Failed to retrieve a Document_View")// TODO .: something more standard
      case Some(doc_view) =>

        val model = doc_view.model
        val snapshot = model.snapshot// TODO .: some threading :. as in isabelle_sidekick
        val command_iterator = snapshot.node.command_range()
        val sublatex = new SubLaTeX()

        /*
         * TODO .:
         *  process until text command
         *  if iterator.hasNext process text command
         *  repeat until iterator.isEmpty
         */

        def process_commands(command: Command, command_start: Text.Offset) =
        {

          /*
           *  traverse Markup_Tree as in Markup_Tree.toString with last_offset
           *    if it is empty, return last_offset
           *    otherwise
           *      start tag according to info
           *      write from last_offset upto start of range in info
           *      recursion// TODO !!! there can be something in between !!!
           *      write from the returned value upto end of range in info
           *      end tag
           *      return offset where I ended
           */

          def traverse_list(list: List[Markup_Tree.Branches.Entry], already_written: Text.Offset)
            : (XML.Body, Text.Offset) =
          {
            list match {
              case Nil => (List(XML.Text("")), already_written)// TODO init List !!!
              case (Text.Info(range, info), tree)::tail =>

                val r = range + command_start

                val text_before = XML.Text(
                  model.buffer.getText(already_written, (r.start - already_written))
                )// TODO lock it !!
                val (inner_xml, aw) = traverse_tree(tree, r.start)
                val text_after = XML.Text(
                  model.buffer.getText(aw, (r.stop - aw))
                )// TODO lock it !!
                val (ret, ret_aw) = traverse_list(tail, r.stop)

                val body = text_before :: inner_xml ::: List(text_after)
                val info2xhtml = xml_info_to_xhtml(body)
                // TODO .. can I do it better with these lists ??? no ::: but just :: ??

                if (info2xhtml.isDefinedAt(info)) {
                  val elem = info2xhtml(info)
                  (elem :: ret, ret_aw)
                } else {
                  (body ::: ret, ret_aw)
                }
            }
          }

          def traverse_tree(tree: Markup_Tree, already_written: Text.Offset)
            : (XML.Body, Text.Offset) =
          {
            tree.branches.toList.map(_._2) match {
              case Nil => (List(XML.Text("")), already_written)
              case list =>
                traverse_list(list, already_written)// TODO use fold instead auxiliary function ..?
            }
          }

          val (xml_body, _) = traverse_tree(snapshot.state(command).root_markup, command_start)
          buff ++= XML.string_of_body(xml_body)

        }

        def process_text(command: Command, command_start: Text.Offset) =
        {
          snapshot.state(command).markup.branches.toList.map(_._2) match {
            case
              (Text.Info(_, XML.Elem(Markup(Markup.COMMAND, Markup.Name(name)), _)), _)::
              (Text.Info(source_range, XML.Elem(Markup(Markup.VERBATIM, _), _)), _)::
            _ =>
              val to_parse = name match {
                case name if name == "text" || name == "txt" =>
                  command.source.substring(source_range.start+2, source_range.stop-2)
                case name if name.endsWith("sect") =>
                  "\\" + name + "ion{" +
                    command.source.substring(source_range.start+2, source_range.stop-2) + "}"
                case name =>
                  "\\" + name + "{" +
                    command.source.substring(source_range.start+2, source_range.stop-2) + "}"
              }
              buff ++= sublatex(to_parse).toString// TODO .: CharSequence to TraversableOnce[Char]
            case _ => error("Unexpected format of markup for command with offset " + command_start)
          }
        }

        for ((command, command_start) <- command_iterator if !command.is_ignored) {
//          buff ++= "TODO .: command: " + command + "\n"// TODO DEBUG
          if(text_command_name(command.name)) {
            process_text(command, command_start)
          } else {
            // TODO .: this makes breaks between commands
            // TODO .: do own iteration that will group the commands
            buff ++= "<pre class=\"source\">"
            process_commands(command, command_start)
            buff ++= "</pre>\n"
          }
        }

    }

    Template(Map("source" -> buff.toString))
  }


  /* export */

  def export(html: CharSequence) =
  {
    Standard_System.write_file(new File(jEdit.getProperty("options.HTMLExport.exporttofile")), html)
  }


}


class HTMLExportPlugin extends EditPlugin {

}

