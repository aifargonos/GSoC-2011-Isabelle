/*  Title:      src/HTMLExportDockable.scala
    Author:     aifargonos

Dockable window for HTMLExportPlugin.

Contains autorefreshing HtmlPanel and a simple toolbar.
*/


import java.awt.BorderLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.JButton
import javax.swing.JToolBar

import scala.actors.Actor

import org.gjt.sp.jedit.View

import org.lobobrowser.html.gui.HtmlPanel
import org.lobobrowser.html.test.SimpleHtmlRendererContext
import org.lobobrowser.html.test.SimpleUserAgentContext

import isabelle.Session
import isabelle.Swing_Thread
import isabelle.jedit.Isabelle
import isabelle.jedit.Dockable



class HTMLExportDockable(view: View, position: String)
  extends Dockable(view, position)
  with ActionListener
{


  def add(component: scala.swing.Component, constraints: AnyRef)
  {
    add(component.peer, constraints)
  }


  /* content */

  val htmlPanel = new HtmlPanel
  set_content(htmlPanel)
  val rcontext = new SimpleHtmlRendererContext(htmlPanel, new SimpleUserAgentContext())

  // TODO !!! check scala.swing... for all this !!!
  val toolBar = new JToolBar("HTML export")// TODO .: texts
  toolBar.setFloatable(false)

  var button = new JButton("export")// TODO .: texts
  button.setActionCommand("export")// TODO .: magic const
  button.setToolTipText("export")// TODO .: texts
  button.addActionListener(this)
  toolBar.add(button)

  button = new JButton("refresh")// TODO .: texts
  button.setActionCommand("refresh")// TODO .: magic const
  button.setToolTipText("refresh")// TODO .: texts
  button.addActionListener(this)
  toolBar.add(button)

  add(toolBar, BorderLayout.NORTH)


  /* inicialisation */

  refresh()

  override def init() =
  {
    Isabelle.session.commands_changed += main_actor
  }

  override def exit() =
  {
    Isabelle.session.commands_changed -= main_actor
  }


  def refresh() =
  {
    Swing_Thread.later {
      htmlPanel.setHtml(
        // TODO .: set uri according to generated document !!!
        HTMLExportPlugin.convert(view),
        "http://localhost/", rcontext)
    }
  }


  /* event handling */

  private val main_actor = Actor.actor {
    Actor.loop {
      Actor.react {
        case Session.Commands_Changed(changed) => refresh()
        case bad => System.err.println("Output_Dockable: ignoring bad message " + bad)
      }
    }
  }
  // TODO .: use only one of these, of possible
  override def actionPerformed(event: ActionEvent)
  {
    // TODO .: double work :. manage the converted code in more clever way ..
    if(event.getActionCommand == "export") HTMLExportPlugin.export(HTMLExportPlugin.convert(view))
    else if(event.getActionCommand == "refresh") refresh()
  }


}

