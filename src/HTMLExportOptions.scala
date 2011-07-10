/*  Title:      .../HTMLExportOptions.props
    Author:     aifargonos

Options for HTMLExportPlugin.

Currently only location of exported file.
*/


import java.awt.BorderLayout
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.JButton
import javax.swing.JPanel
import javax.swing.JTextField
import org.gjt.sp.jedit.AbstractOptionPane
import org.gjt.sp.jedit.GUIUtilities
import org.gjt.sp.jedit.browser.VFSBrowser
import org.gjt.sp.jedit.jEdit



class HTMLExportOptions
  extends AbstractOptionPane("HTMLExport")
  with ActionListener
{


  private val exportToFile = new JTextField(jEdit.getProperty("options.HTMLExport.exporttofile"))


  override def _init()
  {
    val browse = new JButton(jEdit.getProperty("options.HTMLExport.exporttofile.button"));
    browse.addActionListener(this)
    val pathPanel = new JPanel(new BorderLayout(0, 0))
    pathPanel.add(exportToFile, BorderLayout.CENTER)
    pathPanel.add(browse, BorderLayout.EAST)
    addComponent(jEdit.getProperty("options.HTMLExport.exporttofile.title"), pathPanel)
  }

  override def _save()
  {
    jEdit.setProperty("options.HTMLExport.exporttofile", exportToFile.getText)
  }


  override def actionPerformed(e: ActionEvent) =
  {
    val file =
      GUIUtilities.showVFSFileDialog(jEdit.getActiveView(), null, VFSBrowser.SAVE_DIALOG, false)
    if(file != null) exportToFile.setText(file.head)
  }


}

