package tfd.coderover.ui

import javax.swing.JPanel
import java.awt.event._
import java.awt.{Color, Graphics}

class GlassPane extends JPanel with MouseListener with MouseMotionListener with FocusListener with ComponentListener {
  addMouseListener(this)
  addMouseMotionListener(this)
  addFocusListener(this)
  addComponentListener(this)

  def mouseReleased(e: MouseEvent) = {}

  def mousePressed(e: MouseEvent) {}

  def mouseMoved(e: MouseEvent) {}

  def mouseExited(e: MouseEvent) {}

  def mouseEntered(e: MouseEvent) {}

  def mouseDragged(e: MouseEvent) {}

  def mouseClicked(e: MouseEvent) {}

  def focusLost(e: FocusEvent) {
    if (isVisible()) {
      requestFocus()
    }
  }

  def focusGained(e: FocusEvent) {}

  def componentShown(e: ComponentEvent) {}

  def componentResized(e: ComponentEvent) {
    setOpaque(false)
  }

  def componentMoved(e: ComponentEvent) {}

  def componentHidden(e: ComponentEvent) {}

  override def paintComponent(g:Graphics) {
    g.setColor(new Color(1, 1, 1, 0.85f));

    val insets = getInsets()

    g.fillRect(insets.left, insets.top, this.getWidth() - (insets.left + insets.right), this.getHeight() - (insets.top + insets.bottom));
  }

  override def setVisible(visible:Boolean) {
    if (visible) {
      requestFocus()
    }
    super.setVisible(visible);
  }
}