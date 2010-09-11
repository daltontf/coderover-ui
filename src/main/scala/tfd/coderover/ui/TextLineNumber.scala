package tfd.coderover.ui

import javax.swing.{SwingUtilities, JPanel}
import java.awt._
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import javax.swing.border.{CompoundBorder, EmptyBorder, Border, MatteBorder}
import javax.swing.event.{CaretEvent, DocumentEvent, DocumentListener, CaretListener}
import javax.swing.text._
import collection.mutable.HashMap

/**
 *  This class will display line numbers for a related text component. The text
 *  component must use the same line height for each line. TextLineNumber
 *  supports wrapped lines and will highlight the line number of the current
 *  line in the text component.
 *
 *  This class was designed to be used as a component added to the row header
 *  of a JScrollPane.
 */
object TextLineNumber {
  private final val RIGHT = 1.0f
  private final val LEFT = 0.0f
  private final val HEIGHT = Integer.MAX_VALUE - 1000000
  private final val OUTER: Border = new MatteBorder(0, 0, 0, 2, Color.GRAY)
  private final val CENTER = 0.5f
}

/**
 * 	Create a line number component for a text component.
 *
 * @param component the related text component
 * @param minimumDisplayDigits the number of digits used to calculate
 *                               the minimum width of the component
 */

class TextLineNumber(component: JTextComponent, private var minimumDisplayDigits: Int) extends JPanel with CaretListener with DocumentListener with PropertyChangeListener {
  import TextLineNumber._

  /**
   * 	Create a line number component for a text component. This minimum
   *  display width will be based on 3 digits.
   *
   * @param component the related text component
   */

  def this(component: JTextComponent) {
    this(component, 3)
  }

  private var updateFont = false
  private var lastHeight = 0
  private var lastDigits: Int = 0
  private var currentLineForeground: Color = Color.RED
  private var digitAlignment = .0f
  private var borderGap = 0
  private var fonts: HashMap[String, FontMetrics] = null
  private var lastLine = 0

  setFont(component.getFont)
  setBorderGap(5)
  setCurrentLineForeground(Color.RED)
  setDigitAlignment(RIGHT)
  setMinimumDisplayDigits(minimumDisplayDigits)
  component.getDocument.addDocumentListener(this)
  component.addCaretListener(this)
  component.addPropertyChangeListener("font", this)


  private def getOffsetX(availableWidth: Int, stringWidth: Int): Int =
       ((availableWidth - stringWidth) * digitAlignment).asInstanceOf[Int]

  private def documentChanged {
    SwingUtilities.invokeLater(new Runnable {
      def run: Unit = {
        var preferredHeight: Int = component.getPreferredSize.height
        if (lastHeight != preferredHeight) {
          setPreferredWidth
          repaint()
          lastHeight = preferredHeight
        }
      }
    })
  }

  /**
   *  Calculate the width needed to display the maximum line number
   */
  private def setPreferredWidth: Unit = {
    val root = component.getDocument.getDefaultRootElement
    val lines = root.getElementCount
    val digits = Math.max (String.valueOf (lines).length, minimumDisplayDigits)
    if (lastDigits != digits) {
      lastDigits = digits
      val fontMetrics = getFontMetrics (getFont)
      val width = fontMetrics.charWidth ('0') * digits
      val insets = getInsets
      val preferredWidth = insets.left + insets.right + width
      val d = getPreferredSize
      d.setSize (preferredWidth, HEIGHT)
      setPreferredSize (d)
      setSize (d)
    }
  }

  /**
   *  Gets the current line rendering Color
   *
   * @return the Color used to render the current line number
   */
  def getCurrentLineForeground: Color = if (currentLineForeground == null) getForeground else currentLineForeground

  private def isCurrentLine (rowStartOffset: Int): Boolean = {
    val root = component.getDocument.getDefaultRootElement
    if (root.getElementIndex (rowStartOffset) == root.getElementIndex(component.getCaretPosition) ) {
      true
    } else {
      false
    }
  }

  protected def getTextLineNumber (rowStartOffset: Int): String = {
    val root = component.getDocument.getDefaultRootElement
    val index = root.getElementIndex (rowStartOffset)
    val line = root.getElement (index)
    if (line.getStartOffset == rowStartOffset) {
      String.valueOf (index + 1)
    } else {
      ""
    }
  }

  def insertUpdate (e: DocumentEvent) { documentChanged }

  def propertyChange (evt: PropertyChangeEvent) {
    if (evt.getNewValue.isInstanceOf[Font] ) {
      if (updateFont) {
        val newFont = evt.getNewValue.asInstanceOf[Font]
        setFont (newFont)
        lastDigits = 0
        setPreferredWidth
      } else {
        repaint()
      }
    }
  }

  private def getOffsetY (rowStartOffset: Int, fontMetrics: FontMetrics): Int = {
    val r = component.modelToView (rowStartOffset)
    val lineHeight = fontMetrics.getHeight
    val y = r.y + r.height
    var descent = 0
    if (r.height == lineHeight) {
      descent = fontMetrics.getDescent
    } else {
      if (fonts == null) {
        fonts = new HashMap[String, FontMetrics]
      }
      val root = component.getDocument.getDefaultRootElement
      val index = root.getElementIndex (rowStartOffset)
      val line = root.getElement (index)

      {
        var i: Int = 0
        while (i < line.getElementCount) {
          {
            val child: Element = line.getElement (i)
            val as: AttributeSet = child.getAttributes
            val fontFamily: String = as.getAttribute (StyleConstants.FontFamily).asInstanceOf[String]
            val fontSize = as.getAttribute (StyleConstants.FontSize).asInstanceOf[Int]
            val key: String = fontFamily + fontSize
            var fm = fonts.get(key)
            if (fm == None) {
              val font: Font = new Font (fontFamily, Font.PLAIN, fontSize)
              fm = Some(component.getFontMetrics (font))
              fonts.put (key, fm.get)
            }
            descent = Math.max (descent, fm.get.getDescent)
          }
          ( {
            i += 1;
            i
          })
        }
      }
    }
    y - descent
  }

  /**
   *  Specify the mimimum number of digits used to calculate the preferred
   *  width of the component. Default is 3.
   *
   * @param minimumDisplayDigits the number digits used in the preferred
   *                               width calculation
   */
  def setMinimumDisplayDigits(minimumDisplayDigits: Int) {
    this.minimumDisplayDigits = minimumDisplayDigits
    setPreferredWidth
  }

  def caretUpdate (e: CaretEvent) {
    val currentLine: Int = component.getDocument.getDefaultRootElement.getElementIndex(component.getCaretPosition)
    if (lastLine != currentLine) {
      repaint()
      lastLine = currentLine
    }
  }

  /**
   *  The border gap is used in calculating the left and right insets of the
   *  border. Default value is 5.
   *
   * @param borderGap the gap in pixels
   */
  def setBorderGap (borderGap: Int) {
    this.borderGap = borderGap
    setBorder (new CompoundBorder (OUTER, new EmptyBorder (0, borderGap, 0, borderGap)) )
    lastDigits = 0
    setPreferredWidth
  }
    
  /**
   *  Gets the border gap
   *
   * @return the border gap in pixels
   */
  def getBorderGap = borderGap

  /**
   *  Specify the horizontal alignment of the digits within the component.
   *  Common values would be:
   *  <ul>
   *  <li>TextLineNumber.LEFT
   *  <li>TextLineNumber.CENTER
   *  <li>TextLineNumber.RIGHT (default)
   * 	</ul>
   * @param currentLineForeground the Color used to render the current line
   */
  def setDigitAlignment (digitAlignment: Float) {
    this.digitAlignment = if (digitAlignment > 1.0f) 1.0f else if (digitAlignment < 0.0f) - 1.0f else digitAlignment
  }

  /**
   *  Set the update font property. Indicates whether this Font should be
   *  updated automatically when the Font of the related text component
   *  is changed.
   *
   * @param updateFont when true update the Font and repaint the line
   *                     numbers, otherwise just repaint the line numbers.
   */
  def setUpdateFont(updateFont: Boolean) {
    this.updateFont = updateFont
  }

  /**
   *  Draw the line numbers
   */
  override def paintComponent (g: Graphics): Unit = {
    super.paintComponent (g)
    var fontMetrics: FontMetrics = component.getFontMetrics (component.getFont)
    var insets: Insets = getInsets
    var availableWidth: Int = getSize().width - insets.left - insets.right
    var clip: Rectangle = g.getClipBounds
    var rowStartOffset: Int = component.viewToModel (new Point (0, clip.y) )
    var endOffset: Int = component.viewToModel (new Point (0, clip.y + clip.height) )
    while (rowStartOffset <= endOffset) {
      try {
        if (isCurrentLine (rowStartOffset) ) g.setColor (getCurrentLineForeground)
        else g.setColor (getForeground)
        var lineNumber: String = getTextLineNumber (rowStartOffset)
        var stringWidth: Int = fontMetrics.stringWidth (lineNumber)
        var x: Int = getOffsetX (availableWidth, stringWidth) + insets.left
        var y: Int = getOffsetY (rowStartOffset, fontMetrics)
        g.drawString (lineNumber, x, y)
        rowStartOffset = Utilities.getRowEnd (component, rowStartOffset) + 1
      }
      catch {
        case e: Exception => {
        }
      }
    }
  }

  def removeUpdate (e: DocumentEvent) {
    documentChanged
  }

  /**
   *  The Color used to render the current line digits. Default is Coolor.RED.
   *
   * @param currentLineForeground the Color used to render the current line
   */
  def setCurrentLineForeground (currentLineForeground: Color) {
    this.currentLineForeground = currentLineForeground
  }

  /**
   *  Gets the digit alignment
   *
   * @return the alignment of the painted digits
   */
  def getDigitAlignment = digitAlignment

  /**
   *  Gets the update font property
   *
   * @return the update font property
   */
  def getUpdateFont = updateFont

  /**
   *  Gets the minimum display digits
   *
   * @return the minimum display digits
   */
  def getMinimumDisplayDigits = minimumDisplayDigits

  def changedUpdate (e: DocumentEvent) {
    documentChanged
  }

}

