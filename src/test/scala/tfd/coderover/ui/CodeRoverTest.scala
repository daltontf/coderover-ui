package tfd.coderover.ui

import _root_.tfd.coderover.ui.tasks.{TaskManager}
import org.fest.swing.fixture.FrameFixture
import org.fest.swing.launcher.ApplicationLauncher
import junit.framework.TestCase
import org.fest.swing.finder.WindowFinder
import java.awt.{Component, EventQueue}
import reflect.Manifest
import javax.swing.{JTextPane, JComponent, JButton, JFrame}
import org.fest.swing.core.{Robot, BasicRobot, GenericTypeMatcher}

class CodeRoverTest extends TestCase {
  //import tfd.fest.scala.FestImplicits._

  def withToolTip[T <: JComponent](toolTip:String)(implicit m:Manifest[T]) = new GenericTypeMatcher[T](m.erasure.asInstanceOf[Class[T]]) {
     override def isMatching(component:T) = {
       toolTip == component.getToolTipText
     }
  }

  private var window:FrameFixture = _
  private var robot:Robot = _

  override def setUp {
    EventQueue.invokeLater(new Runnable() {
      override def run() {
        new MainApplication()
      }
    })

    robot = BasicRobot.robotWithCurrentAwtHierarchy()

    window = WindowFinder.findFrame(classOf[JFrame]).using(robot) 
    window.target.toFront()
  }

   override def tearDown {
    window.cleanUp
  }


  def testIt() {
    import Thread._

    window.button(withToolTip[JButton]("Run for current scenario")).click()
    sleep(500)
    window.textBox(withToolTip[JTextPane]("Console messages")).requireText("[1.1] parsed: List()\r\n")
    window.textBox(withToolTip[JTextPane]("Enter code here")).setText("FORWARD")
    window.button(withToolTip[JButton]("Run for current scenario")).click()
    window.textBox(withToolTip[JTextPane]("Console messages")).requireText("[1.8] parsed: List(Forward())")
  }
}
