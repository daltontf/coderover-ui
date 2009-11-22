package tfd.coderover.ui

import org.junit._
import org.junit.Assert._

import java.awt.{Component, Container}

import javax.swing._

import java.util.concurrent.{Executors}

import org.netbeans.jemmy.{ComponentChooser, JemmyProperties, QueueTool}
import org.netbeans.jemmy.operators._

class CodeRoverTest {
	lazy val launchExecutor = Executors.newSingleThreadExecutor();

	@Before
	def setUp() {
		JemmyProperties.setCurrentTimeout("ComponentOperator.WaitComponentTimeout", 2000)
	}

//	@After
//	def tearDown() {
//		var break = false
//		while (!break) {
//			try {
//				val frame = JFrameOperator.findJFrame(new JFrameOperator.JFrameFinder())
//				if (frame != null) {
//					new JFrameOperator(frame).close()
//				} else {
//					break
//				}
//			} catch {
//			case _ => break = true
//			}
//		}
//		queueWait(1000)
//	}

	def queueWait(max: Long) {
		val qt = new QueueTool()
		val to = qt.getTimeouts().cloneThis()
		to.setTimeout("QueueTool.WaitQueueEmptyTimeout", 2000)
		qt.setTimeouts(to)
		qt.waitEmpty(10)
	}

	def launchMain() {
//		launchExecutor.execute(new Runnable() {
//
//			def run() {
				  new MainApplication
//			}
//
//		})
	}

	class LoanChooser[A <: java.awt.Component](clazz: Class[A], filter: A => Boolean) extends ComponentChooser {
		override def checkComponent(component:Component) =
			if (component.getClass.isAssignableFrom(clazz)) {
				filter(component.asInstanceOf[A])
			} else {
				false
			}

		override def getDescription() = "Generic Finder using Loan Pattern"
	}

	def findJFrame(filter: JFrame => Boolean) =
		new JFrameOperator(JFrameOperator.findJFrame(new LoanChooser(classOf[JFrame], filter)))

	def findJButton(container:Container, filter: JButton => Boolean) = {
		val button = JButtonOperator.findJButton(container, new LoanChooser(classOf[JButton], filter))
		if (button == null) {
		  fail("cannot find JButton with matching criteria")
		}
        new JButtonOperator(button)
    }

//    def findJButton(container:Container, toolTipText:String) = {
//		val button = JButtonOperator.findJButton(container,
//			new ComponentChooser() {
//				override def checkComponent(component:Component) =
//					if (component.getClass.isAssignableFrom(classOf[JButton])) {
//						component.asInstanceOf[JButton].getToolTipText == toolTipText
//					} else {
//						false
//					}
//
//				override def getDescription() = "JButton Finder using toolTipText"
//			})
//		if (button == null) {
//		  fail("cannot find JButton with tooltip = '" + toolTipText + "'")
//		}
//		new JButtonOperator(button)
//    }

	def findJTextArea(frmOper:JFrameOperator, filter: JTextArea => Boolean) =
		new JTextAreaOperator(JTextAreaOperator.findJTextArea(frmOper.getContentPane, new LoanChooser(classOf[JTextArea], filter)))

	def toolTipTextEquals(toolTipText:String)(component:JComponent) = component.getToolTipText == toolTipText

	implicit def jFrameOperator2Container(frmOper:JFrameOperator) = frmOper.getContentPane

	@Test
	def testMain() = {
		launchMain()
		queueWait(2000)

		val frmOper = findJFrame(frm => frm.getTitle == "Code Rover")

		val List(runButtonOper, runTaskButtonOper, stopButtonOper) =
        List("Run all scenarios",
        	 "Run for current scenario",
             "Stop running program").map(toolTipText => findJButton(frmOper, toolTipTextEquals(toolTipText)))

    val List(codeOper, consoleOper) =
			  List("Enter code here",
				     "Console messages").map(toolTipText => findJTextArea(frmOper, toolTipTextEquals(toolTipText)))

		def runCodeRover(code:String, parserOutput:String) = {
			codeOper.setText("")
			consoleOper.setText("")
			codeOper.setText(code)
			runButtonOper.clickMouse()
			Thread.sleep(1000)
			assertEquals(parserOutput, consoleOper.getText)
		}

		runCodeRover("", "[1.1] parsed: List()\nOnly Scenario - FAILED !!\n")
   }
}
