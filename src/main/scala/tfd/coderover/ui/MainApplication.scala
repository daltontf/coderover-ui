package tfd.coderover.ui

import javax.swing.{AbstractAction, Action, DefaultComboBoxModel, ImageIcon, JButton, JComponent, JComboBox, JLabel, JPanel, JProgressBar, JFileChooser, JFrame, JMenuBar, JMenu, JMenuItem, JOptionPane, JScrollPane, JSplitPane, JTextArea, JToolBar, Scrollable, SwingWorker}
import javax.swing.filechooser.{FileNameExtensionFilter}
import java.awt.{BorderLayout, Color, Dimension, EventQueue, FlowLayout, GridBagLayout, GridBagConstraints, Font, Graphics, Graphics2D, Rectangle}
import java.awt.event.{ActionEvent, ItemListener, ItemEvent}
import java.awt.image.{BufferedImage}
import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import edu.umd.cs.piccolo.{PCanvas, PNode}
import edu.umd.cs.piccolo.nodes.{PImage, PText}

import _root_.tfd.coderover.{Environment, Instruction, State, LanguageParser, Evaluator}
import _root_.tfd.gui.swing.CutCopyPastePopupSupport

import Math._

abstract class Task(val title:String, val description:String, val scenarios:Scenario*) {
  val taskState = new TaskState
  
  def isComplete(environment:Environment, state:State):Boolean
  
  def createNewEnvironment():GUIEnvironment
}

abstract class Scenario(val description:String) {
  def createStartState():State
  
  override def toString = description
}

class StartStateScenario(state:State, description:String) extends Scenario(description) {
	def createStartState() = State(state.gridX, state.gridY, state.directionIndex)
}

class TaskState {
  val isComplete = false
  val code = ""
}

object SimpleTask extends Task("Simple Task", 
                               "Simple Task", 
                               new StartStateScenario(new State(2,2,1), "Only Scenario")) {
  
  def isComplete(environment:Environment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }
  
  def createNewEnvironment() = new GUIEnvironment(10, 10, Some(8,8))
}

object GotoXYTask extends Task("Goto X,Y",
                               "Goto X,Y",
                               new StartStateScenario(new State(2,2,0), "Start at 2,2"),
                               new StartStateScenario(new State(2,8,0), "Start at 2,8"),
                               new StartStateScenario(new State(8,2,0), "Start at 8,2"),
                               new StartStateScenario(new State(8,8,0), "Start at 8,8")
) {
  def isComplete(environment:Environment, state:State) = state match {
    case State(5,5,_) => true
    case _ => false
  }
  
  def createNewEnvironment() = new GUIEnvironment(10, 10, Some(5,5))
 }
                               
                               

object TaskManager {
  private val task:Array[Task] = Array(GotoXYTask, SimpleTask)
  private var currentTaskIndex = 0
    
  def currentTask = task(currentTaskIndex)
}

class GUIEnvironment(sizeX:Int, sizeY:Int, val targetLocation:Option[(Int,Int)]) extends BoundedEnvironment(sizeX, sizeY) {
  
  def this(sizeX:Int, sizeY:Int) = this(sizeX, sizeY, None)	

}



class GUIViewController(squareSize:Int, environment:GUIEnvironment) {
	private val colorMap = Map(
								0 -> Color.BLACK,
								1 -> Color.GREEN,
								2 -> Color.YELLOW,
								3 -> Color.BLUE,
								4 -> Color.RED,
								5 -> Color.WHITE,
								6 -> Color.CYAN,
								7 -> Color.MAGENTA,
								8 -> Color.ORANGE
							)
  
  val canvas = new PCanvas()  
   
  canvas.setPreferredSize(new Dimension(environment.sizeX * squareSize, environment.sizeY * squareSize)) 
    
  private val layer = canvas.getLayer() 
  
  def makePaintedImage(width:Int, height:Int, painter:Graphics => Unit) = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    	painter(image.getGraphics)
    	image
  }
  
  val background = new PImage(makePaintedImage(environment.sizeX * squareSize + 1, environment.sizeY * squareSize + 1, { g:Graphics => 
	    	g.setColor(Color.BLACK)
	    
        for (i <- 0 to environment.sizeX) {
       		g.drawLine(i * squareSize, 0, i * squareSize, environment.sizeX * squareSize - 1)
       	}
    	for (i <- 0 to environment.sizeY) {
        	g.drawLine(0, i * squareSize, environment.sizeY * squareSize - 1, i * squareSize)
        }
    	if (environment.targetLocation != None) {
    		val coordinates = environment.targetLocation.get
    		g.drawArc(coordinates._1 * squareSize + 1, 
    			      coordinates._2 * squareSize + 1,
    			      squareSize - 2,
    			      squareSize - 2,
    			      0,
    			      360);
      
    	}
  }))
  
  layer.addChild(background)
                                
  canvas.setPanEventHandler(null);
  val robot = new PImage(makePaintedImage(squareSize,squareSize, { g:Graphics => 
        	g.setColor(Color.RED)
        	val oneHalf = squareSize/2;
        	val oneFifth = squareSize/5;
        	val fourFifths = oneFifth * 4
          	g.drawLine(oneHalf, oneFifth, oneFifth, fourFifths)
            g.drawLine(oneFifth, fourFifths, fourFifths, fourFifths)
            g.drawLine(fourFifths, fourFifths, oneHalf, oneFifth)
  }))
  background.addChild(robot)  
  
  def paint(state:State, color:Int) {
    if (colorMap.contains(color)) {
    	val g = background.getImage.getGraphics
		val x = state.gridX * squareSize
		val y = state.gridY * squareSize
		g.setColor(colorMap(color))
		g.fillRect(x + 1, y + 1, squareSize-1, squareSize-1)
    } else {
    	state.fail(new Abend("Invalid Paint Color specified:" + color) { })
    }
  }
}

class MainApplication {
	private var currentFile:File = _
	
	private var currentTask:Task = TaskManager.currentTask
 
	private var currentScenario = currentTask.scenarios(0)
  
	private var currentState:State = currentScenario.createStartState

  	private val environment = currentTask.createNewEnvironment
   
    private val viewController = new GUIViewController(50, environment)
    
    private val evaluator = new GUIEvaluator(environment, viewController)
   	evaluator.syncToState(currentState)
 	
    private val codeFont = new Font("Courier", Font.BOLD, 12)
    
    private val codeText = new JTextArea();
    { 	import codeText._
    	setToolTipText("Enter code here")
        setFont(codeFont)
        CutCopyPastePopupSupport.enable(codeText)
    }
    
    private val consoleText = new JTextArea();
    {   import consoleText._
    	setPreferredSize(new Dimension(700, 120))
    	setEditable(false)
        setLineWrap(true)
    	setToolTipText("Console messages")
        setFont(codeFont)
    	CutCopyPastePopupSupport.enable(consoleText)
    }    
        
    def icon(s:String) = new ImageIcon(getClass().getClassLoader().getResource(s))
    
    private lazy val newAction = new AbstractAction("New") {
					override def actionPerformed(ae:ActionEvent) {
						currentState.stopped = true
						codeText.setText("")
					}
     }
    
    private lazy val openAction = new AbstractAction("Open") {
    				override def actionPerformed(ae:ActionEvent) {
    					val chooser = new JFileChooser
    					chooser.setFileFilter(new FileNameExtensionFilter("Code Rover Files", "coderover"))
    					if (chooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
    						val file = chooser.getSelectedFile
    						if (file.exists) {
    							currentState.stopped = true
    							new SwingWorker[StringBuffer,Unit]() {
    								override def doInBackground():StringBuffer = {
    										val reader = new BufferedReader(new FileReader(file))
    										val buffer = new StringBuffer(file.length.asInstanceOf[Int])
    										while (reader.ready) {
    											buffer.append(reader.readLine)
    											buffer.append("\n")
    										}
    										reader.close
    										buffer
    									}
    							    
    								override def done() {
    									codeText.setText(get().toString)
    								}
    							}.execute
    						}
    					}
    				}
    			}
    
    private lazy val saveAction = new AbstractAction("Save") {
      		import javax.swing.JOptionPane._
            
    		override def actionPerformed(ae:ActionEvent) {
    			val chooser = new JFileChooser
    			chooser.setFileFilter(new FileNameExtensionFilter("Code Rover Files", "coderover"))
    			if (chooser.showSaveDialog(frame) == JFileChooser.APPROVE_OPTION) {
    				val selectedFile = chooser.getSelectedFile
    				val file = if (selectedFile.getName.indexOf(".") >= 0) {
    					chooser.getSelectedFile
    				} else {
    					new File(selectedFile.getAbsolutePath + ".coderover")
    				}
    				if (file.exists() &&
    					JOptionPane.showConfirmDialog(frame, "File Exists", "Existing file will be overwritten", OK_CANCEL_OPTION) != OK_OPTION) {
    					return
    				}
    				new SwingWorker[Unit,Unit]() {
  						override def doInBackground() {
  							val writer = new BufferedWriter(new FileWriter(file))
   	 								val text = codeText.getText
   	 								writer.write(text, 0, text.length)
   	 								writer.close
   	 							}
   	 						}.execute
      			}
    	}
    }
        
    private def startRunOperations() {
    	consoleText.setText("")      
    
    	runAction.setEnabled(false)
    	runTaskAction.setEnabled(false)
  		scenarioCombo.setEnabled(false)
    
  		stopAction.setEnabled(true)
    }
    
    private def postRunOperations() {
    	runAction.setEnabled(true)
    	runTaskAction.setEnabled(true)
  		scenarioCombo.setEnabled(true)

  		stopAction.setEnabled(false)
    }
        
    private lazy val runAction:AbstractAction = new AbstractAction("Run") {
    	  		  
  		override def actionPerformed(ae:ActionEvent) {
  			startRunOperations()
  			new RunWorker().execute    	  		  
   		}
     }
    
    abstract private class BaseWorker extends SwingWorker[Unit,String]() {
    	
    	override def process(strings:java.util.List[String]) {
 			 import  scala.collection.jcl.Conversions._ 
 			 strings.map { string =>
 					consoleText.append(string)
 					consoleText.append("\n")
 			}
 			consoleText.setCaretPosition(consoleText.getDocument().getLength())
 		}
     
    	override def done() {
 			viewController.canvas.repaint()
 			postRunOperations()
   		}
     
    	protected def buildTaskCompletionMessage() = {
    		val sb = new StringBuffer()
    		sb.append(currentScenario)
    		sb.append(" - ")
    		if (currentState.stopped) {
    			sb.append(currentState.abend match {
 			  		case Some(abend) => abend.message
 			  		case None => "Stopped by user"
    			})
    		} else {
    			if (currentTask.isComplete(environment, currentState)) {
    				sb.append("SUCCESS !!")
    			} else {
    				sb.append("FAILED !!")
    				currentState.stopped = true
    			}
    		}
    		sb.toString
    	}      
    }
    
    private class RunTaskWorker extends BaseWorker {
      
 		override def doInBackground() {
 			val parseResult = LanguageParser.parse(codeText.getText.toUpperCase)
 			publish(parseResult.toString)
 			if (parseResult.successful) {
 				var scenarioIndex = 0
 				while (scenarioIndex < currentTask.scenarios.length) {
 					EventQueue.invokeAndWait(new Thread {
 					  override def run() {
 						  scenarioCombo.setSelectedIndex(scenarioIndex)
 					  }
 					})
 					currentState = currentScenario.createStartState
 					evaluator.syncToState(currentState)
 					evaluator.evaluate(parseResult.get, currentState)
 					publish(buildTaskCompletionMessage()) 	
 					scenarioIndex += 1
 					EventQueue.invokeAndWait(new Thread {
 						override def run() {
 						  progressBar.setValue(scenarioIndex)
 						  println(currentState.stopped)
 						  if (currentState.stopped) {
 							  progressBar.setForeground(Color.RED)
 						  }
 					  }
 					})
 					if (currentState.stopped) {
 					  scenarioIndex = currentTask.scenarios.length
 					} 									
 				} 
 			}
 		} 		
    }
    
    private class RunWorker extends BaseWorker {
 		override def doInBackground() {
 			val parseResult = LanguageParser.parse(codeText.getText.toUpperCase)
 			publish(parseResult.toString)
 			if (parseResult.successful) {
 				currentState = currentScenario.createStartState
 				evaluator.syncToState(currentState)
 				evaluator.evaluate(parseResult.get, currentState)
 				publish(buildTaskCompletionMessage())
 			} 
 		} 		
    }
        
    private lazy val runTaskAction:AbstractAction = new AbstractAction("Run Task") {
   	 	override def actionPerformed(ae:ActionEvent) {
   	 		progressBar.setValue(0)
   	 		progressBar.setMinimum(0)
   	 		progressBar.setMaximum(currentTask.scenarios.length)
   	 		progressBar.setForeground(Color.GREEN)
  			startRunOperations()
  			new RunTaskWorker().execute    
  		}
    }
    
    private lazy val stopAction = new AbstractAction("Stop") {
    	setEnabled(false)
      
    	override def actionPerformed(ae:ActionEvent) {
    		currentState.stopped = true
    	}
    }

	private val frame = new JFrame("Code Rover")
 	private val menuBar = new JMenuBar;
    private val menu = new JMenu("File"); { 
        import menu._
        
        setMnemonic('F')
        
        def menuItem(action:Action, mnemonic:Char) = {
          val mi = new JMenuItem(action)
          mi.setMnemonic(mnemonic)
          mi
        }
        
        add(menuItem(newAction, 'N'))
        add(menuItem(openAction, 'O'))
        add(menuItem(saveAction, 'S'))
    }
    menuBar.add(menu)   
    frame.setJMenuBar(menuBar)
 
    private val progressBar = new JProgressBar
    private val taskLabel = new JLabel()
    private val scenarioCombo = new JComboBox()
    private val contentPane = frame.getContentPane
	private val toolBar = new JToolBar; { 
      import toolBar._
      
      setFloatable(false)
      
      def createButtonForAction(action:AbstractAction, iconFile:String, toolTipText:String) = {
    	  val btn = new JButton(action); { import btn._
              setText("")
    		  setIcon(icon(iconFile))
    	  	  setToolTipText(toolTipText)
          }
    	  btn
      }
//		add(createButtonForAction(newAction, "new.png", "Start new Code Rover program"))
//    	add(createButtonForAction(openAction, "open.png", "Open existing .coderover file"))
//    	add(createButtonForAction(saveAction, "save.png", "Save current Code Rover code to file"))
        val panel = new JPanel
        panel.setOpaque(false)
        panel.setLayout(new GridBagLayout())
        val gbc = new GridBagConstraints(); 
        import GridBagConstraints._
        gbc.anchor = NORTHWEST
        gbc.fill = BOTH
        gbc.weightx = 1.0
        panel.add(progressBar, gbc)
        progressBar.setLayout(new BorderLayout())
        progressBar.add(new JLabel("Task : "), BorderLayout.WEST)
        progressBar.add(taskLabel)        
        gbc.fill = VERTICAL
        gbc.weightx = 0.0
        panel.add(createButtonForAction(runTaskAction, "go-go.png", "Run all scenarios"), gbc)
        panel.add(scenarioCombo, gbc)
        add(panel)
    	add(createButtonForAction(runAction, "go.png", "Run for current scenario"))
    	add(createButtonForAction(stopAction, "stop.png", "Stop Running Program"))
        
    }
    
    scenarioCombo.setModel(new DefaultComboBoxModel(currentTask.scenarios.toArray.asInstanceOf[Array[Object]])) 
    scenarioCombo.addItemListener(new ItemListener() {
    	override def itemStateChanged(ie:ItemEvent) {
    		currentScenario = ie.getItem().asInstanceOf[Scenario]
    		currentState = currentScenario.createStartState
   			evaluator.syncToState(currentState)
    	}      
    })
                                     
    private val codeCanvasPane = new JSplitPane(
    		JSplitPane.HORIZONTAL_SPLIT,
    		new JScrollPane(codeText),
    		new JScrollPane(viewController.canvas)
    )
    codeCanvasPane.setDividerLocation(265)
    codeCanvasPane.setPreferredSize(new Dimension(780, 505))
    contentPane.add(toolBar, BorderLayout.NORTH) 
    contentPane.add(new JSplitPane(
    	JSplitPane.VERTICAL_SPLIT,
    		codeCanvasPane,
    		new JScrollPane(consoleText)
    	),BorderLayout.CENTER)
    
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack
    frame.setVisible(true)
}

object Main {
	def main(args:Array[String]) {
		 new MainApplication

  	}
}

