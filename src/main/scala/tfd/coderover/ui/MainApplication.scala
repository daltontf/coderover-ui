package tfd.coderover.ui

import javax.swing.{AbstractAction, Action, ImageIcon, JButton, JComponent, JComboBox, JPanel, JFileChooser, JFrame, JMenuBar, JMenu, JMenuItem, JOptionPane, JScrollPane, JSplitPane, JTextArea, JToolBar, SwingWorker}
import javax.swing.filechooser.{FileNameExtensionFilter}
import java.awt.{BorderLayout, Color, Dimension, Font, Graphics, Graphics2D}
import java.awt.event.{ActionEvent}
import java.awt.image.{BufferedImage}
import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import edu.umd.cs.piccolo.{PCanvas, PNode}
import edu.umd.cs.piccolo.nodes.{PImage, PText}

import _root_.tfd.coderover.{Environment, Instruction, State, LanguageParser, Evaluator}
import _root_.tfd.gui.swing.CutCopyPastePopupSupport

import Math._

abstract class Task(val title:String, val description:String, val scenarios:Scenario*) {
  
  def isComplete(environment:Environment, state:State):Boolean
  
  def createNewEnvironment():GUIEnvironment
}

object SimpleTask extends Task("Simple Task", "Simple Task", new Scenario("Only Scenario") {
	def createStartState() = new State(2,2,0)
  }) {
  
  def isComplete(environment:Environment, state:State) = state match {
    case State(8,8,_) => true
    case _ => false
  }
  
  def createNewEnvironment() = new GUIEnvironment(10, 10, 50)
}

abstract class Scenario(val description:String) {
  def createStartState():State
} 


class GUIEnvironment(sizeX:Int, sizeY:Int, squareSize:Int) extends BoundedEnvironment(sizeX, sizeY) {
  val canvas = new PCanvas(); 
	
  private val layer = canvas.getLayer() 
  
  def makePaintedImage(width:Int, height:Int, painter:Graphics => Unit) = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    	painter(image.getGraphics)
    	image
  }
  
  val background = new PImage(makePaintedImage(sizeX * squareSize + 1, sizeY * squareSize + 1, { g:Graphics => 
	    	g.setColor(Color.BLACK)
        for (i <- 0 to sizeX) {
       		g.drawLine(i * squareSize, 0, i * squareSize, sizeX * squareSize - 1)
       	}
    	for (i <- 0 to sizeY) {
        	g.drawLine(0, i * squareSize, sizeY * squareSize - 1, i * squareSize)
        }
  }))
  
  layer.addChild(background)
                                
  //canvas.setPanEventHandler(null);
  val robot = new PImage(makePaintedImage(50,50, { g:Graphics => 
        	g.setColor(Color.RED)
          	g.drawLine(25, 10, 10, 40)
            g.drawLine(10, 40, 40, 40)
            g.drawLine(40, 40, 25, 10)
  }))
  background.addChild(robot)
}

class MainApplication {
	private var currentFile:File = _
	
	private var currentTask:Task = SimpleTask
 
	private var currentScenario = currentTask.scenarios(0)
  
	private var currentState:State = currentScenario.createStartState

  	private val environment = new GUIEnvironment(10,10,50)
    
    private val evaluator = new GUIEvaluator(environment.robot, environment.background, environment)
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
    
    private lazy val runAction:AbstractAction = new AbstractAction("Run") {
    	  		  
  			override def actionPerformed(ae:ActionEvent) {
  				runAction.setEnabled(false)
  				stopAction.setEnabled(true)
  				consoleText.setText("")
   				new SwingWorker[Unit,String]() {
   	  					   	  				    
   					override def doInBackground() {
   						val parseResult = LanguageParser.parse(codeText.getText.toUpperCase)
   						publish(parseResult.toString)
   						if (parseResult.successful) {
   							currentState = SimpleTask.scenarios(0).createStartState
   							evaluator.evaluate(parseResult.get, currentState)
   						}   					
   					}
        
   					override def process(strings:java.util.List[String]) {
   						 import  scala.collection.jcl.Conversions._ 
   						 strings.map { string =>
   							consoleText.append(string)
   							consoleText.append("\n")
   						}
   					} 
        
   					override def done() {
   						environment.canvas.repaint()
   						if (currentState.stopped) {
   							consoleText.append(currentState.abend match {
   							  	case Some(abend) => abend.message
   							  	case None => "Stopped by user"
   							})
   						}
   						runAction.setEnabled(true)
   						stopAction.setEnabled(false)
   					}
   	  			}.execute    	  		  
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
    	add(createButtonForAction(runAction, "go.png", "Run Code Rover"))
    	add(createButtonForAction(stopAction, "stop.png", "Stop Running Program"))
        add(new JComboBox())
    }
    
    private val codeCanvasPane = new JSplitPane(
    		JSplitPane.HORIZONTAL_SPLIT,
    		new JScrollPane(codeText),
    		new JScrollPane(environment.canvas)
    )
    codeCanvasPane.setDividerLocation(350)
    codeCanvasPane.setPreferredSize(new Dimension(780, 410))
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

