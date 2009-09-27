package tfd.coderover.ui

import javax.swing.{AbstractAction, ImageIcon, JButton, JComponent, JPanel, JFileChooser, JFrame, JOptionPane, JScrollPane, JSplitPane, JTextArea, JToolBar, SwingWorker}
import javax.swing.filechooser.{FileNameExtensionFilter}
import java.awt.{BorderLayout, Color, Dimension, Graphics, Graphics2D}
import java.awt.event.{ActionEvent}
import java.awt.geom.AffineTransform
import java.awt.image.{BufferedImage}
import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import Math._

import edu.umd.cs.piccolo.{PCanvas, PNode}
import edu.umd.cs.piccolo.nodes.{PImage, PText}

import _root_.tfd.coderover.{State, LanguageParser, Evaluator}

class GUIState(x:Int, y:Int, directionIndex:Int, robot:PNode) extends State(x, y, directionIndex) {
	private val transform = new AffineTransform()
 
	transform.translate(x * 50, y * 50)
	transform.rotate(directionIndex * (java.lang.Math.PI/2), 25, 25)
	robot.setTransform(transform)
 
	private def executeAnimation(duration:Int) {
		robot.animateToTransform(transform, duration ) 
		Thread.sleep(duration)
	}
	
	override def droidMoveForward(distance:Int) = {
	  super.droidMoveForward(distance)
	  transform.translate(0, -50 * distance)
      executeAnimation(500 * distance)
	}
 
	override def droidTurnRight() = {
	  super.droidTurnRight()
	  transform.rotate(java.lang.Math.PI/2, 25, 25)
	  executeAnimation(1000)
    }
  
	override def droidTurnLeft() = {
	  super.droidTurnLeft()
	  transform.rotate(-java.lang.Math.PI/2, 25, 25)
	  executeAnimation(1000)
	}
}

object Rover 

class MainApplication {
	def makePaintedImage(width:Int, height:Int, painter:Graphics => Unit) = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    	painter(image.getGraphics)
    	image
	}
	  
	private var currentFile:File = _
  
	private val canvas = new PCanvas(); 
	
	val layer = canvas.getLayer() 
    val background = new PImage(makePaintedImage(401,401, { g:Graphics => 
	    	g.setColor(Color.BLACK)
       	for (i <- 0 to 8) {
       	  g.drawLine(i * 50, 0, i * 50, 399)
       	  g.drawLine(0, i * 50, 399, i * 50)
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
    val currentState = new GUIState(0, 0, 1 , robot)
    background.addChild(robot)
    
    val codeText = new JTextArea();
    { 	import codeText._
    	setToolTipText("Enter code here")
        //CutCopyPastePopupSupport.enable(codeText)
    }
    
    val consoleText = new JTextArea();
    {   import consoleText._
    	setPreferredSize(new Dimension(700, 120))
    	setEditable(false)
    	setToolTipText("Console for Logo Parser")
    	//CutCopyPastePopupSupport.enable(consoleText)
    }
    
    def icon(s:String) = new ImageIcon(getClass().getClassLoader().getResource(s))
    
    lazy val newAction = new AbstractAction {
					override def actionPerformed(ae:ActionEvent) {
						codeText.setText("")
					}
     }
    
    lazy val openAction = new AbstractAction {
    				override def actionPerformed(ae:ActionEvent) {
    					val chooser = new JFileChooser
    					chooser.setFileFilter(new FileNameExtensionFilter("Code Rover Files", "coderover"))
    					if (chooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
    						val file = chooser.getSelectedFile
    						if (file.exists) {
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
    
    lazy val saveAction = new AbstractAction() {
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
    
    lazy val runAction = new AbstractAction {
    	  		  
  			override def actionPerformed(ae:ActionEvent) {
   				new SwingWorker[LanguageParser.ParseResult[List[Instruction]],Unit]() {
   	  					   	  				    
   					override def doInBackground() = LanguageParser.parse(codeText.getText)
   	  				  	
   					override def done() {    	  				  	
   						val parseResult = get 
   						consoleText.append(parseResult.toString)
   						consoleText.append("\n")
   						if (parseResult.successful) {
   							new Thread() {
   									override def run() {
   										Evaluator.evaluate(parseResult.get, currentState)
   									}
   							}.start
   						}
   					}
   	  			}.execute    	  		  
   	  		}
     }    

	val frame = new JFrame("Code Rover")
 	val contentPane = frame.getContentPane
	val toolBar = new JToolBar
    toolBar.setFloatable(false)
 
    { 
      import toolBar._
      
      def createButtonForAction(action:AbstractAction, iconFile:String, toolTipText:String) = {
    	  val btn = new JButton(action)
    	  	btn.setIcon(icon(iconFile))
      		btn.setToolTipText(toolTipText)
      		btn
      }
		add(createButtonForAction(newAction, "./new.png", "Start new Code Rover program"))
    	add(createButtonForAction(openAction, "./open.png", "Open existing .coderover file"))
    	add(createButtonForAction(saveAction, "./save.png", "Save current Code Rover code to file"))
    	add(createButtonForAction(runAction, "./go.png", "Run Code Rover"))
    }
    
    val codeCanvasPane = new JSplitPane(
    		JSplitPane.HORIZONTAL_SPLIT,
    		new JScrollPane(codeText),
    		new JScrollPane(canvas)
    )
    codeCanvasPane.setDividerLocation(350)
    codeCanvasPane.setPreferredSize(new Dimension(720, 350))
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


object CodeRoverMain {
	def main(args:Array[String]) {
		 new MainApplication

  	}

}
