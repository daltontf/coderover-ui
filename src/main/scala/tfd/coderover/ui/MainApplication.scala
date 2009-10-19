package tfd.coderover.ui

import javax.swing.{AbstractAction, Action, ImageIcon, JButton, JComponent, JPanel, JFileChooser, JFrame, JMenuBar, JMenu, JMenuItem, JOptionPane, JScrollPane, JSplitPane, JTextArea, JToolBar, SwingWorker}
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


object Rover 

class MainApplication {
	def makePaintedImage(width:Int, height:Int, painter:Graphics => Unit) = {
		val image = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
    	painter(image.getGraphics)
    	image
	}
	  
	private var currentFile:File = _
  
	private val canvas = new PCanvas(); 
	
	private val layer = canvas.getLayer() 
    
    private val background = new PImage(makePaintedImage(401,401, { g:Graphics => 
	    	g.setColor(Color.BLACK)
       	for (i <- 0 to 8) {
       	  g.drawLine(i * 50, 0, i * 50, 399)
       	  g.drawLine(0, i * 50, 399, i * 50)
       	}
    }))
    layer.addChild(background)
                                
    //canvas.setPanEventHandler(null);
    private val robot = new PImage(makePaintedImage(50,50, { g:Graphics => 
        	g.setColor(Color.RED)
          	g.drawLine(25, 10, 10, 40)
            g.drawLine(10, 40, 40, 40)
            g.drawLine(40, 40, 25, 10)
        }))
    private val currentState = new State(0, 0, 1)
    background.addChild(robot)
    
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
    	setToolTipText("Console for Logo Parser")
        setFont(codeFont)
    	CutCopyPastePopupSupport.enable(consoleText)
    }    
    
    private val boundedEnvironment = new BoundedEnvironment(7,7)    
    
    private val evaluator = new GUIEvaluator(robot, background, boundedEnvironment)
   	evaluator.syncToState(currentState)
    
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
   							currentState.stopped = false
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
   						canvas.repaint()
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
    	add(createButtonForAction(stopAction, "stop.png", "Stopping Running Program"))
    }
    
    private val codeCanvasPane = new JSplitPane(
    		JSplitPane.HORIZONTAL_SPLIT,
    		new JScrollPane(codeText),
    		new JScrollPane(canvas)
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

